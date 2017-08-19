{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}

import Debug.Trace

import Data.List (sortBy)
import Data.Maybe
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Data.Ord
import Options.Applicative
import System.IO
import Numeric
import GHC.TypeLits
import Data.Aeson
import Numeric.Log
import Data.Hashable

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.SmallUtf8 as Utf8

import CAR.Types
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import CAR.Utils
import qualified CAR.RunFile as CarRun

import qualified SimplIR.SimpleIndex as Index
import qualified SimplIR.SimpleIndex.Models.QueryLikelihood as QL
import qualified SimplIR.SimpleIndex.Models.BM25 as BM25
-- import ZScore

import qualified SimplIR.Format.TrecRunFile as TrecRun
import GHC.Generics

type EntityIndex = Index.OnDiskIndex Term PageId Int


data QuerySource = QueriesFromCbor FilePath QueryDerivation
                 | QueriesFromJson FilePath

data QueryDerivation = QueryFromPageTitle | QueryFromSectionPaths


data Opts = Opts { output :: FilePath
                 , query :: QuerySource
                 , entityIndexFile :: Index.OnDiskIndex Term PageId Int
                 , selectedQueries :: [TrecRun.QueryId]
                 , methodName :: T.Text
                 , topK :: Int
                 }

data QueryDoc = QueryDoc { queryDocQueryId      :: !TrecRun.QueryId
                         , queryDocPageId       :: !PageId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic
instance FromJSON QueryDocList
instance ToJSON QueryDocList


opts :: Parser Opts
opts = do
    output <- option str (short 'o' <> long "output" <> metavar "OUTFILE" <> help "Output file")
    query <-  querySource
    entityIndexFile <-  option (Index.OnDiskIndex <$> str)
               (short 'i' <> long "index" <> metavar "INDEX" <> help "simplir edgedoc index")
    selectedQueries <- many (option (T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    methodName <- T.pack <$> option str (short 'n' <> value "" <> long "methodname" <> metavar "NAME" <> help "name of method for trec run file" )
    topK <- option auto (short 'k' <> value 100 <> long "topK" <> help "number of ranking entries per query" <> metavar "K" )
    return Opts {..}
    where
        
      querySource :: Parser QuerySource
      querySource =
              fromCbor
          <|> fromJson
        where
          fromCbor =
                QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> flag QueryFromPageTitle QueryFromSectionPaths
                       (long "query-from-sections" <> help "Use sections as query documents")
          
          fromJson =
              QueriesFromJson
                <$> option str (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")


bm25 :: (Hashable term, Eq term) => Index.RetrievalModel term doc Int
bm25 = BM25.bm25 $ BM25.sensibleParams

ql :: (Hashable term, Eq term) => Index.RetrievalModel term doc Int
ql = QL.queryLikelihood (QL.Dirichlet 100)


retrieveEntities :: EntityIndex ->  Index.RetrievalModel Term PageId Int -> IO ([Term] -> [(Log Double, PageId)])
retrieveEntities entityIndexFile model = do
    entityIndex <- Index.open entityIndexFile
    return $ sortBy Index.descending . Index.score entityIndex model
 
pagesToQueryDocs :: QueryDerivation -> [Page] ->  [QueryDoc]
pagesToQueryDocs deriv pages  =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = T.pack $ unpackPageId $ pageId page
                     , queryDocPageId       = pageId page
                     , queryDocQueryText    = getPageName $ pageName page
                     }
          | page <- pages
         ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = T.pack $ escapeSectionPath sectionPath
                     , queryDocPageId       = pageId page
                     , queryDocQueryText    = getPageName (pageName page) <> getPageName (pageName page) -- twice factor
                                              <> T.unwords (map getSectionHeading headings)
                     }
          | page <- pages
          , (sectionPath, headings, _) <- pageSections page
          ]

queryDocRawTerms :: QueryDoc -> [Term]
queryDocRawTerms = textToTokens' . queryDocQueryText

main :: IO ()
main = do
    Opts{..} <- execParser (info (helper <*> opts) mempty)

    retrieve <- retrieveEntities entityIndexFile bm25

    queries <-
        case query of
          QueriesFromCbor queryFile queryDeriv -> do
              pagesToQueryDocs queryDeriv
                  <$> readCborList @Page queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    let retrieveQuery :: QueryDoc -> [TrecRun.RankingEntry]
        retrieveQuery q@QueryDoc{..} =
            trace (show q) $ fmap toTrecEntry
                $ zip [1.. ]
                $ take topK
                $ retrieve (textToTokens' queryDocQueryText)
          where toTrecEntry::  (Int, (Log Double, PageId)) -> TrecRun.RankingEntry
                toTrecEntry (rank, (score, doc)) =
                    TrecRun.RankingEntry { queryId  = queryDocQueryText
                                 , documentName  = T.pack $ unpackPageId doc
                                 , documentRank  = rank
                                 , documentScore = ln score
                                 , methodName    = methodName
                                 }


    let trecRanking = foldMap retrieveQuery queries
    TrecRun.writeRunFile output trecRanking