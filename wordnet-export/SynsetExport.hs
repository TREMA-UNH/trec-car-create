{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Data.Hashable
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HM.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (chunksOf)
import System.FilePath
import System.IO.Unsafe

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Options.Applicative

import CAR.Types
import CAR.Utils
import qualified NLP.WordNet as WordNet
import qualified POS
import qualified UKB

main :: IO ()
main = do
    let opts = (,,,)
            <$> option str (short 'c' <> long "connect" <> help "PostgreSQL connection string")
            <*> option str (short 'D' <> long "ukb-dict" <> metavar "DICT" <> help "ukb dictionary file")
            <*> option str (short 'K' <> long "ukb-kb" <> metavar "KB" <> help "ukb knowledge base file")
            <*> option str (short 'W' <> long "wordnet-dict" <> metavar "DIR" <> help "WordNet dictionary directory")
    (connStr, ukbDict, ukbKb, wnPath) <- execParser $ info (helper <*> opts) mempty
    let openConn = connectPostgreSQL (BS.pack connStr)
    toPostgres openConn wnPath ukbDict ukbKb

createTables :: [Query]
createTables =
    [ [sql| CREATE UNLOGGED TABLE IF NOT EXISTS synsets
               ( id integer PRIMARY KEY
               , dict_offset integer NOT NULL
               , pos char NOT NULL
               , words text[] NOT NULL
               )
      |]

    , [sql| CREATE UNLOGGED TABLE IF NOT EXISTS synset_mentions
               ( synset_id integer REFERENCES synsets (id)
               , paragraph_id text REFERENCES paragraphs (paragraph_id)
               )
      |]
    ]

toPostgres :: IO Connection -> FilePath -> FilePath -> FilePath -> IO ()
toPostgres openConn dictPath ukbDict ukbKb = do
    conn <- openConn
    mapM_ (execute_ conn) createTables
    mapM_ (exportSynsets openConn . (dictPath </>)) [ "data.verb", "data.noun", "data.adv", "data.adj" ]

exportSynsets :: IO Connection -> FilePath -> IO ()
exportSynsets openConn dbFile = do
    conn <- openConn
    synsets <- WordNet.iterSynsets dbFile
    void $ executeMany
        conn
        [sql| INSERT INTO synsets (dict_offset, pos, words)
              SELECT x.column1, x.column2, x.column3
              FROM (VALUES (?,?,?)) AS x |]
        (map synsetToRow synsets)
  where
    synsetToRow :: WordNet.Synset -> (Int, String, PGArray T.Text)
    synsetToRow WordNet.Synset{ssOffset=WordNet.Offset off, ..} =
        (off, [toPosChar ssPos], PGArray $ map (TE.decodeUtf8 . WordNet.ssWord) ssWords)

    toPosChar WordNet.Verb = 'v'
    toPosChar WordNet.Noun = 'n'
    toPosChar WordNet.Adj  = 'j'
    toPosChar WordNet.Adv  = 'r'

exportMentions :: FilePath -> FilePath -> IO Connection -> [Page] -> IO ()
exportMentions ukbDict ukbKb openConn pages = do
    conn <- openConn
    let paragraphs :: [(SectionPath, Paragraph)]
        paragraphs = foldMap pageParasWithPaths pages
    tagger <- POS.startTagger
    ukb <- UKB.startUKB ukbDict ukbKb
    print $ map (\(sp, para) -> (sp, paragraphMentions tagger ukb para)) paragraphs
    return ()

paragraphMentions :: POS.Tagger -> UKB.UKB -> Paragraph -> [UKB.ConceptId]
paragraphMentions tagger ukb para = unsafePerformIO $ do
    tags <- POS.posTag tagger $ TL.toStrict (paraToText para)
    let tokens =
            [ UKB.InputToken (UKB.Lemma tok) pos' (UKB.WordId n)
            | (n, (tok, pos)) <- zip [0..] tags
            , Just pos' <- pure $ toUkbPos pos
            ]
    res <- UKB.run ukb tokens
    return [ concept | UKB.OutputToken _ concept <- res ]
  where
    toUkbPos :: POS.POS -> Maybe UKB.POS
    toUkbPos pos =
        case pos of
          POS.Noun      -> Just UKB.Noun
          POS.Verb      -> Just UKB.Verb
          POS.Adjective -> Just UKB.Adj
          POS.Adverb    -> Just UKB.Adv
          _             -> Nothing
