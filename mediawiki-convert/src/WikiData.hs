{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module WikiData where

import Control.Applicative
import GHC.Generics
import Data.Maybe (isNothing)
import qualified Data.Aeson as Aeson
import Data.Aeson
    ( (.:), withObject, withText, FromJSON(parseJSON), FromJSONKey )
import Data.Hashable
import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Parallel.Strategies

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Codec.Serialise as CBOR
import Text.Read (Read(readPrec))
import Text.ParserCombinators.ReadPrec (get)
import qualified Data.JsonStream.Parser as JS
import CAR.Types (PageName(..), SiteId(..), WikiDataId)
import SimplIR.DataSource.Compression.Lazy (decompress)

type WikiDataQidIndex = HM.HashMap PageName WikiDataId

type WikiDataCrossSiteIndex = [(WikiDataId, HM.HashMap SiteId PageName)]

newtype Lang = Lang T.Text
             deriving (Show, Eq, Ord, Hashable, FromJSON, FromJSONKey, CBOR.Serialise)

data WikiDataItem = EntityItem Entity | PropertyItem

instance FromJSON WikiDataItem where
    parseJSON = withObject "wikidata item" $ \o -> do
        ty <- o .: "type"
        case ty :: T.Text of
          "property" -> return PropertyItem
          "item" -> do
            entity <- Entity
                <$> o .: "id"
              --  <*> (o .: "labels" >>= withObject "labels" parseLabels)
                <*> (o .: "sitelinks" >>= withObject "site links" parseSitelinks)
            return $ EntityItem entity
          _ -> fail $ "unknown wikidata item type: " ++ show ty
      where
        -- parseLabels = mapM parseLabel . HM.elems
        -- parseLabel = withObject "label" $ \o ->
        --       (,) <$> o .: "language"
        --           <*> o .: "value"

        parseSitelinks = mapM parseSitelink . HM.elems
        parseSitelink = withObject "site link" $ \o ->
              (,) <$> o .: "site"
                  <*> o .: "title"

-- | A projection of the wikidata entity representation.
data Entity = Entity { entityId        :: !WikiDataId
                   --  , entityLabels    :: [(Lang, T.Text)]
                     , entitySiteLinks :: [(SiteId, PageName)]
                     }
            deriving (Show, Generic)

instance CBOR.Serialise Entity

createCrossSiteLookup :: WikiDataCrossSiteIndex -> SiteId -> SiteId -> HM.HashMap PageName PageName
createCrossSiteLookup index fromLang toLang =
    HM.fromList
    [ (fromPage, toPage )
    | (qid, siteMap) <- index
    , Just fromPage <- pure $ fromLang `HM.lookup` siteMap
    , Just toPage <- pure $ toLang `HM.lookup` siteMap
    ]

loadWikiDataCrossSiteIndex :: FilePath -> IO WikiDataCrossSiteIndex
loadWikiDataCrossSiteIndex =
    CBOR.readFileDeserialise

--- WikiData QID

parseJSONLinesArray :: forall a. FromJSON a => BSL.ByteString -> [a]
parseJSONLinesArray bsl
  | start /= "[" = error "parseWikiDataDump"
  | otherwise =
      zipWith parseOne [1..] $ takeWhile (isNothing . BSL.stripPrefix "]") lines
  where
    start:lines = BSL.split '\n' $ decompress bsl

    parseOne :: Int -> BSL.ByteString -> a
    parseOne lineNo l = 
        case parseOne' l of
          Left err -> error $ "parseJSONLinesArray: parse error on line " ++ show lineNo ++ ":\n" ++ err
          Right x -> x

    parseOne' :: BSL.ByteString -> Either String a
    parseOne' l = do
        l <- pure $ maybe l id $ "," `BSL.stripSuffix` l
        Aeson.eitherDecode l

parseWikiDataDump :: BSL.ByteString -> [WikiDataItem]
parseWikiDataDump = parseJSONLinesArray

chunksOf n [] = []
chunksOf n xs =
    let (hd, tl) = splitAt n xs
    in hd : chunksOf n tl

buildWikiDataQidIndex
    :: SiteId
    -> BSL.ByteString
    -> WikiDataQidIndex
buildWikiDataQidIndex siteId bs =
    HM.unions
    $ withStrategy strat
    $ map (buildWikiDataQidIndex' siteId)
    $ chunksOf 16 (parseWikiDataDump bs)
  where
    strat = parBuffer 256 rseq

buildWikiDataQidIndex'
    :: SiteId
    -> [WikiDataItem]
    -> WikiDataQidIndex
buildWikiDataQidIndex' siteId entities =
    HM.fromList $ foldMap f entities
  where
    f :: WikiDataItem -> [(PageName,WikiDataId)]
    f item = do
        EntityItem e <- pure item
        guard $ not $ null (entitySiteLinks e)
        (s, p) <- entitySiteLinks e
        guard $ s == siteId
        return (p, entityId e)
