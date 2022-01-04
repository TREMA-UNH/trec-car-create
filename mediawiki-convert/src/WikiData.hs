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


-- | converting page names from one SiteId to another (e.g. Italian to Japanese)
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

-- | Build WikiData QID index based on a cross-site CBOR. 
--   The cross-site CBOR can be built with `WikiDataCrossSite.hs`, expected to load CBOR as lazy ByteString.
buildWikiDataQidIndex :: SiteId -> WikiDataCrossSiteIndex -> WikiDataQidIndex
buildWikiDataQidIndex siteId crossSiteIndex =
  HM.fromList 
     $ [ (pagename, qid) 
       | (qid, sitemap) <- crossSiteIndex
       , Just pagename <- pure $ siteId `HM.lookup` sitemap
       ]
