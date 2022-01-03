{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module WikiData where

import GHC.Generics
import Data.Aeson
    ( (.:), withObject, withText, FromJSON(parseJSON), FromJSONKey )
import Data.Hashable
import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL

import           Pipes (Producer)
import qualified Pipes as P
import qualified Pipes.Safe
import qualified Pipes.Prelude as P.P
import qualified Pipes.ByteString as P.BS
import           System.IO
import           StreamJSON

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Codec.Serialise as CBOR
import Text.Read (Read(readPrec))
import Text.ParserCombinators.ReadPrec (get)
import qualified Data.JsonStream.Parser as JS
import CAR.Types (PageName(..), SiteId(..), WikiDataId)
import SimplIR.DataSource.Compression (decompressed)

type WikiDataQidIndex = HM.HashMap PageName WikiDataId

type WikiDataCrossSiteIndex = HM.HashMap WikiDataId (HM.HashMap SiteId PageName)

newtype Lang = Lang T.Text
             deriving (Show, Eq, Ord, Hashable, FromJSON, FromJSONKey, CBOR.Serialise)


data EntityType = Item
                deriving (Show, Generic)
instance CBOR.Serialise EntityType
instance FromJSON EntityType where
    parseJSON = withText "entity type" $ \s ->
      case s of
        "item" -> return Item
        _      -> fail "unknown entity type"

-- | A projection of the wikidata entity representation.
data Entity = Entity { entityType :: EntityType
                     , entityId   :: WikiDataId
                     , entityLabels :: [(Lang, T.Text)]
                     , entitySiteLinks :: [(SiteId, PageName)]
                     }
            deriving (Show, Generic)
instance CBOR.Serialise Entity

instance FromJSON Entity where
    parseJSON = withObject "entity" $ \o ->
        Entity <$> o .: "type"
               <*> o .: "id"
               <*> (o .: "labels" >>= withObject "labels" parseLabels)
               <*> (o .: "sitelinks" >>= withObject "site links" parseSitelinks)
      where
        parseLabels = mapM parseLabel . HM.elems
        parseLabel = withObject "label" $ \o ->
              (,) <$> o .: "language"
                  <*> o .: "value"

        parseSitelinks = mapM parseSitelink . HM.elems
        parseSitelink = withObject "site link" $ \o ->
              (,) <$> o .: "site"
                  <*> o .: "title"




createCrossSiteLookup :: WikiDataCrossSiteIndex -> SiteId -> SiteId -> HM.HashMap PageName PageName
createCrossSiteLookup index fromLang toLang =
    HM.fromList
    [ (fromPage, toPage )
    | entries <- HM.elems index
    , Just fromPage <- pure $ fromLang `HM.lookup` entries
    , Just toPage <- pure $ toLang `HM.lookup` entries
    ]



loadWikiDataCrossSiteIndex :: FilePath -> IO WikiDataCrossSiteIndex
loadWikiDataCrossSiteIndex =
    CBOR.readFileDeserialise



--- WikiData QID

parseWikiDataDump :: (Pipes.Safe.MonadSafe m, MonadFail m)
                  => Handle -> Producer Entity m ()
parseWikiDataDump hdl = do
    leftovers <- parseJsonP parser $ decompressed $ P.BS.fromHandle hdl
    have_leftovers <- P.lift $ P.BS.null leftovers
    when have_leftovers $ fail "parseWikiDataDump: found leftovers"
  where parser = JS.arrayOf (JS.value @Entity)
{-# SPECIALISE parseWikiDataDump :: Handle -> Producer Entity (Pipes.Safe.SafeT IO) () #-}

buildWikiDataQidIndex
    :: (MonadFail m)
    => SiteId
    -> Producer Entity m ()
    -> m WikiDataQidIndex
buildWikiDataQidIndex siteId prod =
    fmap fst $ P.P.fold' (<>) mempty id $
        prod P.>-> P.P.map f
  where
    f :: Entity -> HM.HashMap PageName WikiDataId
    f e
      | null (entitySiteLinks e) = mempty
      | otherwise                = HM.fromList $ [ (p, entityId e)  
                                                 | (s,p) <-  entitySiteLinks e
                                                 , s == siteId
                                                 ]  
