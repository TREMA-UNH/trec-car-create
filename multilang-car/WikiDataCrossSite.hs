{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Codec.Serialise as CBOR
import qualified Data.JsonStream.Parser as JS
import Data.Maybe (mapMaybe)
import CAR.Types.AST ( PageName(..), SiteId(..), WikiDataId )
import WikiData
    ( Entity(entityId, entitySiteLinks), WikiDataCrossSiteIndex, WikiDataItem (EntityItem) )

buildWikiDataCrossSiteIndex :: BSL.ByteString -> WikiDataCrossSiteIndex
buildWikiDataCrossSiteIndex =
    mapMaybe ff . JS.parseLazyByteString (JS.arrayOf (JS.value @WikiDataItem))
  where
    ff :: WikiDataItem -> Maybe (WikiDataId, HM.HashMap SiteId PageName)
    ff (EntityItem e) = f e
    ff _ = Nothing

    f :: Entity -> Maybe (WikiDataId, HM.HashMap SiteId PageName)
    f e
      | null (entitySiteLinks e) = Nothing
      | otherwise                = Just (entityId e, HM.fromList $ entitySiteLinks e)

main :: IO ()
main = do
    --BSL.getContents >>= print . eitherDecode @[Entity]
    BSL.getContents >>= pure . buildWikiDataCrossSiteIndex >>= BSL.writeFile "wikidata-cross-site-index.cbor" . CBOR.serialise
