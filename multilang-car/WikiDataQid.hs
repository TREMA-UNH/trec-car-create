{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WikiDataQid where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Codec.Serialise as CBOR
import qualified Data.JsonStream.Parser as JS
import CAR.Types (PageName(..), SiteId(..), WikiDataId)
import WikiData
    ( Entity(entityId, entitySiteLinks), WikiDataQidIndex, buildWikiDataQidIndex )

-- moved to WikiData.hs

-- openWikiDataFile :: FilePath -> IO BSL.ByteString
-- openWikiDataFile file =
--     BSL.getContents file

-- decodeWikiDataDump :: BSL.ByteString -> [Entity] 
-- decodeWikiDataDump =  
--   JS.parseLazyByteString (JS.arrayOf (JS.value @Entity))

-- buildWikiDataQidIndex :: SiteId -> BSL.ByteString -> WikiDataQidIndex
-- buildWikiDataQidIndex siteId =
--     foldMap f . decodeWikiDataDump
--   where
--     f :: Entity -> HM.HashMap PageName WikiDataId
--     f e
--       | null (entitySiteLinks e) = mempty
--       | otherwise                = HM.fromList $ [ (p, entityId e)  
--                                                  | (s,p) <-  entitySiteLinks e
--                                                  , s == siteId
--                                                  ]  

main :: IO ()
main = do
    --BSL.getContents >>= print . eitherDecode @[Entity]
    BSL.getContents >>= pure . (buildWikiDataQidIndex (SiteId "enwiki")) >>= BSL.writeFile "wikidata-qid-index.cbor" . CBOR.serialise
