{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Codec.Serialise as CBOR
import qualified Data.JsonStream.Parser as JS
import Data.Maybe (mapMaybe, isNothing)

import SimplIR.DataSource.Compression.Lazy
import CAR.Types.AST ( PageName(..), SiteId(..), WikiDataId )
import WikiData
    ( Entity(entityId, entitySiteLinks), WikiDataCrossSiteIndex, WikiDataQidIndex, WikiDataItem (EntityItem) )
import Data.Aeson

-- --------------------------------------------------

-- | convert chunks of WikiDataItems by pulling out cross site data
buildWikiDataCrossSiteChunk :: [WikiDataItem] -> WikiDataCrossSiteIndex
buildWikiDataCrossSiteChunk chunk =
    mapMaybe convertWikiDataItem chunk
  where  
    convertWikiDataItem (EntityItem e) 
      | null (entitySiteLinks e) = Nothing
      | otherwise                = Just (entityId e, HM.fromList $ entitySiteLinks e)

    convertWikiDataItem _ = Nothing

-- --------------------------------------------
-- parsing WikiData JSON

-- | general parser for JSONLinesArray. (These are real JSON arrays, where each element is on its own line, separated by commas) 
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
        JS.eitherDecode l

-- | parse WikiData JSON dump as JSONLinesArray
parseWikiDataJSONDump :: BSL.ByteString -> [WikiDataItem]
parseWikiDataJSONDump = parseJSONLinesArray


-- --------------------------------------------
-- tie everything together

lazyChunksOf :: Int -> [a] -> [[a]]
lazyChunksOf n [] = []
lazyChunksOf n xs =
    let (hd, tl) = splitAt n xs
    in hd : lazyChunksOf n tl


buildWikiDataCrossSiteIndex
    :: BSL.ByteString
    -> WikiDataCrossSiteIndex
buildWikiDataCrossSiteIndex bs =
    concat
    $ withStrategy strat
    $ map buildWikiDataCrossSiteChunk
    $ lazyChunksOf 16 (parseWikiDataJSONDump bs)
  where
    strat = parBuffer 256 rseq



main :: IO ()
main = do
    BSL.getContents >>= pure . buildWikiDataCrossSiteIndex >>= BSL.writeFile "wikidata-cross-site-index.cbor" . CBOR.serialise
