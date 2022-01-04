{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Codec.Serialise as CBOR
import qualified Data.JsonStream.Parser as JS
import Data.Maybe (mapMaybe, isNothing)
import Options.Applicative

import SimplIR.DataSource.Compression.Lazy
import Data.ByteString.Lazy.Progress as PBSL
import CAR.ToolVersion
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


data Opts = Opts { wikiDataJSONFile :: FilePath
                 , crossSiteFile :: FilePath
                 }

opts :: Parser Opts 
opts = do
  wikiDataJSONFile <- option str (short 'i' <> long "wiki-data-json" <> metavar "INFILE" <> help "Wikidata ``all'' json dump as available online. Can be uncompressed or compressed with bz2, xz, gz.")
  crossSiteFile <- option str (short 'o' <> long "cross-site-file-cbor" <> metavar "OUTFILE" <> help "Output file for cross-site index (in CBOR)")
  return Opts {..}

main :: IO ()
main = do
    Opts{..} <- execParser' 1 (helper <*> opts ) $ progDescDoc (Just "Convert WikiData JSON dump to cross-site.cbor ")

    PBSL.readFile wikiDataJSONFile 
      >>= pure . decompress
      >>= pure . buildWikiDataCrossSiteIndex 
      >>= BSL.writeFile crossSiteFile . CBOR.serialise
