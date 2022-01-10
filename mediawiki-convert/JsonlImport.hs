{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

import Data.Monoid hiding (All, Any)
import Development.GitRev
import Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip as GZip
import Codec.Compression.Zlib.Internal (DecompressError)

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson


import SimplIR.DataSource.Compression.Lazy
import CAR.Types
import CAR.ToolVersion
import CAR.CarJSON



helpDescr :: PP.Doc
helpDescr =
    "Import CAR JSONL into CBOR" <$$> PP.indent 4 options
  where
    cmd a b = PP.nest 8 (a <$$> b)
    options = PP.vsep
      [
        -- cmd "need doc"                           "here"
      ]

data ConfOpts = ConfOpts { 
                         }

opts :: Options.Applicative.Parser (FilePath, FilePath, Provenance)
opts =
    (,,)
    <$> argument str (help "input pages file as `.jsonl.gz` must follow CAR datamodel." <> metavar "JSON.GZ")
    <*> option str (short 'o' <> long "output" <> metavar "CBOR" <> help "CBOR Output file")
    <*> provParser


--  -------------- file writing and command line handling -------------



-- writeGzJsonLRunFile ::  FilePath -> [Page] -> IO()
-- writeGzJsonLRunFile fname pages = do
--     let lines :: [BSL.ByteString]
--         lines = fmap (Aeson.encode . S) $ pages
--     BSL.writeFile fname 
--         $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
--         $ BSL.unlines $ lines
--     Prelude.putStrLn  $ "Writing JsonL.gz to "<> fname



commit :: T.Text
commit = $(gitHash)

provParser :: Options.Applicative.Parser (Provenance)
provParser =
    prov
  where
    text = T.pack <$> str
    prov = do
        sourceName <- option str (short 'D' <> long "dump-date" <> metavar "DATE" <> help "Wikipedia dump date" <> value "unknown")
        dataReleaseName <- option str (short 'N' <> long "release-name" <> metavar "NAME" <> help "Data release name" <> value "unknown")
        provSiteId <- option str (short 'S' <> long "site-id" <> metavar "SITE" <> help "Wikipedia site id (e.g. 'enwiki')" <> value "enwiki")
        comments <- many $ option str (short 'C' <> long "comments" <> metavar "NAME" <> help "Other comments about data release")
        language <- option (Language <$> text) (short 'L' <> long "language" <> metavar "LANG" <> help "The IETF language code of the text collection" <> value "en-US")
        pure $ 
            Provenance { siteProvenances = [ SiteProvenance {siteComments = [], ..} ]
                       , dataReleaseName = dataReleaseName
                       , comments = comments
                       , transforms = [transform "json-import" commit ()] -- TODO
                       }


    
parseJsonL :: forall a. Aeson.FromJSON a => BSL.ByteString -> [a]
parseJsonL = zipWith f [1..] . BSL.lines
  where
    f :: Int -> BSL.ByteString -> a
    f !lineNo bs = 
      case Aeson.eitherDecode bs of
        Left err -> error "parseJsonL"
        Right x -> x



main :: IO ()
main = do
    (inputFile, outputFile, prov) <- execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)

    sPages <- parseJsonL . decompress <$> BSL.readFile inputFile
              :: IO [S Page]
    let pages = unwrapS sPages
    
    writeCarFile  outputFile prov pages


