{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid hiding (All, Any)
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

import CAR.Types
import CAR.ToolVersion
import CAR.CarJSON


helpDescr :: PP.Doc
helpDescr =
    "Export CBOR corpus of pages as JSONL" <$$> PP.indent 4 options
  where
    cmd a b = PP.nest 8 (a <$$> b)
    options = PP.vsep
      [
        -- cmd "need doc"                           "here"
      ]

data ConfOpts = ConfOpts { 
                         }

opts :: Options.Applicative.Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE.gz" <> help "Output file, will be written as gzip encoded JSONL")


--  -------------- file writing and command line handling -------------

writeGzJsonLRunFile ::  FilePath -> [Page] -> IO()
writeGzJsonLRunFile fname pages = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . S) $ pages
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines
    Prelude.putStrLn  $ "Writing JsonL.gz to "<> fname




main :: IO ()
main = do
    (inputFile, outputFile) <- execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    (prov, pages) <- readPagesFileWithProvenance inputFile
    writeGzJsonLRunFile outputFile pages


