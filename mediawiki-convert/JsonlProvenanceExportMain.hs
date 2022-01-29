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
import CAR.Types.Files
import CAR.ToolVersion
import CAR.CarJSON


helpDescr :: PP.Doc
helpDescr =
    "Export Provenance of CBOR corpus as JSONL" <$$> PP.indent 4 options
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

writeJsonLProvFile ::  FilePath -> Provenance -> IO()
writeJsonLProvFile fname prov = do
    let line :: BSL.ByteString
        line = Aeson.encode $ S prov
    BSL.writeFile fname line
    Prelude.putStrLn  $ "Writing JsonL to "<> fname



main :: IO ()
main = do
    (inputFile, outputFile) <- execParser' 2 (helper <*> opts) (progDescDoc $ Just helpDescr)
    carHeader <- readCarFileType inputFile
    prov <- case (carHeader) of
            Just ParagraphsFile -> do
                (prov, _paragraphs) <- readParagraphsFileWithProvenance inputFile
                return $ prov
            Nothing -> fail $ "CBOR file "<> inputFile<> " has no header."
            _ -> do   -- pages or outlines type
                (prov, _pages) <- readPagesOrOutlinesAsPagesWithProvenance inputFile
                return $ prov
    writeJsonLProvFile outputFile prov        


 