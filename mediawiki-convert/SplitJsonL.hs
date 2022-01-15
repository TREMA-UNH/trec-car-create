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
import Control.Monad
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
-- import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip as GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import Data.List.Split as S

import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

import SimplIR.DataSource.Compression.Lazy
import CAR.ToolVersion

helpDescr :: PP.Doc
helpDescr =
    "Split JSONL into multiple archives of `n` entries " <$$> PP.indent 4 options
  where
    cmd a b = PP.nest 8 (a <$$> b)
    options = PP.vsep
      [
        -- cmd "need doc"                           "here"
      ]

data ConfOpts = ConfOpts { 
                         }

opts :: Options.Applicative.Parser (FilePath, FilePath, Int)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "JSONL.gz")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file template, will substitute chunk number `{n}` for filename ex: \"input-{n}.jsonl.gz\" . Will be written as gzip encoded JSONL")
    <*> option auto (short 'n' <> long "num" <> metavar "N" <> help "Number of objects in each splitted file.")


--  -------------- file writing and command line handling -------------

writeGzJsonLFile ::  FilePath -> [BSL.ByteString] -> IO()
writeGzJsonLFile fname lines = do
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines
    Prelude.putStrLn  $ "Writing JsonL.gz to "<> fname

main :: IO ()
main = do
    (inputFile, outputFileTemplate, numLines) <- execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    lines <- BSL.split '\n' . decompress <$> BSL.readFile inputFile
          :: IO [BSL.ByteString] 
    let chunks = S.chunksOf numLines lines
    forM_ (zip [1..] chunks) (\(i, chunk) -> 
<<<<<<< Updated upstream
            let outputFile = (filePattern outputFileTemplate i)
=======
            let outputFile = filePattern outputFileTemplate i
>>>>>>> Stashed changes
            in writeGzJsonLFile outputFile chunk
        )
  where filePattern outputFileTemplate i =
            T.unpack
            $ T.replace "{n}" (T.pack $ show i)
            $ T.pack outputFileTemplate
