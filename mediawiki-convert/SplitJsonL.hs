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
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip as GZip
import Data.List.Split as S



import SimplIR.DataSource.Compression.Lazy
import CAR.ToolVersion

helpDescr :: PP.Doc
helpDescr =
    "Split JSONL into multiple archives of `n` entries " <$$> PP.indent 4 options
  where
    -- cmd a b = PP.nest 8 (a <$$> b)
    options = PP.vsep
      [
        -- cmd "need doc"                           "here"
      ]

opts :: Options.Applicative.Parser (FilePath, FilePath, Int)
opts =
    (,,)
    <$> argument str (help "input file" <> metavar "JSONL.gz")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file template, will substitute chunk number `{n}` for filename ex: \"input-{n}.jsonl.gz\" . Will be written as gzip encoded JSONL")
    <*> option auto (short 'n' <> long "num" <> metavar "N" <> help "Number of objects in each splitted file.")


--  -------------- file writing and command line handling -------------

writeGzJsonLFile ::  FilePath -> [BSL.ByteString] -> IO()
writeGzJsonLFile fname lnes = do
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lnes
    Prelude.putStrLn  $ "Writing JsonL.gz to "<> fname

main :: IO ()
main = do
    (inputFile, outputFileTemplate, numLines) <- execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)
    lns <- BSL.lines . decompress <$> BSL.readFile inputFile
          :: IO [BSL.ByteString] 
    let chunks = S.chunksOf numLines lns
    forM_ (zip [1::Integer ..] chunks) (\(i, chnk) -> 
            let outputFile = filePattern outputFileTemplate i
            in writeGzJsonLFile outputFile chnk
        )
  where filePattern outputFileTemplate i =
            T.unpack
            $ T.replace "{n}" (T.pack $ show i)
            $ T.pack outputFileTemplate
