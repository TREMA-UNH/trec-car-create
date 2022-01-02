{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}

import Data.Semigroup hiding (option)
import Options.Applicative

import Data.Maybe 
import CAR.Types
import CAR.ToolVersion
import CAR.FillMetadata



data Stage = StageResolveRedirect
           | StageResolveDisambiguationAndInlinks
           | StageCategoryTags
           | StageWikiDataQids FilePath SiteId -- wikiDataDumpFile siteId

data Opts = Opts { inputPath :: FilePath
                 , outputPath :: FilePath
                 , stage :: Stage
                 }

opts :: Parser Opts
opts = do
    inputPath <- option str (short 'i' <> long "input" <> metavar "INFILE" <> help "Input CBOR pages file" )
    outputPath <- option str (short 'o' <> long "output" <> metavar "OUTFILE" <> help "Output CBOR pages file ")
    stage <- redirect <|> disambigInlinks  <|> categoryTags <|> wikiDataQids
    return Opts {..}
  where
    redirect = flag' StageResolveRedirect (short 'r' <> long "redirect" <> help "Resolve redirect stage")
    disambigInlinks = flag' StageResolveDisambiguationAndInlinks (short 'd' <> long "disambiguation" <> help "Collect disambiguation names and inlinks")
    categoryTags = flag' StageCategoryTags (short 'c' <> long "category" <> help "Load category tags into meta data")

    wikiDataQids :: Parser Stage
    wikiDataQids =
      flag' StageWikiDataQids (short 'q' <> long "qid" <> help "Load WikiData QIDs into meta data")
        <*> option str (long "wikidata" <> metavar "WIKIDATAFILE" <> help "Matching WikiData dump file")  
        <*> option str (long "siteId" <> metavar "SITE" <> help "Site id of the dump, e.g. 'enwiki' for English Wikipedia")


main :: IO ()
main = do
    Opts{..} <- execParser' 2 (helper <*> opts) $ progDescDoc (Just "Fill in derived page metadata. ")

    (prov, pages') <-
      case stage of
          StageResolveRedirect                 -> stageResolveRedirect inputPath
          StageResolveDisambiguationAndInlinks -> stageResolveDisambiguationAndInlinks inputPath
          StageCategoryTags                    -> stageResolveCategoryTags inputPath
          StageWikiDataQids wikiDataFile siteId -> stageResolveWikiDataQIDs  wikiDataFile siteId inputPath
    writeCarFile outputPath prov pages'


