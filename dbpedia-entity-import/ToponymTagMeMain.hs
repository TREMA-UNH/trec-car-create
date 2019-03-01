{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Monoid hiding (All, Any)
import Control.Monad
import System.Environment

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
import Data.Void
import Control.Monad (void)
import Options.Applicative
import qualified Data.Binary.Serialise.CBOR as CBOR

import CAR.Types.AST as CAR
import CAR.ToolVersion
import CAR.Types
import qualified SimplIR.Format.QRel as QF
import qualified SimplIR.Format.TrecRunFile as RF
import CAR.AnnotationsFile as CAR
import qualified Debug.Trace as Debug


-- import Control.DeepSeq
import Data.Char
import Data.Maybe
import Data.Semigroup hiding (option)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Builder as TB
-- import qualified Data.Text.Lazy.Builder.Int as TB
-- import qualified Data.Text.Lazy.Builder.RealFloat as TB
-- import qualified Data.Text.Lazy.Read as TL.Read
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Aeson as Aeson
import GHC.Generics
import System.FilePath

import TagMe

data PubmedDocument = PubmedDocument { content :: T.Text
                                     , filename :: T.Text
                                     }
        deriving (Aeson.ToJSON, Generic)

data PubmedAnnotations = PubmedAnnotations { doc :: PubmedDocument
                                           , annotations :: [Annotation]
                                           }
        deriving (Aeson.ToJSON, Generic)

data ToponymWrapper = ToponymWrapper { list :: [PubmedAnnotations]}
        deriving (Aeson.ToJSON, Generic)

readPubmedFiles :: [FilePath] -> Int ->  IO [PubmedDocument]
readPubmedFiles (f1:rest) maxLen = do
    d1 <- readPubmedFile f1 maxLen
    drest <- readPubmedFiles rest maxLen
    return $ d1:drest
readPubmedFiles [] _ = do
    pure []


readPubmedFile :: FilePath -> Int -> IO PubmedDocument
readPubmedFile fname maxLen = do
    text <- T.readFile fname
    let text' = T.take maxLen  text -- $  T.replace "\n" " " text
    return PubmedDocument { content = text'
                         , filename = T.pack $ takeBaseName fname
                         }


writePubmedAnnotations :: FilePath -> [PubmedAnnotations] -> IO ()
writePubmedAnnotations fname outData =
    BSL.writeFile fname $ Aeson.encode $ ToponymWrapper outData

tagData :: TagMe.TagMeEnv -> TagMe.Token -> PubmedDocument -> IO [TagMe.Annotation]
tagData env tagMeToken document = do
    let t = content document
    res <- annotateWithEntityLinksConf env tagMeToken t tagMeOptions
    return [ annotation
           | TextEntityLink _  annotation <- res
           ]



tagMeOptions :: TagMeOptions
tagMeOptions = TagMeOptions { inclAbstract = False
                            , inclCategories = True
                            , isTweet = False
                            , isLongText = True
                            , language = langEn
                            }




helpDescr :: PP.Doc
helpDescr =
    "Convert PubMed documents to TagMe annotations."


opts :: Parser (IO ())
opts = subparser
    $  cmd "annotate"   annotatePubMed'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    inputRawDataFile = argument str (help "pubmed text file" <> metavar "TXT")
    maxLen = option auto (help "max length of text to submit to TagMe" <> metavar "L")
    outputFile = option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    annotatePubMed' =
        annotatePubMed <$> (many inputRawDataFile) <*> outputFile <*> maxLen

    annotatePubMed :: [FilePath] -> FilePath -> Int -> IO()
    annotatePubMed inFiles outputFile maxLen = do
        tagMeToken <- Token . T.pack <$> getEnv "TAG_ME_TOKEN"
        env <- mkTagMeEnv

        inData <- readPubmedFiles inFiles maxLen
               :: IO [PubmedDocument]
        annotatedInData <- sequence [ do anns <- tagData env tagMeToken line
                                         return PubmedAnnotations {doc = line, annotations = anns}
                                    | line <- inData
                                    ]
                           :: IO [PubmedAnnotations]

        writePubmedAnnotations outputFile annotatedInData

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)