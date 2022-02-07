{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Foldable
import Data.Maybe

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.List
import System.FilePath

import Options.Applicative

import CAR.Types hiding (transform)
import CAR.ToolVersion
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile
import CAR.QRelFile
import CAR.Utils (pageParasWithPaths)
import Data.ByteString.Char8 (hPutStrLn)


-- Cluster benchmark

import Data.Aeson
    ( FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), (.:), (.:?) )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import CAR.CarJSON

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip as GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import CAR.Types.AST (SectionPath(SectionPath))



options :: Parser (FilePath, [PageName], [PageId], [Exporter])
options =
    (,,,)
        <$> argument str (help "annotations file" <> metavar "FILE")
        <*> many (option (packPageName <$> str)
                         (short 'p' <> long "page"
                          <> metavar "PAGE NAME" <> help "Export only this page"))
        <*> many (option (packPageId <$> str)
                         (short 'P' <> long "page-id"
                          <> metavar "PAGE ID" <> help "Export only this page"))
        <*> some exporter
  where
    exporter :: Parser Exporter
    exporter = asum
----- cluster ------
        [ exportClusterParagraphAnnotations cutSectionPathTopLevel
          <$> option str (long "para-toplevel-cluster" <> metavar "OUTPUT" <> help "Export cluster ground truth for paragraphs from toplevel")
        ]
------

exportAllWithPrefix :: FilePath -> Exporter
exportAllWithPrefix outpath prov pages= do
    exportClusterParagraphAnnotations cutSectionPathTopLevel  (outpath <.> "toplevel.cluster.jsonl.gz") prov pages


type Exporter = Provenance -> [Page] -> IO ()



sortByParagraphIds :: (a -> ParagraphId) -> [a] -> [a]
sortByParagraphIds by as = fmap snd $ M.toAscList 
                         $ M.fromList
                         $  [ (by a, a)
                            | a  <- as 
                            ]


cutSectionPathArticle, cutSectionPathTopLevel :: SectionPath -> SectionPath
cutSectionPathArticle (SectionPath pgId _headinglist)  =
    SectionPath pgId mempty
cutSectionPathTopLevel  (SectionPath pgId headinglist) =
    SectionPath pgId (take 1 headinglist)

-- ----------- clustering -------------

data ClusterBenchmark = ClusterBenchmark { query :: PageId
                     , elements :: [ParagraphId]
                    , trueClusters :: [Int]
                    , trueLabels :: Maybe [SectionPath] }
  deriving (Show, Eq)

k_QUERY = "query"
k_ELEMENTS = "elements"
k_TRUE_CLUSTERS = "true_cluster_idx"
k_TRUE_LABELS = "true_cluster_labels"


instance Aeson.ToJSON (ClusterBenchmark) where 
    toJSON (ClusterBenchmark{..}) = 
      Aeson.object 
      $ ([ k_QUERY .= S query
        , k_ELEMENTS .= map S elements
        , k_TRUE_CLUSTERS .= trueClusters
        ] ++
        case trueLabels of
          Nothing -> []
          Just lbs -> [ k_TRUE_LABELS .= map S lbs ]
      )

instance Aeson.FromJSON (ClusterBenchmark) where
    parseJSON = Aeson.withObject "S ClusterBenchmark" $ \content -> do
        S query <- content .: k_QUERY 
        elements <- unwrapS $ content .: k_ELEMENTS
        trueClusters <- unwrapS $ content .: k_TRUE_CLUSTERS
        -- let trueLabels = Nothing --- @laura Fix!
        maybeLabels <- content .:? k_TRUE_LABELS
            :: Aeson.Parser (Maybe [S SectionPath])
        let trueLabels :: Maybe [SectionPath]
            trueLabels =  unwrapS <$> maybeLabels -- maybe Nothing unwrapS maybeLabels
        return $ ClusterBenchmark {..}


printClusterBenchmark :: ClusterBenchmark -> String
printClusterBenchmark ClusterBenchmark{..} = 
  "query: " <>  (unpackPageId query) <>
  "\n paraIds: "<> (unwords $ fmap unpackParagraphId elements) <>
  "\n trueLabels" <> (unwords $ fmap escapeSectionPath $ fromMaybe [] trueLabels) <>
  "\n trueClusters" <> (show trueClusters)


writeGzJsonLClusterBenchmarkFile ::  FilePath -> [ClusterBenchmark] -> IO()
writeGzJsonLClusterBenchmarkFile fname benchmarks = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode) $ benchmarks
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines
    Prelude.putStrLn  $ "Writing ClusterBenchmark JsonL.gz to "<> fname



-- Export cluster ground truth to work with scikit.learn cluster evaluation package
exportClusterParagraphAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Exporter
exportClusterParagraphAnnotations cutSectionPath outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let benchmarks = fmap toClusterBenchmark pagesToExport
    -- putStrLn $ unlines $ fmap printClusterBenchmark $ benchmarks
    writeGzJsonLClusterBenchmarkFile outPath benchmarks      
    -- putStrLn "done"
   where toClusterBenchmark :: Page -> ClusterBenchmark   
         toClusterBenchmark  page = 
            let clusterParas =
                  sortByParagraphIds (\(_a, para) -> paraId para)
                  $ [ (cutSectionPath sectionPath, para)
                    | (sectionPath@SectionPath{sectionPathHeadings= headings}, para) <- pageParasWithPaths page
                    , headings  /= []  -- skip lead text
                    ]

                clusters = S.toList $ S.fromList 
                           $ fmap fst clusterParas

                clusterIdx :: HM.HashMap SectionPath Int
                clusterIdx = HM.fromList $ zip clusters [0 ..]

                clusterBenchmark = ClusterBenchmark {
                                     query = pageId page
                                     , elements = fmap (paraId . snd) clusterParas
                                     , trueLabels = Just $ fmap fst clusterParas
                                     , trueClusters = [ clusterIdx HM.! s | (s, _p) <- clusterParas ]
                                    }
            in clusterBenchmark 
-- -------------------------------------



main :: IO ()
main = do
    (path, names, pageIds1, exporters) <- execParser' 2 (helper <*> options) mempty
--     anns <- openAnnotations path
--     (prov, _) <- readPagesFileWithProvenance path
--     nameMap <- CARN.openNameToIdMap path
    pageBundle <- openPageBundle path


    let pageIds2 = S.toList $ bundleLookupAllPageNames pageBundle names

    forM_ exporters $ \exporter ->
        let pagesToExport
              | null names = bundleAllPages pageBundle
              | otherwise  = mapMaybe (bundleLookupPage pageBundle)  $ nub (pageIds1 ++ pageIds2)
            {-# INLINE pagesToExport #-}
        in exporter (bundleProvenance pageBundle) pagesToExport



