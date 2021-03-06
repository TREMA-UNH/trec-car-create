{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Foldable
import Data.Maybe
import System.FilePath
import Options.Applicative


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text.Lazy as TL

import CAR.Types hiding (transform)
import CAR.ToolVersion 
import CAR.AnnotationsFile as AnnsFile
import CAR.Utils (pageParasWithPaths, paraLinks, paraToText, pageParas)


-- Cluster benchmark

import Data.Aeson
    ( FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), (.:), (.:?) )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import CAR.CarJSON

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip as GZip
import CAR.NameToIdMap (NameToQidMap(..), openRNameToQidMap'')



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
        [
----- cluster ------
         exportClusterParagraphAnnotations cutSectionPathTopLevel
          <$> option str (long "para-toplevel-cluster" <> metavar "OUTPUT" <> help "Export cluster ground truth for paragraphs from toplevel")
----- entity-linking ------
        , exportEntityLinkAnnotations 
          <$> option str (long "entity-linking" <> metavar "OUTPUT" <> help "Export entity linking ground truth for paragraphs")
          <*> option str (long "qid2name" <> metavar "TOC.qid2name" <> help "TOC file to convert page titles to qids")
------
        ]

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

data ClusterBenchmark = ClusterBenchmark { queryId :: PageId
                    , queryText :: PageName 
                    , elements :: [ParagraphId]
                    , trueClusters :: [Int]
                    , trueLabels :: Maybe [SectionPath] }
  deriving (Show, Eq)

k_ELEMENTS = "elements"
k_TRUE_CLUSTERS = "true_cluster_idx"
k_TRUE_LABELS = "true_cluster_labels"


instance Aeson.ToJSON (ClusterBenchmark) where 
    toJSON (ClusterBenchmark{..}) = 
      Aeson.object 
      $ ([ k_QUERY_ID .= S queryId
         , k_QUERY_TEXT .= S queryText
        , k_ELEMENTS .= map S elements
        , k_TRUE_CLUSTERS .= trueClusters
        ] ++
        case trueLabels of
          Nothing -> []
          Just lbs -> [ k_TRUE_LABELS .= map S lbs ]
      )

instance Aeson.FromJSON (ClusterBenchmark) where
    parseJSON = Aeson.withObject "S ClusterBenchmark" $ \content -> do
        S queryId <- content .: k_QUERY_ID
        S queryText <- content .: k_QUERY_TEXT 
        elements <- unwrapS $ content .: k_ELEMENTS
        trueClusters <- unwrapS $ content .: k_TRUE_CLUSTERS
        -- let trueLabels = Nothing --- @laura Fix!
        maybeLabels <- content .:? k_TRUE_LABELS
            :: Aeson.Parser (Maybe [S SectionPath])
        let trueLabels :: Maybe [SectionPath]
            trueLabels =  unwrapS <$> maybeLabels -- maybe Nothing unwrapS maybeLabels
        return $ ClusterBenchmark {..}



writeGzJsonLBenchmarkFile :: ToJSON a =>  FilePath -> [a] -> IO()
writeGzJsonLBenchmarkFile fname benchmarks = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode) $ benchmarks
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines
    Prelude.putStrLn  $ "Writing ClusterBenchmark JsonL.gz to "<> fname


printClusterBenchmark :: ClusterBenchmark -> String
printClusterBenchmark ClusterBenchmark{..} = 
  "queryId: " <>  (unpackPageId queryId) <>
  "\n queryText: " <>  (unpackPageName queryText) <>
  "\n paraIds: "<> (unwords $ fmap unpackParagraphId elements) <>
  "\n trueLabels" <> (unwords $ fmap escapeSectionPath $ fromMaybe [] trueLabels) <>
  "\n trueClusters" <> (show trueClusters)

minClusterSize :: ClusterBenchmark -> Bool 
minClusterSize ClusterBenchmark{..} =
    let clusters = S.size $S.fromList trueClusters
        elems = length elements
    in clusters > 1 && elems >= 1 

-- Export cluster ground truth to work with scikit.learn cluster evaluation package
exportClusterParagraphAnnotations :: (SectionPath -> SectionPath) -> FilePath -> Exporter
exportClusterParagraphAnnotations cutSectionPath outPath _prov pagesToExport = do
    putStr "Writing cluster benchmark..."
    let benchmarks = filter minClusterSize $ fmap toClusterBenchmark pagesToExport
    -- putStrLn $ unlines $ fmap printClusterBenchmark $ benchmarks
    writeGzJsonLBenchmarkFile outPath benchmarks      
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
                                     queryId = pageId page
                                     , queryText = pageName page
                                     , elements = fmap (paraId . snd) clusterParas
                                     , trueLabels = Just $ fmap fst clusterParas
                                     , trueClusters = [ clusterIdx HM.! s | (s, _p) <- clusterParas ]
                                    }
            in clusterBenchmark 

-- ----------- entity linking -------


data EntityLinkingBenchmark = EntityLinkingBenchmark 
                            { queryId :: PageId
                            , queryText :: PageName
                            , paragraphId :: ParagraphId
                            , textOnlyParagraph :: Paragraph
                            , trueLinkedParagraph :: Paragraph
                            , trueLabelPageIds :: [PageId]
                            , trueLabelQids :: [WikiDataId]
                            , acceptableLabelQids :: [WikiDataId]
                            , acceptableLabelPageIds :: [PageId]

                            }
  deriving (Show, Eq)

k_QUERY_ID = "query_id"
k_QUERY_TEXT = "query_text"
k_PARAGRAPH_ID = "paragraph_id"
k_TEXT_ONLY_PARAGRAPH = "text_only_paragraph"
k_TRUE_LINKED_PARAGRAPH = "true_linked_paragraph"
k_TRUE_LABEL_PAGE_IDS = "true_label_page_ids"
k_TRUE_LABEL_QIDS = "true_label_qids"
k_ACCEPTABLE_LABEL_QIDS = "acceptable_label_qids"
k_ACCEPTABLE_LABEL_PAGE_IDS = "acceptable_label_page_ids"


instance Aeson.ToJSON (EntityLinkingBenchmark) where 
    toJSON (EntityLinkingBenchmark{..}) = 
      Aeson.object 
      $ [ k_QUERY_ID .= S queryId
        , k_QUERY_TEXT .= S queryText
        , k_PARAGRAPH_ID .= S paragraphId
        , k_TEXT_ONLY_PARAGRAPH .= S textOnlyParagraph
        , k_TRUE_LINKED_PARAGRAPH .= S trueLinkedParagraph
        , k_TRUE_LABEL_PAGE_IDS .= fmap S trueLabelPageIds
        , k_TRUE_LABEL_QIDS .= trueLabelQids -- use native JSON representation
        , k_ACCEPTABLE_LABEL_PAGE_IDS .= fmap S acceptableLabelPageIds
        , k_ACCEPTABLE_LABEL_QIDS .= acceptableLabelQids -- use native JSON representation
        ] 
      

instance Aeson.FromJSON (EntityLinkingBenchmark) where
    parseJSON = Aeson.withObject "S EntityLinkingBenchmark" $ \content -> do
        S queryId <- content .: k_QUERY_ID 
        S queryText <- content .: k_QUERY_TEXT 
        S paragraphId <- content .: k_PARAGRAPH_ID
        S textOnlyParagraph <- content .: k_TEXT_ONLY_PARAGRAPH
        S trueLinkedParagraph <- content .: k_TRUE_LINKED_PARAGRAPH
        trueLabelPageIds <- unwrapS <$> content .: k_TRUE_LABEL_PAGE_IDS
        trueLabelQids <- content .: k_TRUE_LABEL_QIDS  -- use native JSON representation
        acceptableLabelPageIds <- unwrapS <$> content .: k_ACCEPTABLE_LABEL_PAGE_IDS
        acceptableLabelQids <- content .: k_ACCEPTABLE_LABEL_QIDS  -- use native JSON representation
        return $ EntityLinkingBenchmark {..}



printEntityLinkingBenchmark :: EntityLinkingBenchmark -> String
printEntityLinkingBenchmark EntityLinkingBenchmark{..} = 
  "queryId: " <>  (unpackPageId queryId) <>
  "queryText: " <>  (unpackPageName queryText) <>
  "\n paragraphId: "<> (unpackParagraphId paragraphId) <>
  "\n textOnlyParagraph" <> (show textOnlyParagraph) <>
  "\n trueLinkedParagraph" <> (show trueLinkedParagraph) <>
  "\n trueLabelPageIds" <> (show $ fmap unpackPageId trueLabelPageIds) <>
  "\n trueLabelQids" <> (show $ fmap show trueLabelQids) <>
  "\n acceptableLabelPageIds" <> (show $ fmap unpackPageId acceptableLabelPageIds) <>
  "\n acceptableLabelQids" <> (show $ fmap show acceptableLabelQids)
  



-- Export cluster ground truth to work with scikit.learn cluster evaluation package
exportEntityLinkAnnotations :: FilePath ->  FilePath ->Exporter
exportEntityLinkAnnotations outPath nameToQidMapFile _prov pagesToExport = do
    nameToQidMap <- openRNameToQidMap'' nameToQidMapFile
    putStr "Writing entity linking benchmark..."

    let benchmarks = filter minEntityLink
                     [ mark
                     | page <- pagesToExport
                     , mark <- pageToEntityLinkingBenchmark nameToQidMap page
                     ]

    -- putStrLn $ unlines $ fmap printEntityLinkingBenchmark $ benchmarks
    writeGzJsonLBenchmarkFile outPath benchmarks      
    -- putStrLn "done"
   where pageToEntityLinkingBenchmark :: NameToQidMap -> Page -> [EntityLinkingBenchmark]
         pageToEntityLinkingBenchmark  nameToQidMap page =
             let marks = fmap (toEntityLinkingBenchmark nameToQidMap page) $ pageParas page
                 marksWithQids = [ mark{acceptableLabelQids = prevEntities}
                                 | (mark, prevEntities) <- accumPrevList trueLabelQids marks
                                 ]
                 marksWithPageIds = [ mark{acceptableLabelPageIds = prevEntities}
                                    | (mark, prevEntities) <- accumPrevList trueLabelPageIds marksWithQids
                                    ]
             in marksWithPageIds

           where accumPrevList :: forall a b . Ord b => (a -> [b]) -> [a] -> [(a,[b])]
                 accumPrevList f lst =
                     let accum :: [[b]]
                         accum =
                            fmap (S.toList . S.fromList)
                            $ scanl1 (<>) 
                            $ fmap f lst
                     in zip lst accum

         toEntityLinkingBenchmark :: NameToQidMap -> Page -> Paragraph -> EntityLinkingBenchmark   
         toEntityLinkingBenchmark  nameToQidMap page paragraph = 
            let textOnly = TL.toStrict $ paraToText paragraph
                textOnlyParagraph = Paragraph {paraId = paraId paragraph
                                            , paraBody = [ParaText textOnly]
                                            }
                entityIds = fmap linkTargetId $ paraLinks paragraph
                entityTitles = fmap linkTarget $ paraLinks paragraph

                entityLinkingBenchmark = EntityLinkingBenchmark {
                                        queryId = pageId page
                                        , queryText = pageName page
                                        , paragraphId = paraId paragraph
                                        , textOnlyParagraph = textOnlyParagraph 
                                        , trueLinkedParagraph = augmentParagraphWithQids nameToQidMap paragraph 
                                        , trueLabelPageIds = entityIds
                                        , trueLabelQids = concatMap (lookupQidsForPageNames nameToQidMap) entityTitles 
                                        , acceptableLabelPageIds = []
                                        , acceptableLabelQids = []
                                        }
            in entityLinkingBenchmark 

         minEntityLink :: EntityLinkingBenchmark -> Bool 
         minEntityLink EntityLinkingBenchmark{..} =
             let numLinks = length trueLabelPageIds
             in numLinks >= 1
-- -------------------------------------

-- Code for embellishing Paragraph links with Qids.  should ultimately move to Trec-car-create pipeline.

lookupQidsForPageNames :: NameToQidMap -> PageName -> [WikiDataId]
lookupQidsForPageNames (NameToQidMap m) pageName =
             [ qid
             | Just qidSet <- pure $ pageName `M.lookup` m
             , let qid:_ = S.toList qidSet
             ]

augmentParagraphWithQids :: NameToQidMap -> Paragraph -> Paragraph 
augmentParagraphWithQids nameToQidMap p@Paragraph{..} =
    p{paraBody=fmap (augmentParaBodyWithQids nameToQidMap) paraBody}


augmentParaBodyWithQids :: NameToQidMap -> ParaBody -> ParaBody 
augmentParaBodyWithQids _nameToQidMap t@(ParaText _)  =
    t
augmentParaBodyWithQids nameToQidMap (ParaLink link)  =
    ParaLink $ augmentLinkWithQids nameToQidMap link

augmentLinkWithQids :: NameToQidMap -> Link -> Link
augmentLinkWithQids nameToQidMap (l@Link {..})  =
    let qids = lookupQidsForPageNames nameToQidMap linkTarget
    in l { linkTargetQid = listToMaybe qids}

-- ----------------------------------

main :: IO ()
main = do
    (path, names, pageIds1, exporters) <- execParser' 7 (helper <*> options) mempty
    pageBundle <- openPageBundle path


    let pageIds2 = S.toList $ bundleLookupAllPageNames pageBundle names

    forM_ exporters $ \exporter ->
        let pagesToExport
              | null names = bundleAllPages pageBundle
              | otherwise  = mapMaybe (bundleLookupPage pageBundle)  $ nub (pageIds1 ++ pageIds2)
            {-# INLINE pagesToExport #-}
        in exporter (bundleProvenance pageBundle) pagesToExport



