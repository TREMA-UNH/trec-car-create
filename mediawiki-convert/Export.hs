{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Foldable
import Data.Maybe

import qualified Data.Map.Strict as M
import qualified Data.Set as S


import Data.List
import System.FilePath

import Options.Applicative

import CAR.Types hiding (transform)
import CAR.ToolVersion
import CAR.CarExports as Exports
import CAR.AnnotationsFile as AnnsFile
import CAR.QRelFile





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
        [ exportPages
          <$> option str (long "pages" <> metavar "OUTPUT" <> help "Export pages")

        , exportOutlines
          <$> option str (long "outlines" <> metavar "OUTPUT" <> help "Export outlines")

        , exportParagraphs
          <$> option str (long "paragraphs" <> metavar "OUTPUT" <> help "Export paragraphs")

        , exportParagraphAnnotations id id
          <$> option str (long "para-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations id resolveSubsumption
          <$> option str (long "para-tree-qrel" <> metavar "OUTPUT" <> help "Export tree qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathArticle id
          <$> option str (long "para-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for paragraphs")

        , exportParagraphAnnotations cutSectionPathTopLevel id
          <$> option str (long "para-toplevel-qrel" <> metavar "OUTPUT" <> help "Export toplevel qrel for paragraphs")

        , exportEntityAnnotations id id
          <$> option str (long "entity-hier-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations id resolveEntitySubsumption
          <$> option str (long "entity-tree-qrel" <> metavar "OUTPUT" <> help "Export tree qrel for entities")

        , exportEntityAnnotations cutSectionPathArticle id
          <$> option str (long "entity-article-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityAnnotations cutSectionPathTopLevel id
          <$> option str (long "entity-toplevel-qrel" <> metavar "OUTPUT" <> help "Export hierarchical qrel for entities")

        , exportEntityPassageAnnotations id resolveEntityPassageSubsumption
          <$> option str (long "entity-psg-tree-qrel" <> metavar "OUTPUT" <> help "Export tree qrel for entities with support passages")


        , exportAllWithPrefix
          <$> option str (short 'o' <> long "output-prefix" <> metavar "PREFIX" <> help "Export all under prefix (backwards compatibility)")
        ]

type Exporter = Provenance -> [Page] -> IO ()

exportOutlines :: FilePath -> Exporter
exportOutlines outPath prov pagesToExport = do
    putStr "Writing outlines..."
    let skeletonFile = outPath 
    writeCarFile skeletonFile prov $ map toStubSkeleton pagesToExport
    putStrLn "done"

sortParagraphs :: [Paragraph] -> [Paragraph]
sortParagraphs ps = sortByParagraphIds (paraId) ps


sortByParagraphIds :: (a -> ParagraphId) -> [a] -> [a]
sortByParagraphIds by as = fmap snd $ M.toAscList 
                         $ M.fromList
                         $  [ (by a, a)
                            | a  <- as 
                            ]


exportParagraphs :: FilePath -> Exporter
exportParagraphs outPath prov pagesToExport = do
    putStr "Writing paragraphs..."
    let paragraphsFile = outPath
    writeCarFile paragraphsFile prov $ sortParagraphs $ concatMap toParagraphs pagesToExport
    putStrLn "done"

exportParagraphAnnotations :: (SectionPath -> SectionPath) -> ([Annotation IsRelevant] -> [Annotation IsRelevant]) -> FilePath -> Exporter
exportParagraphAnnotations cutSectionPath transform outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (Annotation sectionPath paragId rel) =
          Annotation (cutSectionPath sectionPath) paragId rel
    writeParagraphQRel outPath
          $ transform
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toAnnotations pagesToExport
    putStrLn "done"


resolveSubsumption :: [Annotation IsRelevant] -> [Annotation IsRelevant]
resolveSubsumption annotations =
        [ (Annotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headingPrefix}) paraid rel)
        | (Annotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headings}) paraid rel) <- annotations
        , headingPrefix <- heads headings
        ]
  where heads xs = map reverse $ tails $ reverse xs

resolveEntitySubsumption ::  [EntityAnnotation IsRelevant] -> [EntityAnnotation IsRelevant]
resolveEntitySubsumption annotations =
        [ (EntityAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headingPrefix}) entityId rel)
        | (EntityAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headings}) entityId rel) <- annotations
        , headingPrefix <- heads headings
        ]
  where heads xs = map reverse $ tails $ reverse xs

resolveEntityPassageSubsumption ::  [EntityPassageAnnotation IsRelevant] -> [EntityPassageAnnotation IsRelevant]
resolveEntityPassageSubsumption annotations =
        [ (EntityPassageAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headingPrefix}) entityId entityId' rel)
        | (EntityPassageAnnotation (SectionPath {sectionPathPageId=pageid, sectionPathHeadings=headings}) entityId entityId' rel) <- annotations
        , headingPrefix <- heads headings
        ]
  where heads xs = map reverse $ tails $ reverse xs

cutSectionPathArticle, cutSectionPathTopLevel :: SectionPath -> SectionPath
cutSectionPathArticle (SectionPath pgId _headinglist)  =
    SectionPath pgId mempty
cutSectionPathTopLevel  (SectionPath pgId headinglist) =
    SectionPath pgId (take 1 headinglist)

exportEntityAnnotations :: (SectionPath -> SectionPath) -> ([EntityAnnotation IsRelevant] -> [EntityAnnotation IsRelevant]) -> FilePath -> Exporter
exportEntityAnnotations cutSectionPath transform outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (EntityAnnotation sectionPath entityId rel) =
          EntityAnnotation (cutSectionPath sectionPath) entityId rel
    writeEntityQRel outPath
          $ transform
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toEntityAnnotations pagesToExport
    putStrLn "done"

exportEntityPassageAnnotations :: (SectionPath -> SectionPath) -> ([EntityPassageAnnotation IsRelevant] -> [EntityPassageAnnotation IsRelevant]) -> FilePath -> Exporter
exportEntityPassageAnnotations cutSectionPath transform outPath _prov pagesToExport = do
    putStr "Writing section relevance annotations..."
    let cutAnnotation (EntityPassageAnnotation sectionPath entityId paragId rel) =
          EntityPassageAnnotation (cutSectionPath sectionPath) entityId paragId rel
    writeEntityPassageQRel outPath
          $ transform
          $ S.toList
          $ S.map cutAnnotation
          $ foldMap Exports.toEntityPassageAnnotations pagesToExport
    putStrLn "done"


exportPages :: FilePath -> Exporter
exportPages outPath prov pagesToExport = do
    putStr "Writing articles..."
    let articleFile = outPath
    writeCarFile articleFile prov pagesToExport
    putStrLn "done"



exportAllWithPrefix :: FilePath -> Exporter
exportAllWithPrefix outpath prov pages= do
    exportPages (outpath <> "-articles.cbor") prov pages
    exportOutlines (outpath <> "-outlines.cbor") prov pages
    exportParagraphs ( outpath <> "-paragraphs.cbor") prov pages
    exportParagraphAnnotations id id (outpath <.> "hierarchical.qrels") prov pages
    exportParagraphAnnotations id resolveSubsumption (outpath <.> "tree.qrels") prov pages
    exportParagraphAnnotations cutSectionPathArticle id  (outpath <.> "article.qrels") prov pages
    exportParagraphAnnotations cutSectionPathTopLevel id (outpath <.> "toplevel.qrels") prov pages
    exportEntityAnnotations id id  (outpath <.> "hierarchical.entity.qrels") prov pages
    exportEntityAnnotations id resolveEntitySubsumption (outpath <.> "tree.entity.qrels") prov pages
    exportEntityAnnotations cutSectionPathArticle id (outpath <.> "article.entity.qrels") prov pages
    exportEntityAnnotations cutSectionPathTopLevel id (outpath <.> "toplevel.entity.qrels") prov pages
    exportEntityPassageAnnotations id resolveEntityPassageSubsumption (outpath <.> "tree.entitypsg.qrels") prov pages

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
