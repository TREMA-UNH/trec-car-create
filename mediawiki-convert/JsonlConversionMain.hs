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

import CAR.Types
import CAR.ToolVersion


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

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


t :: T.Text -> T.Text
t = id

t_PARAGRAPH = t "paragraph"
t_SECTION = t "section"
t_LIST = t "list"
t_IMAGE = t "image"
t_INFOBOX = t "infobox"

k_TYPE = "type"
k_LINK = "link"
k_TEXT = "text"
k_KEY = "key"
k_VALUE = "value"
k_FREQ = "freq"
k_TARGET_PAGE = "target_page"
k_TARGET_PAGE_ID = "target_page_id"
k_TARGET_SECTION  = "target_section"
k_PARA_ID = "para_id"
k_PARA_BODY = "para_body"
k_HEADING_TEXT = "heading_text"
k_HEADING_ID = "heading_id"
k_SKELETON = "skeleton"
k_PARAGRAPH = "paragraph"
k_INDENT = "indent"
k_ENTRY = "entry"
k_IMAGE_FILE = "image_file"
k_IMAGE_CAPTION = "image_caption"
k_INFOBOX_NAME = "infobox_name"
k_INFOBOX_ENTRIES = "infobox_entries"
k_REDIRECT_NAMES = "redirect_names"
k_DISAMBIGUATION_NAMES = "disambiguation_names"
k_DISAMBIGUATION_IDS = "disambiguation_ids"
k_CATEGORY_NAMES = "category_names"
k_CATEGORY_IDS = "category_ids"
k_INLINK_IDS = "inlink_ids"
k_OLD_INLINK_ANCHORS = "old_inlink_anchors"
k_INLINK_ANCHORS = "inlink_anchors"
k_WIKIDATA_QID = "wiki_data_qid"
k_WIKI_SITE_ID = "wiki_site_id"

data InfoboxEntry = InfoboxEntry { infoboxKey :: T.Text
                                 , infoboxValue :: [PageSkeleton] 
                                 }

-- | Serialized newtype for JSON representation
newtype S a = S a
    deriving (Show)

deriving instance Aeson.ToJSON a => Aeson.ToJSON (S [a])
instance Aeson.ToJSON (S Link) where 
    toJSON (S (Link {..})) = 
        object
        $ [ k_TEXT .= linkAnchor
         , k_TARGET_PAGE .= linkTarget
         , k_TARGET_PAGE_ID .= linkTargetId
         ] <>
         maybe [] (\l -> [ k_TARGET_SECTION .= l]) linkSection

instance Aeson.ToJSON (S InfoboxEntry) where 
    toJSON (S (InfoboxEntry{..})) = object $ [k_KEY .= infoboxKey, k_VALUE .= fmap S infoboxValue] 


instance Aeson.ToJSON (S ParaBody) where 
    toJSON (S (ParaText txt)) = object [k_TEXT .= txt]
    -- toJSON (S (ParaLink (link))) = object [k_TYPE .= t "link", k_LINK .= S link]
    toJSON (S (ParaLink (Link {..}))) = 
        object
        $ [ k_TEXT .= linkAnchor
         , k_TARGET_PAGE .= linkTarget
         , k_TARGET_PAGE_ID .= linkTargetId
         ] <>
         maybe [] (\l -> [ k_TARGET_SECTION .= l]) linkSection


instance Aeson.ToJSON (S Paragraph) where 
    toJSON (S (Paragraph {..})) = 
        object
        $ [ k_PARA_ID .= unpackParagraphId paraId
          , k_PARA_BODY.= fmap S paraBody
          ]



instance Aeson.ToJSON (S PageSkeleton) where 
    toJSON (S (Section (SectionHeading headingText) headingId sectionSkeleton)) =
        object 
        $ [ k_TYPE .= t_SECTION
          , k_HEADING_TEXT .= headingText
          , k_HEADING_ID .= unpackHeadingId headingId
          , k_SKELETON .=  fmap S sectionSkeleton
          ]
    toJSON (S (Para paragraph )) = 
        object 
        $ [ k_TYPE .= t_PARAGRAPH
          , k_PARAGRAPH .= S paragraph
          ]
    toJSON (S (List indent paragraph )) = 
        object 
        $ [ k_TYPE .= t_LIST
          , k_INDENT .= indent
          , k_ENTRY .= S paragraph
          ]
    toJSON (S (Image imageFile caption)) = 
        object 
        $ [ k_TYPE .= t_IMAGE
          , k_IMAGE_FILE .= imageFile
          , k_IMAGE_CAPTION .= fmap S caption
          ]
    toJSON (S (Infobox infoboxName infoboxEntries)) =
        object 
        $ [ k_TYPE .= t_INFOBOX
          , k_INFOBOX_NAME .= infoboxName
          , k_INFOBOX_ENTRIES .= 
                fmap (\(key,skel) -> S (InfoboxEntry key skel)) infoboxEntries
          ]





instance Aeson.ToJSON (S PageMetadata) where
    toJSON (S (PageMetadata metadataItems)) =
        object $ fmap metadataItemToJSON metadataItems
      where
        metadataItemToJSON :: MetadataItem ->  (T.Text, Value) -- Aeson.Pair
        metadataItemToJSON (RedirectNames pageNames) = (k_REDIRECT_NAMES.= pageNames)
        metadataItemToJSON (DisambiguationNames pageNames) = (k_DISAMBIGUATION_NAMES .= pageNames)
        metadataItemToJSON (DisambiguationIds pageIds) = (k_DISAMBIGUATION_IDS .= pageIds)
        metadataItemToJSON (CategoryNames pageNames) = (k_CATEGORY_NAMES .= pageNames)
        metadataItemToJSON (CategoryIds pageIds) = (k_CATEGORY_IDS .= pageIds)
        metadataItemToJSON (InlinkIds pageIds) = (k_INLINK_IDS.= pageIds)
    
        metadataItemToJSON (OldInlinkAnchors texts) = (k_OLD_INLINK_ANCHORS .= texts)
        metadataItemToJSON (InlinkAnchors anchorStats) = (k_INLINK_ANCHORS .= fmap anchorStatToJSON anchorStats)

        metadataItemToJSON (WikiDataQID wikiDataId) = (k_WIKIDATA_QID .= wikiDataId)
        metadataItemToJSON (WikiSiteId siteId) = (k_WIKI_SITE_ID .= siteId)
        
        metadataItemToJSON m@(UnknownMetadata _a _b _terms) = error $ "Can produce JSON for UnknownMetadata items: "<> show m

        anchorStatToJSON :: (T.Text, Int) -> Aeson.Value
        anchorStatToJSON (anchor, freq) = object $ [ k_TEXT .= anchor, k_FREQ .= freq ]

pageTypeToJSON :: PageType -> Aeson.Value
pageTypeToJSON ArticlePage = "article"
pageTypeToJSON CategoryPage = "category"
pageTypeToJSON DisambiguationPage = "disambiguation"
pageTypeToJSON (RedirectPage _link) = "redirect" -- also exposes field "redirect_target"

k_PAGE_NAME = "page_name"
k_PAGE_ID = "page_id"
k_PAGE_TYPE = "page_type"
k_REDIRECT_TARGET = "redirect_target"
k_METADATA = "metadata"


instance Aeson.ToJSON (S Page) where
    toJSON (S Page{..}) =
        object
        $ [ k_PAGE_NAME .= pageName
            , k_PAGE_ID .= pageId
            , k_PAGE_TYPE .= pageTypeToJSON pageType
        ] 
        ++ [ k_REDIRECT_TARGET .= S link | RedirectPage link <- pure pageType ]
        ++ [ k_METADATA .= S pageMetadata
           , k_SKELETON .= map S pageSkeleton
           ]


-- instance Aeson.FromJSON (S Page) where
--     parseJSON = Aeson.withObject "S Page" $ \content-> do
--         pageName <- content Aeson..: "page_name"
--         pageId <- content Aeson..: "page_id"
--         S pageType <- do
--             ty <- content Aeson..: "page_type"
--             case ty :: T.Text of
--                 "article" -> pure ArticlePage
--                 "category" -> pure CategoryPage
--                 "disambiguation" -> pure DisambiguationPage
--                 "redirect" -> do link <- content Aeson..: "redirect_target"
--                                  return $ RedirectPage link

--         S pageMetadata <- content Aeson..: "metadata"
--         pageSkeleton <- fmap (\S x -> x)  <$> content Aeson..:"page_skeleton"

--         return $ S (Page {..})


-- instance Aeson.FromJSON (S PageMetadata) where
--     parseJSON = PageMetadata <$> Aeson.withObject "S PageMetadata" \content -> do
--                   catMaybes $
--                    [(RedirectNames <$> content Aeson..:? "redict_names")
--                    , (DisambiguationNames <$> content Aeson..:? "disambiguation_names")
--                    , (DisambiguationIds <$> content Aeson..:? "disambiguation_ids")
--                    , (CategoryNames <$> content Aeson..:? "category_names")
--                    , (CategoryIds <$> content Aeson..:? "category_ids")
--                    , (InlinkIds <$> content Aeson..:? "inlink_ids")
--                    , (OldInlinkAnchors <$> content Aeson..:? "old_inlink_anchors")
--                    , (InlinkAnchors <$> content Aeson..:? "inlink_anchors")
--                    , (WikiDataQID <$> content Aeson..:? "wiki_data_qid")
--                    , (WikiSiteId <$> content Aeson..:? "wiki_site_id")
--                    ]

-- instance Aeson.FromJSON (S PageSkeleton) where
--     parseJSON = Aeson.withObject "S PageSkeleton" $ \content -> do
--         pageSkeletonOpt <- content Aeson..:? "page_skeleton"

--         sectionHeadingOpt <- content Aeson..:? "section_heading"
--         case sectionHeadingOpt of
--             Just sectionHeading -> 
--                 headingId <- content Aeson..:? "heading_id"
--                 Section sectionHeading headingId (fromJust pageSkeletonOpt)
--             Nothing -> 

--                 paragraphOpt <- content Aeson..:? "paragraph"
--                 case paragraphOpt of
--                     Just (S paragraph) -> 
--                         listIndentOpt <- content Aeson..:? "list_indent"
--                         case listIndentOpt of
--                             Just listIndent -> List listIndent paragraph
--                             nothing -> Para paragraph
--                     Nothing ->
                        
--                         imageCaptionOpt <- content Aeson..:? "image_caption"
--                         case imageCaptionOpt of
--                             Just imageCaption -> Image imageCaption (fromJust pageSkeletonOpt)
--                             Nothing -> 

--                                 infoboxNameOpt <- content Aeson..:? "infobox_name"
--                                 Just infoboxName -> Infobox infoboxName
--                                     S infoboxContent <- content Aeson..: "infobox_content"                                    
--                                     Infobox infoboxName $ fmap (\InfoboxEntry key value -> (key, value)) infoboxContent



-- instance Aeson.FromJSON (S InfoboxEntry) where
--     parseJSON = InfoboxEntry <$> Aeson.withObject "S InfoboxEntry" \content -> do
--                   infoboxKey <- content..:"key"
--                   infoboxValue <- content..:"value"
                  


-- instance (Aeson.ToJSON q, Aeson.ToJSON d) 
--         => Aeson.ToJSON (SerializedPage) where
--     toJSON (SerializedPage (CAR.Page{..})) =
--         Aeson.object $ [ "query" Aeson..= queryId
--                        , "document" Aeson..= documentName
--                        , "rank" Aeson..= documentRank
--                        , "score" Aeson..= documentScore
--                        ] <> case methodName of
--                                 "" -> []
--                                 name -> ["method" Aeson..= name]


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


