{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module CAR.CarExports
    ( ParaNumber(..)
    , PassageFile
      -- * Stubs
    , Stub(..)
    , toStubSkeleton
    , prettyStub
      -- * Paragraphs
    , toParagraphs
      -- * Ground truth
    , Relevance(..)
    , Annotation(..)
    , EntityAnnotation(..)
    , toAnnotations
    , toEntityAnnotations
    , prettyAnnotation
    , prettyEntityAnnotation
    , escapeSectionPath
    ) where

import Data.List (intercalate, nub, sort)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList as DList
import qualified Data.Text as T
import Data.Binary.Serialise.CBOR
import GHC.Generics

import Data.MediaWiki.Markup (PageName(..))
import CAR.Types
import CAR.Utils

-- General
newtype ParaNumber = ParaNumber Int -- Sequential index
                   deriving (Show)

-- Passage file
type PassageFile = [Paragraph]

-- Stub
data Stub = Stub { stubName     :: PageName
                 , stubPageId   :: PageId
                 , stubSkeleton :: [PageSkeleton] }
          deriving (Show, Generic)
instance Serialise Stub

-- Ground truth
data Relevance = Relevant | NonRelevant
               deriving (Show, Eq, Ord)

-- | A relevance annotation of a paragraph in a section
data Annotation = Annotation SectionPath ParagraphId Relevance
                deriving (Show, Eq, Ord)

data EntityAnnotation = EntityAnnotation SectionPath PageName Relevance
                deriving (Show, Eq, Ord)

escapeSectionPath :: SectionPath -> String
escapeSectionPath (SectionPath page headings) =
    intercalate "/" $ (unpackPageId page) : map unpackHeadingId headings

-- | In TREC @qrel@ format.
prettyAnnotation :: Annotation -> String
prettyAnnotation (Annotation sectionPath paraId rel) =
    unwords [ escapeSectionPath sectionPath
            , "0"
            , unpackParagraphId paraId
            , case rel of
                Relevant    -> "1"
                NonRelevant -> "0"
            ]
prettyEntityAnnotation :: EntityAnnotation -> String
prettyEntityAnnotation (EntityAnnotation sectionPath entityId rel) =
    unwords [ escapeSectionPath sectionPath
            , "0"
            , T.unpack $ getPageName entityId
            , case rel of
                Relevant    -> "1"
                NonRelevant -> "0"
            ]

toStubSkeleton :: Page -> Stub
toStubSkeleton (Page name pageId skeleton) =
    Stub name pageId (mapMaybe go skeleton)
  where
    go :: PageSkeleton -> Maybe PageSkeleton
    go (Section heading headingId children) =
        Just $ Section heading headingId (mapMaybe go children)
    go (Para _) = Nothing

prettyStub :: Stub -> String
prettyStub (Stub (PageName name) _ skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
           ++ map prettySkeleton skeleton

toParagraphs :: Page -> [Paragraph]
toParagraphs (Page name _ skeleton) =
    concatMap go skeleton
  where
    go :: PageSkeleton -> [Paragraph]
    go (Section _ _ children) = concatMap go children
    go (Para para) = [para]

toAnnotations :: Page -> [Annotation]
toAnnotations (Page _ pageId skeleton) =
    -- recurse into sections, recursively collect section path, emit one annotation per paragraph
    nub $ sort $ concatMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> [Annotation]
    go parentIds (Section _ sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
        in concatMap (go parentIds') children
    go parentIds (Para (Paragraph paraId body)) =
        [Annotation sectionPath paraId Relevant]
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)

toEntityAnnotations :: Page -> [EntityAnnotation]
toEntityAnnotations (Page _ pageId skeleton) =
    -- recurse into sections, recursively collect section path, emit one entity annotation per link
    nub $ sort $ concatMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> [EntityAnnotation]
    go parentIds (Section _ sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
        in concatMap (go parentIds') children
    go parentIds (Para paragraph) =
        let entities =  fmap fst $ paraLinks paragraph
        in [EntityAnnotation sectionPath entityId Relevant | entityId <- entities]
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
