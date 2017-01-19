{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module GraphExpansion where

import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Maybe
import Data.Bifunctor

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq

import Dijkstra
import PageRank
import CAR.Utils
import CAR.Types

data EdgeDoc = EdgeDoc { edgeDocParagraphId     :: ParagraphId
                       , edgeDocArticleId       :: PageId
                       , edgeDocSourceEntityId  :: PageId
                       , edgeDocOutlinkIds      :: [PageId]
                       }
           deriving Show


transformContent :: Page -> [EdgeDoc]
transformContent (Page pageName' pageId pageSkeleta) =
    foldMap go pageSkeleta
  where
    pageName = normTargetPageName pageName'
    go :: PageSkeleton -> [EdgeDoc]
    go (Section heading _ children) =
       concatMap go  children
    go (Para paragraph) =
      [convertPara paragraph ]

    convertPara :: Paragraph -> EdgeDoc
    convertPara paragraph =
      let
        edgeDocParagraphId    = paraId $ paragraph
        edgeDocArticleId      = pageId
        edgeDocSourceEntityId = pageNameToId pageName
        edgeDocOutlinks       = fmap (first normTargetPageName) $ paraLinks $ paragraph
        edgeDocOutlinkIds     = fmap (pageNameToId . fst) $ edgeDocOutlinks
      in EdgeDoc {..}


dropEdgeDocsNoLinks :: [EdgeDoc] -> [EdgeDoc]
dropEdgeDocsNoLinks =
    filter (\edgeDoc -> not ( null (edgeDocOutlinkIds $edgeDoc)))

hashEdgeNodes :: EdgeDoc -> [(PageId, [EdgeDoc])]
hashEdgeNodes edgeDoc =
  [(edgeDocSourceEntityId $edgeDoc, [edgeDoc])] ++ [(target, [edgeDoc])  | target <- edgeDocOutlinkIds $edgeDoc]

type UniverseGraph = HM.HashMap PageId [EdgeDoc]
hashUniverseGraph :: [Page] -> UniverseGraph
hashUniverseGraph pages = HM.fromListWith (++) $ foldMap hashEdgeNodes
      $ foldMap ( dropEdgeDocsNoLinks . transformContent)
      $ pages


-- ------------------------------------------------
type BinarySymmetricGraph = HM.HashMap PageId (HS.HashSet PageId)

universeToBinaryGraph :: UniverseGraph -> BinarySymmetricGraph
universeToBinaryGraph universeGraph =
  fmap (foldMap (\edgeDoc -> HS.fromList $ [edgeDocSourceEntityId $ edgeDoc] ++ (edgeDocOutlinkIds $edgeDoc))) $ universeGraph


expandNodes :: BinarySymmetricGraph -> HS.HashSet PageId -> HS.HashSet PageId
expandNodes binarySymmetricGraph seeds =
  seeds <> foldMap (fromMaybe mempty . (`HM.lookup` binarySymmetricGraph)) seeds

expandNodesK binarySymmetricGraph seeds k =
  iterate (expandNodes binarySymmetricGraph) seeds !! k



-- ------------------------------------------------

-- | Outward weighted hyper-edges
newtype OutWHyperEdges weight = OutWHyperEdges (HM.HashMap PageId weight)     -- ^ a set of outward wHyperEdges and their weights
        deriving (Show, Functor)
data WHyperEdges weight = WHyperEdges PageId (OutWHyperEdges weight) -- ^ sourceNode and its outward wHyperEdges
        deriving (Show, Functor)


singleWHyperEdge :: Num weight => PageId -> OutWHyperEdges weight
singleWHyperEdge target = OutWHyperEdges $ HM.singleton target 1

type WHyperGraph weight = HM.HashMap PageId (OutWHyperEdges weight)

instance Num weight => Monoid (OutWHyperEdges weight) where
    mempty = OutWHyperEdges mempty
    OutWHyperEdges a `mappend` OutWHyperEdges b = OutWHyperEdges (HM.unionWith (+) a b)



countEdges :: (Num weight) => [EdgeDoc] -> OutWHyperEdges weight
countEdges edgeDocs =
      foldMap (singleWHyperEdge . edgeDocSourceEntityId) edgeDocs
   <> foldMap (foldMap singleWHyperEdge . edgeDocOutlinkIds) edgeDocs


lookupNeighbors :: Monoid v =>  HM.HashMap PageId v -> PageId -> v
lookupNeighbors graph node =
    fromMaybe mempty $ HM.lookup node graph


subsetOfUniverseGraph :: UniverseGraph -> HS.HashSet PageId -> UniverseGraph
subsetOfUniverseGraph universe nodeset =
    foldMap (\node -> HM.singleton node (map pruneEdges $ universe `lookupNeighbors` node) ) $ nodeset
  where
    -- Throw out neighbors not in our subgraph
    pruneEdges :: EdgeDoc -> EdgeDoc
    pruneEdges edoc = edoc { edgeDocOutlinkIds = filter (`HS.member` nodeset) (edgeDocOutlinkIds edoc) }

rankByPageRank :: Graph PageId Double -> Double -> Int -> [(PageId, Double)]
rankByPageRank graph teleport iterations =
  let pr = (!! iterations)  $ PageRank.pageRank teleport graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking


rankByShortestPaths :: Dijkstra.Graph PageId (Sum Double) -> [PageId] -> [(PageId, Double)]
rankByShortestPaths graph seeds =
    let shortestPaths =  [ (n1, n2, Dijkstra.shortestPaths paths n2)
                         | n1 <- toList seeds
                         , let paths = Dijkstra.dijkstra (graph) n1
                         , n2 <- toList seeds
                         ]

        pathRanking = shortestPathsToNodeScores shortestPaths
    in pathRanking

takeMiddle :: Seq.Seq a -> Seq.Seq a
takeMiddle s =
    case Seq.viewl s of
      Seq.EmptyL  -> mempty
      _ Seq.:< s' ->
       case Seq.viewr s' of
         Seq.EmptyR   -> mempty
         s'' Seq.:> _ -> s''

shortestPathsToNodeScores :: [(PageId, PageId, [Dijkstra.Path PageId])] -> [(PageId, Double)]
shortestPathsToNodeScores paths =
    let innerPaths :: [Seq.Seq PageId]
        innerPaths = fmap takeMiddle [ Seq.fromList path
                                     | (_, _, pathList) <- paths
                                     , path <- pathList
                                     ]

        numPaths = realToFrac $ length innerPaths
    in HM.toList
     $ HM.fromListWith (+) [ (elem, 1 / numPaths)
                           | path <- innerPaths
                           , elem <- toList path
                           ]

wHyperGraphToGraph :: WHyperGraph Double -> Graph PageId Double
wHyperGraphToGraph =
    Graph . fmap (\(OutWHyperEdges x) -> fmap (fmap $ recip . realToFrac) $ HM.toList x)


