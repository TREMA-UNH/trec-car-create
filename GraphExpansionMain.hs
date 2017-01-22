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


import Control.Exception (evaluate)
import Control.DeepSeq
import Control.Monad (when, void)
import Control.Concurrent.Async
import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Coerce
import Data.List (intercalate)
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import qualified ExtractKnowledgeBase as KB

--import qualified Control.Concurrent.ForkMap as ForkMap
import WriteRanking
import Retrieve
import GraphExpansion

opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


data QueryDoc = QueryDoc { queryDocQueryId :: PageId
                         , queryDocQueryText :: PageName
                         , queryDocLeadEntities ::  HS.HashSet PageId
                         , queryDocRawTerms :: [Term]
                         }
           deriving Show

pagesToLeadEntities :: [Page] -> [QueryDoc]
pagesToLeadEntities pages =
        map (\page -> let kbDoc = KB.transformContent inlinkCounts page
                      in QueryDoc { queryDocQueryId        = KB.kbDocPageId kbDoc
                                  , queryDocQueryText      = pageName page
                                  , queryDocLeadEntities   = HS.fromList $ fmap pageNameToId $ KB.kbDocOutLinks kbDoc
                                  , queryDocRawTerms       = textToTokens' $ getPageName $ pageName page
                                  }
            )
        $ pages
      where
        inlinkInfo   = KB.collectInlinkInfo pages
        inlinkCounts = KB.resolveRedirects inlinkInfo

type Name = String

data GraphStats = GraphStats { nNodes, nEdges :: !Int }
                deriving (Show)

graphSize :: WHyperGraph Double -> GraphStats
graphSize graph =
    GraphStats { nNodes = HM.size graph
               , nEdges = getSum $ foldMap (\(OutWHyperEdges g) -> Sum $ HM.size g) graph
               }

-- ----------------------------------------------------------------------

data EdgeDocWithScores = EdgeDocWithScores { withScoreEdgeDoc   :: EdgeDoc
                                           , withScoreCount     :: Int
                                           , withScoreScore     :: Double
                                           , withScoreRecipRank :: Double
                                           }
           deriving (Show, Generic)
instance NFData EdgeDocWithScores



rankNormDocs :: RankingFunction -> Int -> Int -> [Term] -> [EdgeDoc] -> [EdgeDocWithScores]
rankNormDocs rankDocs normRank cutoffRank query edgeDocs =
    let rankedEdgeDocs :: [(EdgeDoc, Double)]
        rankedEdgeDocs = take cutoffRank
                         $ rankDocs query
                         $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent $ edgeDoc))
                         $ edgeDocs
        (_, normScore)
          | length rankedEdgeDocs > normRank  = rankedEdgeDocs !! normRank
          | not (null rankedEdgeDocs)         = last rankedEdgeDocs
          | otherwise                         = error "rankNormDocs: ranking empty"

        cutRankedEdgeDocs =  fmap (\(rank, (edgeDoc, score))  -> (EdgeDocWithScores edgeDoc 1 (exp (score - normScore)) (1.0/(realToFrac rank))))
                           $ zip [1::Int ..]
                           $ rankedEdgeDocs
    in cutRankedEdgeDocs


filterGraphByTop100GraphEdges :: RankingFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop100GraphEdges rankDocs query edgeDocs =
        let edges :: [EdgeDocWithScores]
            edges  = rankNormDocs rankDocs 100 100 query edgeDocs
        in HM.fromListWith (++) $ foldMap groupByEntity $ edges
  where groupByEntity :: EdgeDocWithScores -> [(PageId, [EdgeDocWithScores])]
        groupByEntity elem@(EdgeDocWithScores edgeDoc count score reciprank) =
                  [ (entity, [elem])
                  | entity <- edgeDocNeighbors $ edgeDoc]


filterGraphByTop5NodeEdges :: RankingFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop5NodeEdges rankDocs query edgeDocs =
  let perNodeEdges :: HM.HashMap PageId [EdgeDoc]
      perNodeEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
      perNodeFilteredEdges :: HM.HashMap PageId [EdgeDocWithScores]
      perNodeFilteredEdges =  fmap filterNode perNodeEdges
   in perNodeFilteredEdges


  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]
        filterNode :: [EdgeDoc] -> [EdgeDocWithScores]
        filterNode edgeDocs =
            rankNormDocs rankDocs 5 5 query edgeDocs


noFilter :: [EdgeDoc] ->  HM.HashMap PageId [EdgeDoc]
noFilter edgeDocs =
  let perNodeEdges :: HM.HashMap PageId [EdgeDoc]
      perNodeEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
  in perNodeEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]


noFilterTwice :: [EdgeDoc] ->  HM.HashMap PageId (HM.HashMap PageId [EdgeDoc])
noFilterTwice edgeDocs =
  let perSourceEdges :: HM.HashMap PageId [EdgeDoc]
      perSourceEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
      perTargetEdges = fmap (\edgeDocs' -> HM.fromListWith (++) $ foldMap groupByEntity edgeDocs' ) $ perSourceEdges
  in perTargetEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]



accumulateEdgeWeights :: forall w. Num w => HM.HashMap PageId [EdgeDocWithScores] -> (EdgeDocWithScores -> w) ->   HM.HashMap PageId (HM.HashMap PageId w)
accumulateEdgeWeights sourceToEdgeDocsWithScores by=
    HM.mapWithKey countEdgeDocs sourceToEdgeDocsWithScores
  where countEdgeDocs :: PageId -> [EdgeDocWithScores] -> HM.HashMap PageId w
        countEdgeDocs sourceNode edgeDocsWithScores =
            HM.fromListWith (+)
              [ (targetNode, by edgeDocsWithScore)
              | edgeDocsWithScore <- edgeDocsWithScores
--               | (EdgeDocWithScores edgeDoc count _ _) <- edgeDocsWithScores
              , targetNode <- edgeDocNeighbors $ withScoreEdgeDoc $ edgeDocsWithScore
              , targetNode /= sourceNode
              ]

-- ----------------------------------------------------------------------

data GraphNames = Top5PerNode | Top100PerGraph | SimpleGraph
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data WeightingNames = Count | Binary | Score | RecipRank
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data GraphRankingNames = PageRank | ShortPath
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data Method = Method GraphNames WeightingNames GraphRankingNames
    deriving ( Ord, Eq, Generic)
instance Show Method where
    show = showMethodName
showMethodName:: Method -> String
showMethodName (Method a b c) = intercalate "-" [show a, show b, show c]


instance NFData GraphNames
instance NFData WeightingNames
instance NFData GraphRankingNames


type RankingFunction = forall elem. [Term] -> [(elem, T.Text)] -> [(elem, Double)]

computeRankingsForQuery :: RankingFunction
                        -> QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                           -> [(Method, [(PageId, Double)])]

computeRankingsForQuery rankDocs queryDoc radius universeGraph binarySymmetricGraph =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        query =  queryDocRawTerms $ queryDoc

        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet


        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset = HS.toList $ HS.fromList $ concat $ HM.elems universeSubset

        fancyGraphs :: [(GraphNames, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = [(Top5PerNode,  filterGraphByTop5NodeEdges rankDocs query $ edgeDocsSubset)
                      ,(Top100PerGraph,  filterGraphByTop100GraphEdges rankDocs query $ edgeDocsSubset)
                      ]

        simpleGraphs ::  [(GraphNames, HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]
        simpleGraphs = [(SimpleGraph, noFilterTwice $ edgeDocsSubset)]

        weightings :: [(WeightingNames, EdgeDocWithScores -> Double)]
        weightings =  [ (Count,  realToFrac . withScoreCount)
                      , (Score, withScoreScore)
                      , (RecipRank, withScoreRecipRank)
                      ]

        graphRankings :: [(GraphRankingNames, (HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]))]
        graphRankings = [(PageRank, \graph ->  let wgraph = hypergraphToGraph graph
                                               in rankByPageRank wgraph 0.15 20)
                        ,(ShortPath, \graph -> let wgraph = hypergraphToGraph graph
                                               in rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce wgraph) (toList seeds))
                        ]

        fancyWeightedGraphs ::  [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        fancyWeightedGraphs =  [((gname, wname), accumulateEdgeWeights graph weighting)
                               | (gname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        simpleWeightedGraphs :: [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        simpleWeightedGraphs = concat [
                                         [ ((gname, Count), fmap (fmap (realToFrac . length) )  $ graph)
                                         , ((gname, Binary),      fmap (fmap (const 1))         $ graph)
                                         ]
                                      | (gname, graph) <- simpleGraphs]


        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname wname rname,  graphRanking (graph) )
            | ((gname, wname), graph) <- fancyWeightedGraphs ++ simpleWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ]

    in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'

main :: IO ()
main = do
    (articlesFile, queryFile, outputFilePrefix) <- execParser $ info (helper <*> opts) mempty
    pagesForLinkExtraction <- decodeCborList <$> BSL.readFile articlesFile

    let universeGraph :: UniverseGraph
        !universeGraph = edgeDocsToUniverseGraph $ emitEdgeDocs pagesForLinkExtraction

    let binarySymmetricGraph :: BinarySymmetricGraph
        !binarySymmetricGraph = universeToBinaryGraph universeGraph


    queriesToSeedEntities <- pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile

    let queryTermsAll = foldMap (queryDocRawTerms) $ queriesToSeedEntities
    putStrLn $ "# queryTermsAll " ++ show queryTermsAll

    let !corpusStatistics = Retrieve.computeTermCounts queryTermsAll
                          $ map (\edgeDoc -> Doc edgeDoc (edgeDocContent edgeDoc))
                          $ emitEdgeDocs pagesForLinkExtraction

    putStrLn $ "# corpus statistics " ++ show corpusStatistics

    let rankDoc q docs =
            map (\(Doc a b) -> (a,b))
            $ Retrieve.retrieve corpusStatistics q
            $ map (uncurry Doc) docs


    let allMethods = [ Method gName wName rName
                     | gName <- [minBound :: GraphNames .. maxBound]
                     , wName <- [minBound :: WeightingNames .. maxBound]
                     , rName <- [minBound :: GraphRankingNames .. maxBound]
                     ]

    let enumAll :: (Enum a, Bounded a) => [a]
        enumAll = [minBound .. maxBound]


    handles <- sequence $ M.fromList
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode)
      | method <- allMethods ]
        :: IO (M.Map Method Handle)

    let --forM_' = forM_
        forM_' = forConcurrently_
        --forM_' xs f = void $ runEffect $ ForkMap.mapIO 16 16 f xs
    forM_' queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

        T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
        let queryId = queryDocQueryId query
            rankings = computeRankingsForQuery rankDoc query 3
                                  universeGraph binarySymmetricGraph

            runMethod :: Method -> [(PageId, Double)] -> IO ()
            runMethod method ranking = do
                let hdl = handles M.! method
                let logMsg t = T.putStr $ T.pack $ unpackPageId queryId++"\t"++showMethodName method++"\t"++t++"\n"
                    logTimed t doIt = do
                        logMsg t
                        t0 <- getCurrentTime
                        !r <- doIt
                        t1 <- getCurrentTime
                        let dt = t1 `diffUTCTime` t0
                        logMsg $ t++"\ttime="++(showFFloat (Just 3) (realToFrac dt / 60 :: Double) "")
                        return r

                --logTimed "evaluating graph" $ evaluate $ rnf graph
                --logMsg $ "graph size: "++show (graphSize graph)
                --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
                logMsg $ "ranking entries="++show (length ranking)
                let formatted = WriteRanking.formatEntityRankings
                                (T.pack $ show method)
                                (T.pack $ unpackPageId queryId)
                                ranking
                logTimed "writing ranking" $ TL.hPutStr hdl formatted

        mapM_ (uncurry runMethod) rankings
    mapM_ hClose handles
