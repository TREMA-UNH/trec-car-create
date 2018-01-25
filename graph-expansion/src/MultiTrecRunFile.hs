{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MultiTrecRunFile where

import qualified Data.DList as DList
import Data.Foldable
import Data.Ord
import Data.List
import Data.Monoid
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified CAR.RunFile as RunFile
import CAR.Types


type Score = Double
type Rank = Int
type QueryId = RunFile.QueryId

-- data RankingEntry' doc = RankingEntry { carQueryId     :: !QueryId
--                                       , carDocument    :: doc
--                                       , carRank        :: !Int
--                                       , carScore       :: !Run.Score
--                                       , carMethodName  :: !MethodName
--                                       }
--                        deriving  (Show)

-- data RankingEntry = RankingEntry { queryId       :: !QueryId
--                                  , documentName  :: !DocumentName
--                                  , documentRank  :: !Rank
--                                  , documentScore :: !Score
--                                  , methodName    :: !MethodName
--                                  }
--                   deriving (Show)

type RankingEntry doc = RunFile.RankingEntry' doc -- CAR.RunFile definition

data MultiRankingEntry doc key =  MultiRankingEntry { multiRankingEntryCollapsed :: !(RankingEntry doc)
                                                    , multiRankingEntryAll       :: [(key, RankingEntry doc)]
                                                    }

multiRankingEntryGetDocumentName :: MultiRankingEntry doc key -> doc
multiRankingEntryGetDocumentName mre =  RunFile.carDocument $ multiRankingEntryCollapsed mre


multiAsRankingEntry :: MultiRankingEntry doc key -> RankingEntry doc
multiAsRankingEntry multiRankingEntry = multiRankingEntryCollapsed multiRankingEntry

-- Precondition: MethodNames need to be unique!

collapseRuns :: forall doc key . (Eq doc, Hashable doc, Hashable key) =>  [(key, [RankingEntry doc])]  -> M.Map QueryId [MultiRankingEntry doc key]
collapseRuns runs =
    let listOfRunMaps :: M.Map QueryId [(key, RankingEntry doc)]
        listOfRunMaps = fmap toList . RunFile.groupByQuery' $ [(key, elem) | (key, run) <- runs, elem <- run]

     in M.fromList $ [ (query, collapseRankings rankings)
                      | (query, rankings) <- M.toList listOfRunMaps  -- fetch rankings for query
                      ]


-- rankings: rank entries for the same query, across different run files
collapseRankings ::  forall doc key . (Eq doc, Hashable doc) => [(key, RankingEntry doc)] -> [MultiRankingEntry doc key]
collapseRankings rankingEntries =
    -- docid -> [ entries ]
    -- groupBy (docId) rankingEntries
    let groupByDoc :: [DList.DList (key, RankingEntry doc)]
        groupByDoc = map snd $ HM.toList -- equivalent for HM.values
                  $ HM.fromListWith (<>)
                  $ [ ((RunFile.carDocument entry), DList.singleton (key, entry) ) |  (key, entry) <- rankingEntries ]

        -- with score but not sorted
        scoredMultiRankings :: [ ( Score,  [(key, RankingEntry doc)] ) ]
        scoredMultiRankings = [ ( aggregatedScore rankingsPerDoc', rankingsPerDoc')
                              | rankingsPerDoc <- groupByDoc
                              , let rankingsPerDoc' = toList rankingsPerDoc
                              ]


        -- sorted and with ranks
    in zipWith buildData [1 .. ]
       $ sortOn (Down . fst) scoredMultiRankings

  where buildData :: Rank -> (Score, [(key, RankingEntry doc)]) -> MultiRankingEntry doc key
        buildData r (s, rankings) =  MultiRankingEntry {
                                    multiRankingEntryCollapsed =
                                        RunFile.RankingEntry { RunFile.carQueryId = qId
                                                             , RunFile.carDocument = docName
                                                             , RunFile.carRank = r
                                                             , RunFile.carScore = s
                                                             , RunFile.carMethodName = RunFile.MethodName "collapsed"
                                                             }
                                    , multiRankingEntryAll = rankings
                                    }
          where
            qId = RunFile.carQueryId $ snd $ head rankings
            docName = RunFile.carDocument $ snd $ head rankings



-- precondition: all RankingEntry share the same queryId
-- precondition2: all RankingEntry share the same documentName
aggregatedScore :: [(key, RankingEntry doc)] -> Score
aggregatedScore rankings =
    recipRankAggregation $ fmap  ( RunFile.carRank . snd) rankings
  where
    recipRankAggregation :: [Rank] -> Score
    recipRankAggregation rs =
        sum $ fmap recipRank rs
      where
          recipRank :: Int -> Double
          recipRank x | x < 0 = error $ "rank needs to be positive but is "++ (show x)
          recipRank r = 1.0/(realToFrac r+1)

--  Todo: compare or merge with MergeRankings.hs