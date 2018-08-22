{-# language RecordWildCards #-}

import qualified Data.DList as DList
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text as T

import Options.Applicative as Opts

import SimplIR.TREC.News as TREC
import SimplIR.TREC as TREC
import SimplIR.TREC as TREC
import SimplIR.Format.TrecRunFile as Run
import SimplIR.Ranking as Ranking

opts :: Opts.Parser (FilePath, FilePath, FilePath)
opts =
    (,,) <$> argument str (metavar "TOPICS" <> help "XML topic file")
         <*> argument str (metavar "RUN" <> help "Run file")
         <*> option str (short 'o' <> long "output" <> help "Output run file")

main :: IO ()
main = do
    (queryFile, runFile, outFile) <- execParser $ info (helper <*> opts) mempty
    let queryFile =  "/home/ben/trec-car/data/wapo/newsir18-entity-ranking-topics.xml"
        runFile = ""
        outFile = ""

    topics <- TREC.parseMany TREC.newsQuery <$> TLIO.readFile queryFile
    let knownEntities = M.fromList [ (TREC.entityLink entity, TREC.entityId entity)
                                   | topic <- topics
                                   , entity <- TREC.topicEntities topic
                                   ]

    rankings <- partitionRankings <$> Run.readRunFile runFile
    let rankings' :: M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score T.Text)
        rankings' = fmap (Ranking.mapMaybe isKnownDoc) rankings
        isKnownDoc = (`M.lookup` knownEntities)
    Run.writeRunFile outFile $ combineRankings rankings'

partitionRankings :: [Run.RankingEntry]
                  -> M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score Run.DocumentName)
partitionRankings entries =
    fmap (Ranking.fromList . DList.toList)
    $ M.fromListWith (<>)
      [ ( (Run.queryId ent, Run.methodName ent)
        , DList.singleton (Run.documentScore ent, Run.documentName ent)
        )
      | ent <- entries
      ]

combineRankings :: M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score Run.DocumentName)
                -> [Run.RankingEntry]
combineRankings entries =
    [ Run.RankingEntry {..}
    | ((queryId, methodName), ranking) <- M.toList entries
    , (documentRank, (documentScore, documentName))
          <- zip [1..] (Ranking.toSortedList ranking)
    ]
