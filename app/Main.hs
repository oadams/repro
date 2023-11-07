module Main (main) where

import Parse ( readDAG
             , DAG(..)
             , Stage(..)
             )

import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Set as Set
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.Process

-- I should construct the graph from YAML in Parse.hs but then in this module define isCyclic and then just call isCyclic
-- when topologicalSort is called. That way topologicalSort never has to assume isCyclic has already been called. An 
-- alternative option would be to define both a Graph type and a DAG type, with some sort of confirmCyclic :: Graph -> Maybe DAG
-- and topologicalSort :: DAG -> [Stage], but that is probably not as good as topologicalSort :: DAG -> Maybe [Stage]
-- Or perhaps better still: Leave isCyclic in Parse.hs with constructDAG, and leave the DAG type as is, but make constructDAG
-- return a Maybe DAG or an Either String DAG and have constructDAG call isCyclic. That way, any DAG value is actually already confirmed to be a DAG
-- and doesn't rely on us checking it separately.

-- For a topological sort:
-- Find the nodes that don't have any dependencies
-- Put them in the sorted list
-- Remove dependencies to those nodes from other nodes in the graph
tSort :: DAG -> [Text]
tSort dag = go initialSources [] ndag
  where
    ndag = nameDag dag
    initialSources = Map.keysSet $ Map.filter null ndag
    go :: Set.Set Text -> [Text] -> Map.Map Text [Text] -> [Text]
    go sources sorted graph
        | Set.null sources = reverse sorted
        | otherwise =
            let (source, remainingSources) = Set.deleteFindMin sources
                sorted' = source : sorted
                graph' = Map.map (filter (/= source)) graph -- Remove references to this node from any dependencies
                graph'' = Map.delete source graph' -- Remove this node from the graph entirely
                sources' = Set.union remainingSources (Map.keysSet $ Map.filter null graph'')
            in go sources' sorted' graph''

-- Takes a DAG and reduces it to just a mapping from names of stages to the names of the dependent stages
nameDag :: DAG -> Map.Map Text [Text]
nameDag (DAG mapDag) = Map.map getDeps mapDag
  where
    getDeps :: (Stage, [Text]) -> [Text]
    getDeps (_, deps) = deps

getOrderedCommands :: DAG -> [Text] -> [Text]
getOrderedCommands (DAG mapDAG) orderedStages =
    let values = catMaybes $ map (`Map.lookup` mapDAG) orderedStages
    in [command $ fst val | val <- values]

runCommands :: [Text] -> IO()
runCommands [] = return ()
runCommands (cmdtext:rest) = do
    let cmd = unpack cmdtext
    putStrLn $ cmd
    exitCode <- system $ cmd
    case exitCode of 
        ExitSuccess -> runCommands rest
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

main :: IO ()
main = do
    dag <- Parse.readDAG "repro.yaml"
    print dag
    let orderedStages = tSort dag
    print orderedStages
    let orderedCommands = getOrderedCommands dag orderedStages
    print orderedCommands
    runCommands orderedCommands