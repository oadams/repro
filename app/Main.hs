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

{-
-- Then need to execute them in order, failing fast if any steps fail
exampleCommands :: [(String, String, [String])]
exampleCommands = [("A", "echo", ["first step"]),
                   ("B", "false", []),
                   ("C", "echo", ["C", "A"])]
-}


-- I should construct the graph from YAML in Parse.hs but then in this module define isCyclic and then just call isCyclic
-- when topologicalSort is called. That way topologicalSort never has to assume isCyclic has already been called. An 
-- alternative option would be to define both a Graph type and a DAG type, with some sort of confirmCyclic :: Graph -> Maybe DAG
-- and topologicalSort :: DAG -> [Stage], but that is probably not as good as topologicalSort :: DAG -> Maybe [Stage]
-- Or perhaps better still: Leave isCyclic in Parse.hs with constructDAG, and leave the DAG type as is, but make constructDAG
-- return a Maybe DAG or an Either String DAG and have constructDAG call isCyclic. That way, any DAG value is actually already confirmed to be a DAG
-- and doesn't rely on us checking it separately.

{-
topologicalSort :: DAG -> [Stage]
topologicalSort (DAG dagMap) = go initialSources Set.empty [] dagMap
  where
    -- Find all nodes that have no incoming edges
    initialSources = Map.keysSet $ Map.filter (null . snd) dagMap --Can I refactor the data structures so that instead of snd here I can talk about deps?

    go :: Set.Set Text -> Set.Set Text -> [Stage] -> Map.Map Text (Stage, [Text]) -> [Stage]
    go sources visited sorted graph
      | Set.null sources = reverse sorted -- If there are no sources left, we're done
      | otherwise = 
          let -- Pick a source and remove it from sources
              (source, remainingSources) = Set.deleteFindMin sources
              -- Mark it as visited
              visited' = Set.insert source visited
              -- Find the stage associated with this source
              Just (stage, _) = Map.lookup source graph
              -- Add it to the sorted list
              sorted' = stage : sorted
              -- Update the graph: reduce the incoming edge count for the neighbors
              -- If any neighbors become sources (no incoming edges), add them to sources
              graph' = Map.mapWithKey updateNeighbors graph
              newSources = foldr Set.insert remainingSources $ Map.keysSet $ Map.filter (null . snd) graph'
          in go newSources visited' sorted' graph'
      where 
        updateNeighbors :: Text -> (Stage, [Text]) -> (Stage, [Text])
        updateNeighbors neighbor (st, deps)
          | neighbor `Set.member` sources = (st, deps)
          | otherwise = (st, filter (`Set.notMember` sources) deps)
-}

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

{-
runCommands :: [(String, String, [String])] -> IO ()
runCommands [] = return ()
runCommands ((stage, cmd, args):rest) = do
    putStrLn $ "Running command: " ++ cmd ++ " " ++ unwords args
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitSuccess -> runCommands rest
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd
-}

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
