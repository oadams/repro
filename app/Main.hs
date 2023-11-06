module Main (main) where

import Parse ( readDAG
             , DAG(..)
             , Stage
             )

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Set as Set
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.Process

-- Need to implement function to detect cycles
--TODO

-- Then need to sort them topologically
--type Stage = String
--type Command = String
--topologicalSort :: Map.Map Stage [(Command, [Stage])] -> Stage -> [(Command, Stage)]

-- Then need to execute them in order, failing fast if any steps fail
exampleCommands :: [(String, String, [String])]
exampleCommands = [("A", "echo", ["first step"]),
                   ("B", "false", []),
                   ("C", "echo", ["C", "A"])]

topologicalSort :: DAG -> [Stage]
topologicalSort (DAG dagMap) = go initialSources Set.empty [] dagMap
  where
    -- Find all nodes that have no incoming edges
    initialSources = Map.keysSet $ Map.filter (null . snd) dagMap

    go :: Set.Set Text -> Set.Set Text -> [Stage] -> Map.Map Text (Stage, [Text]) -> [Stage]
    go sources visited sorted graph
      | Set.null sources = reverse sorted  -- If there are no sources left, we're done
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

    updateNeighbors :: Text -> (Stage, [Text]) -> (Stage, [Text])
    updateNeighbors neighbor (st, deps)
      | neighbor `Set.member` sources = (st, deps)
      | otherwise = (st, filter (`Set.notMember` sources) deps)

runCommands :: [(String, String, [String])] -> IO ()
runCommands [] = return ()
runCommands ((stage, cmd, args):rest) = do
    putStrLn $ "Running command: " ++ cmd ++ " " ++ unwords args
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitSuccess -> runCommands rest
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

main :: IO ()
main = do
    dag <- Parse.readDAG "repro.yaml"
    print dag
    runCommands exampleCommands
