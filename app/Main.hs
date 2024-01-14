module Main (main) where

import Parse ( readDag , Dag(..) , Stage(..) )

import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as Set
import System.Directory (doesFileExist)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.Process


gitHash :: FilePath -> IO Text
gitHash path = do
    gitOutput <- readProcess "git" ["hash-object", path] ""
    return ((T.strip . T.pack) gitOutput)

gatherFilePaths :: Dag -> [Text] -> [Text]
gatherFilePaths (Dag mapDag) orderedStages = concatMap stageDeps orderedStages'
  where
    orderedStages' = catMaybes $ map (`Map.lookup` mapDag) orderedStages
    

-- Need a function that takes in some dependency specified in the dag and:
-- (a) determines if it is a file
-- (b) if so computes its hash
-- (c) if it is a directory then it must handle the recursive application to all
-- the files in that director
--hashDep :: FilePath -> IO String
--hashDep path
--    | doesFileExist path = gitHash path


-- For a topological sort:
-- Find the nodes that don't have any dependencies
-- Put them in the sorted list
-- Remove dependencies to those nodes from other nodes in the graph
tSort :: Dag -> [Text]
tSort dag = go initialSources [] ndag
  where
    ndag = nameDag dag
    initialSources = Map.keysSet $ Map.filter null ndag
    go :: Set.Set Text -> [Text] -> Map Text [Text] -> [Text]
    go sources sorted graph
        | Set.null sources = reverse sorted
        | otherwise =
            let (source, remainingSources) = Set.deleteFindMin sources
                sorted' = source : sorted
                 -- Remove references to this node from any dependencies
                graph' = Map.map (filter (/= source)) graph
                 -- Remove this node from the graph entirely
                graph'' = Map.delete source graph'
                sources' = Set.union
                    remainingSources
                    (Map.keysSet $ Map.filter null graph'')
            in go sources' sorted' graph''

-- Takes a Dag and reduces it to just a mapping from names of stages to the
-- names of the dependent stages
nameDag :: Dag -> Map Text [Text]
nameDag (Dag mapDag) = Map.map stageDeps mapDag

getOrderedCommands :: Dag -> [Text] -> [Text]
getOrderedCommands (Dag mapDag) orderedStages =
    let values = catMaybes $ map (`Map.lookup` mapDag) orderedStages
    in [command val | val <- values]

runCommands :: [Text] -> IO ()
runCommands [] = return ()
runCommands (cmdtext:rest) = do
    let cmd = T.unpack cmdtext
    putStrLn $ cmd
    exitCode <- system $ cmd
    case exitCode of 
        ExitSuccess -> runCommands rest
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

main :: IO ()
main = do
    dag <- Parse.readDag "repro.yaml"
    print $ "DAG: " <> show dag
    let orderedStages = tSort dag
    print $ "orderedStages: " <> show orderedStages
    let orderedCommands = getOrderedCommands dag orderedStages
    print $ "orderedCommands: " <> show orderedCommands
    let filePaths = gatherFilePaths dag orderedStages
    print $ "deps: "  <> show filePaths
    runCommands orderedCommands