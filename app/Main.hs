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
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as B

lockFileName :: FilePath
lockFileName = "lock.json"

gitHash :: FilePath -> IO Text
gitHash path = do
    gitOutput <- readProcess "git" ["hash-object", path] ""
    return ((T.strip . T.pack) gitOutput)

-- 
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


-- Given a stage name and a Dag, produces a Dag subset that consists only of dependencies of the specified stage and their dependencies
-- Just do a breadth-first search for this.
subDag :: Text -> Dag -> Dag
subDag target (Dag mapDag) = subDag' [target] (Dag Map.empty)
  where 
    getDeps :: Text -> [Text]
    getDeps stageName = maybe [] stageDeps (Map.lookup stageName mapDag)
    subDag' :: [Text] -> Dag -> Dag
    subDag' [] visited = visited
    subDag' deps' (Dag visited) = subDag' (concatMap getDeps deps') (Dag (Map.union depsMap visited))
      where
        depsMap = Map.fromList $ filter (\(k, _) -> k `elem` deps') $ Map.toList mapDag


-- Reads from file a mapping from filepaths to their git hash.
readLock :: FilePath -> IO (Map FilePath Text)
readLock lockPath = do
    exists <- doesFileExist lockPath
    if exists
        then readLock'
        else return Map.empty
      where
        readLock' :: IO (Map FilePath Text)
        readLock' = do
          jsonContent <- B.readFile lockFileName
          let lockMap = decode jsonContent :: Maybe (Map FilePath Text)
          case lockMap of
              Nothing -> return Map.empty
              Just m -> return m


writeLock :: Map FilePath Text -> IO ()
writeLock lockMap = do
    let bs = encode lockMap
    B.writeFile lockFileName bs


runStage :: Stage -> IO ExitCode
runStage stage = do
    let cmd = T.unpack command stage
    putStrLn $ cmd
    system $ cmd

runDag :: Text -> Dag -> Map FilePath Text -> IO ()
runDag target dag@(Dag mapDag) lockMap = runDag' orderedStages
  where
    orderedStageNames = tSort $ subDag target dag -- A topologically sorted list of stages that the target depends on
    orderedStages = catMaybes $ map (`Map.lookup` mapDag) orderedStageNames
    runStages :: [Stage] -> IO ()
    runStages [] = return ()
    runStages (stage:rest) = do
        exitCode <- runStage 
        case exitCode of 
            ExitSuccess -> runStages rest
            ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

    {-- Need to incorporate the below logic into runDag/runStage 
    runStage :: Text -> IO ()
    runStage stageName = do
        let maybeStage = Map.lookup stageName mapDag
        depHashes <- case maybeStage of
            Nothing -> error "Stage not in DAG"
            Just stage -> sequence $ Map.fromList $ map (\depFilePath -> (T.unpack depFilePath, gitHash $ T.unpack depFilePath)) (deps stage) -- Need to calculate the hashes of these deps and then compare them to hashes read from lockfile. For now we'll use FilePaths = [String] but need to circle back around to more efficient ways of doing this.
        let shouldRunStage = Map.union lockMap depHashes /= depHashes -- If the subset of lockMap that contains just depHashes keys is not the same as depHashes, then we need to run the stage
        -- Also need shouldRunStage to be based on a check of the outputs of the stage
        if shouldRunStage
            then let cmd = T.unpack $ command stage
    --}

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
    lockMap <- readLock lockFileName
    print $ "lockMap: " <> show lockMap
    print $ "DAG: " <> show dag
    print $ "Stage 3 subdag: " <> show (subDag (T.pack "stage3") dag)
    let orderedStages = tSort dag
    print $ "orderedStages: " <> show orderedStages
    let orderedCommands = getOrderedCommands dag orderedStages
    print $ "orderedCommands: " <> show orderedCommands
    let filePaths = gatherFilePaths dag orderedStages
    print $ "deps: "  <> show filePaths
    runCommands orderedCommands