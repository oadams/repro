{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( readDAG
    , DAG
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.YAML
    ( (.!=), (.:), (.:?), decode1, withMap, FromYAML(..), Pos )
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL

data YamlStage = YamlStage
    { yamlName :: Text
    , yamlCmd  :: Text
    , yamlDeps :: [Text]
    , yamlOuts :: [Text]
    } deriving Show

instance FromYAML YamlStage where
   parseYAML = withMap "YamlStage" $ \m -> YamlStage
       <$> m .: "name"
       <*> m .: "cmd"
       <*> m .:? "deps" .!= []
       <*> m .:? "outs" .!= []

data Stage = Stage {
  name :: Text,
  cmd  :: Text
} deriving (Show, Eq, Ord)

-- A DAG is represented by a map from a stage name to a stage and its dependencies
newtype DAG = DAG (Map.Map Text (Stage, [Text])) deriving (Show)

-- Adjusted constructDag to consider outs and deps for building the DAG
constructDAG :: [YamlStage] -> DAG
constructDAG yamlStages = DAG $ foldr addDependencies initialMap yamlStages
  where
    -- Step 1: Initialize the DAG with stages without dependencies
    initialMap :: Map.Map Text (Stage, [Text])
    initialMap = Map.fromList $ map (\ys -> (yamlName ys, (Stage (yamlName ys) (yamlCmd ys), []))) yamlStages

    -- Step 2: For each YamlStage, find stages that have any of its 'outs' in their 'deps'
    addDependencies :: YamlStage -> Map.Map Text (Stage, [Text]) -> Map.Map Text (Stage, [Text])
    addDependencies ys stageMap = foldr (addDependence (yamlName ys)) stageMap yamlStages
      where
        addDependence :: Text -> YamlStage -> Map.Map Text (Stage, [Text]) -> Map.Map Text (Stage, [Text])
        addDependence sourceName ysTarget stageMapTarget =
          if any (`elem` yamlDeps ysTarget) (yamlOuts ys)
             -- If any of 'outs' is in 'deps' of the target, add the source as a dependency
             then Map.adjust (\(stage, deps) -> (stage, sourceName : deps)) (yamlName ysTarget) stageMapTarget
             else stageMapTarget


-- This could be optimized by keeping track of an overall set of visited nodes between calls to each DFS. But this is fine for now.
isCyclic :: DAG -> Bool
isCyclic (DAG dagMap) = any hasCycle (Map.keys dagMap)
  where
    hasCycle :: Text -> Bool
    hasCycle node = dfs Set.empty node

    dfs :: Set.Set Text -> Text -> Bool
    dfs visited node
      | node `Set.member` visited = True
      | otherwise =
          --let children = fromMaybe [] $ fmap snd (Map.lookup node dagMap)
          let children = maybe [] snd (Map.lookup node dagMap)
              visited' = Set.insert node visited
          in any (dfs visited') children


readDAG :: FilePath -> IO DAG
readDAG filepath = do
  contents <- BL.readFile filepath
  let eitherStages = decode1 contents :: Either (Pos, String) [YamlStage]
  case eitherStages of
    Left err -> error $ "Failed to parse YAML: " ++ snd err
    Right stages -> do
      let dag = constructDAG stages
      return $ if isCyclic dag then error "Cycle detected!" else dag