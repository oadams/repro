{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( readDAG
    , DAG
    ) where

import qualified Data.Map as Map
import Data.YAML
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

readDAG :: FilePath -> IO DAG
readDAG filepath = do
    contents <- BL.readFile filepath
    let eitherStages = decode1 contents :: Either (Pos, String) [YamlStage]
    case eitherStages of
        Left err -> error $ "Failed to parse YAML: " ++ snd err
        Right stages -> return $ constructDAG stages