{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( Stage(..)
    , Dag(..)
    , readDag
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.YAML ( (.!=), (.:), (.:?), decode1, withMap, FromYAML(..), Pos )
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

data Stage = Stage
    { stageDeps :: [Text]
    , deps :: [Text] -- These are file paths
    , outs :: [Text] -- These are file paths
    , command  :: Text
    } deriving (Show, Eq)

-- A Dag is represented by a map from a stage name to a stage and its dependencies
newtype Dag = Dag (Map.Map Text Stage) deriving (Show)

constructDag :: [YamlStage] -> Either String Dag
constructDag yamlStages
    | duplicateStageNames yamlStages = Left "Error: Duplicate stage names."
    | isCyclic dag = Left "Error: Graph contains cycles."
    | otherwise = Right dag
      where
        dag :: Dag
        dag = Dag $ foldr addDependencies initialMap yamlStages
        -- Step 1: Initialize the Dag with stages without dependencies
        initialMap :: Map.Map Text Stage
        initialMap = Map.fromList $ map 
            (\ys -> (yamlName ys, Stage [] (yamlDeps ys) (yamlOuts ys) (yamlCmd ys)))
            yamlStages
        -- Step 2: For each YamlStage ys, find other stages that have any of the 'outs' of ys in their 'deps'
        addDependencies :: YamlStage -> Map.Map Text Stage -> Map.Map Text Stage
        addDependencies ys stageMap = foldr (addDependence (yamlName ys)) stageMap yamlStages
          where
            addDependence :: Text -> YamlStage -> Map.Map Text Stage -> Map.Map Text Stage
            addDependence sourceName ysTarget stageMapTarget =
                if any (`elem` yamlDeps ysTarget) (yamlOuts ys)
                -- If any of 'outs' is in 'deps' of the target, add the source as a dependency
                then Map.adjust 
                    (\(Stage sdeps deps' outs' cmd) -> Stage (sourceName : sdeps) deps' outs' cmd)
                    (yamlName ysTarget)
                    stageMapTarget
                else stageMapTarget

-- TODO I also need to check that there aren't duplicate stage names in the YAML
duplicateStageNames :: [YamlStage] -> Bool
duplicateStageNames stages =
    length stages /= Set.size (Set.fromList [yamlName stage | stage <- stages])

-- This could be optimized by keeping track of an overall set of visited nodes between calls to each DFS. But this is fine for now.
isCyclic :: Dag -> Bool
isCyclic (Dag dagMap) = any hasCycle (Map.keys dagMap)
  where
    hasCycle :: Text -> Bool
    hasCycle node = dfs Set.empty node

    dfs :: Set.Set Text -> Text -> Bool
    dfs visited node
        | node `Set.member` visited = True
        | otherwise =
            --let children = fromMaybe [] $ fmap snd (Map.lookup node dagMap)
            let children = maybe [] stageDeps (Map.lookup node dagMap)
                visited' = Set.insert node visited
            in any (dfs visited') children

-- TODO: Explore using liftIO, monad combinators, etc in order to effectly nest the Either values in the IO.
-- That might not actually be more readable than this sort of indented nesting below. I'm not sure what would be better.
-- Another option here would be to explore raising IO exceptions. This would mean no monad transformers are needed since the error handling would
-- be part of the IO monad.
readDag :: FilePath -> IO Dag
readDag filepath = do
    contents <- BL.readFile filepath
    let eitherStages = decode1 contents :: Either (Pos, String) [YamlStage]
    case eitherStages of
        Left err -> error $ "Failed to parse YAML: " ++ snd err
        Right stages -> do
            let eitherDag = constructDag stages
            case eitherDag of
                Left err -> error err
                Right dag -> return dag