{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( readDAG
    ) where

import qualified Data.Map as Map
import Data.YAML
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

data YamlStage = YamlStage
    { yamlName :: T.Text
    , yamlCmd  :: T.Text
    , yamlDeps :: [T.Text]
    , yamlOuts :: [T.Text]
    } deriving Show

instance FromYAML YamlStage where
   parseYAML = withMap "YamlStage" $ \m -> YamlStage
       <$> m .: "name"
       <*> m .: "cmd"
       <*> m .:? "deps" .!= []
       <*> m .:? "outs" .!= []

data Stage = Stage {
  name :: T.Text,
  cmd  :: T.Text
} deriving (Show, Eq, Ord)

-- A DAG is represented by a map from a stage name to a stage and its dependencies
newtype DAG = DAG (Map.Map T.Text (Stage, [T.Text])) deriving (Show)

emptyDAG :: DAG
emptyDAG = DAG Map.empty

constructDAG :: [YamlStage] -> DAG
constructDAG stages = DAG $ Map.fromList $ map (\stage -> (yamlName stage, (Stage (yamlName stage) (yamlCmd stage), yamlDeps stage))) stages 

readDAG :: FilePath -> IO DAG
readDAG filepath = do
    contents <- BL.readFile filepath
    let eitherStages = decode1 contents :: Either (Pos, String) [YamlStage]
    case eitherStages of
        Left err -> error $ "Failed to parse YAML: " ++ snd err
        Right stages -> return $ constructDAG stages