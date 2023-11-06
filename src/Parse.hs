{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( someFunc
    ) where

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
       <$> m .: "yamlName"
       <*> m .: "yamlCmd"
       <*> m .:? "yamlDeps" .!= []
       <*> m .:? "yamlOuts" .!= []

data Stage = Stage { name :: T.Text
                   , cmd  :: T.Text
                   , deps :: [Stage]
                   } deriving Show

someFunc :: IO ()
someFunc = do
    contents <- BL.readFile "repro.yaml"
    let decoded = decode1 contents :: Either (Pos,String) [YamlStage]
    case decoded of
        Left err -> putStrLn $ "An error occurred: " ++ show err
        Right stages -> do
            -- Process your stages here
            print stages