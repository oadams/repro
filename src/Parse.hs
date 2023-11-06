{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( someFunc
    ) where

import Data.YAML
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

data Stage = Stage
    { name :: T.Text
    , cmd  :: T.Text
    , deps :: [T.Text]
    , outs :: [T.Text]
    } deriving Show

instance FromYAML Stage where
   parseYAML = withMap "Stage" $ \m -> Stage
       <$> m .: "name"
       <*> m .: "cmd"
       <*> m .:? "deps" .!= []
       <*> m .:? "outs" .!= []

someFunc :: IO ()
someFunc = do
    contents <- BL.readFile "repro.yaml"
    let decoded = decode1 contents :: Either (Pos,String) [Stage]
    case decoded of
        Left err -> putStrLn $ "An error occurred: " ++ show err
        Right stages -> do
            -- Process your stages here
            print stages