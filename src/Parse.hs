{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( someFunc
    ) where

import Data.YAML
import qualified Data.Text as T

data Stage = Stage
    { cmd  :: T.Text
    , deps :: [T.Text]
    , outs :: [T.Text]
    } deriving Show

instance FromYAML Stage where
   parseYAML = withMap "Stage" $ \m -> Stage
       <$> m .: "cmd"
       <*> m .: "deps" .!= []
       <*> m .:? "outs" .!= []

someFunc :: IO ()
someFunc = putStrLn "someFunc"