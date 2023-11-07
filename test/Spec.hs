{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Parse (YamlStage(..), constructDAG, isCyclic)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

yamlcycle1 :: [YamlStage]
yamlcycle1 = [
     YamlStage {yamlName="a", yamlCmd="echo a > a.txt", yamlDeps=["b.txt"], yamlOuts=["a.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo b > b.txt", yamlDeps=["a.txt"], yamlOuts=["b.txt"]}    
    ]

yamldag1 :: [YamlStage]
yamldag1 = [
     YamlStage {yamlName="a", yamlCmd="echo a > a.txt", yamlDeps=[], yamlOuts=["a.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo b > b.txt", yamlDeps=["a.txt"], yamlOuts=["b.txt"]}    
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Detect a simple cycle" $
    let graph = constructDAG yamlcycle1
    in isCyclic graph @?= True

  , testCase "Simple DAG is not a cycle" $
    let graph = constructDAG yamldag1
    in isCyclic graph @?= False
  ]