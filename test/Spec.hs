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

yamlempty :: [YamlStage]
yamlempty = []

yamlUnconnected :: [YamlStage]
yamlUnconnected = [
     YamlStage {yamlName="a", yamlCmd="echo a > a.txt", yamlDeps=[], yamlOuts=["a.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo b > b.txt", yamlDeps=[], yamlOuts=["b.txt"]}    
    ]

yamlUnconnectedWithCycle :: [YamlStage]
yamlUnconnectedWithCycle = [
     YamlStage {yamlName="a", yamlCmd="echo a > a.txt", yamlDeps=[], yamlOuts=["a.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo b > b.txt", yamlDeps=["c.txt"], yamlOuts=["b.txt"]}    
    ,YamlStage {yamlName="c", yamlCmd="echo c > c.txt", yamlDeps=["b.txt"], yamlOuts=["c.txt"]}    
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Detect a simple cycle" $
    let graph = constructDAG yamlcycle1
    in isCyclic graph @?= True

  , testCase "Simple DAG is not a cycle" $
    let graph = constructDAG yamldag1
    in isCyclic graph @?= False

  , testCase "Empty YAML is not a cycle" $
    let graph = constructDAG yamlempty
    in isCyclic graph @?= False

  , testCase "Unconnected YAML" $
    let graph = constructDAG yamlUnconnected
    in isCyclic graph @?= False

  , testCase "Unconnected YAML with a cycle" $
    let graph = constructDAG yamlUnconnectedWithCycle
    in isCyclic graph @?= True
  ]