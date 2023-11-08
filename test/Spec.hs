{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertBool )

import Parse (YamlStage(..), constructDag, duplicateStageNames)

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

yamlDuplicateStageNames :: [YamlStage]
yamlDuplicateStageNames = [
     YamlStage {yamlName="a", yamlCmd="echo a > a.txt", yamlDeps=[], yamlOuts=["a.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo b > b.txt", yamlDeps=["c.txt"], yamlOuts=["b.txt"]}    
    ,YamlStage {yamlName="b", yamlCmd="echo c > c.txt", yamlDeps=["b.txt"], yamlOuts=["c.txt"]}    
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Detect a simple cycle" $
    case constructDag yamlcycle1 of
        Left _err -> assertBool "Expected no error" True
        Right _dag -> assertBool "Expected an error" False

  , testCase "Simple Dag is not a cycle" $
    case constructDag yamldag1 of
        Left _err -> assertBool "Expected no error" False
        Right _dag -> assertBool "Expected an error" True

  {-
  , testCase "Empty YAML is not a cycle" $
    let graph = constructDag yamlempty
    in isCyclic graph @?= False

  , testCase "Unconnected YAML" $
    let graph = constructDag yamlUnconnected
    in isCyclic graph @?= False

  , testCase "Unconnected YAML with a cycle" $
    let graph = constructDag yamlUnconnectedWithCycle
    in isCyclic graph @?= True
  -}

  , testCase "Found duplicate stage names" $
    duplicateStageNames yamlDuplicateStageNames @?= True
    
  , testCase "Confirmed no duplicate stage names" $
    duplicateStageNames yamlcycle1 @?= False

  , testCase "Confirmed no duplicate stage names" $
    duplicateStageNames yamldag1 @?= False

  , testCase "Confirmed no duplicate stage names" $
    duplicateStageNames yamlempty @?= False

  , testCase "Confirmed no duplicate stage names" $
    duplicateStageNames yamlUnconnected @?= False

  , testCase "Confirmed no duplicate stage names" $
    duplicateStageNames yamlUnconnectedWithCycle @?= False
  ]