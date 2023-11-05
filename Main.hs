import qualified Data.Map as Map
import System.Exit
import System.Process

-- Need to implement function to detect cycles
--TODO

-- Then need to sort them topologically
--type Stage = String
--type Command = String
--topologicalSort :: Map.Map Stage [(Command, [Stage])] -> Stage -> [(Command, Stage)]

-- Then need to execute them in order, failing fast if any steps fail
exampleCommands :: [(String, String, [String])]
exampleCommands = [("A", "echo", ["first step"]),
                   ("B", "false", []),
                   ("C", "echo", ["C", "A"])]

runCommands :: [(String, String, [String])] -> IO ()
runCommands [] = return ()
runCommands ((stage, cmd, args):rest) = do
    putStrLn $ "Running command: " ++ cmd ++ " " ++ unwords args
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitSuccess -> runCommands rest
        ExitFailure _ -> putStrLn $ "Command failed: " ++ cmd

main :: IO ()
main = runCommands exampleCommands
