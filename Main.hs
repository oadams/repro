module Main where

import System.Exit
import System.Process

import Control.Monad.Trans.Maybe

-- A function to run a shell command and return its exit code as Maybe
runShellCommand :: String -> IO (Maybe ExitCode)
runShellCommand cmd = do
    (_, _, _, processHandle) <- createProcess (shell cmd)
    waitForProcess processHandle

{- 
Let's start by representing two basic scenarios using Monads.
    1. A linear chain of two processes where Process A runs before process B
    2. A linear chain of two processes where Process A runs before process B, but process A fails so B is never run.
Note for this starting point we don't care about determining whether a dependency needs to run or not. We're just interested
in:
    (a) making the dependencies run first
    (b) stopping the pipeline if one of the dependencies fails.
-}

maybeSuccess :: IO ExitCode -> Maybe IO ExitCode


x :: IO ExitCode
x = createProcess (shell "echo 'Process A'") >>= \(_, _, _, processHandle) -> waitForProcess processHandle

--(_, _, _, processHandle) <- createProcess (shell "echo 'Process A'")
--exitCode <- waitForProcess processHandle
-- print exitCode
main :: IO ()
main = do
    code <- x
    print code