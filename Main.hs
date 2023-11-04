module Main where

import System.Exit
import System.Process

{- 
Let's start by representing two basic scenarios using Monads.
    1. A linear chain of two processes where Process A runs before process B
    2. A linear chain of two processes where Process A runs before process B, but process A fails so B is never run.
-}

x :: IO ExitCode
x = createProcess (shell "echo 'Process A'") >>= \(_, _, _, processHandle) -> waitForProcess processHandle

--(_, _, _, processHandle) <- createProcess (shell "echo 'Process A'")
--exitCode <- waitForProcess processHandle
-- print exitCode
main :: IO ()
main = do
    code <- x
    print code