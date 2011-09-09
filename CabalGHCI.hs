module Main where

import Distribution.Dev.Interactive

import System.Environment
import System.Process

main = do
  args ← getArgs
  withOpts args putStrLn $ 
        \opts → do
          putStrLn $ "Executing ghci with the following options: " ++ unwords opts
          rawSystem "ghci" opts >> return ()
