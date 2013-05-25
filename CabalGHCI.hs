module Main where

import Distribution.Dev.Interactive

import System.Environment
import System.Process

opt0 opt args = f [] args
     where f r (x:xs)
             | x == ("--cabal-ghci-" ++ opt) = (reverse r ++ xs, True)
             | otherwise = f (x:r) xs
           f _ _ = (args, False)

main = do
  args ← getArgs
  let (args', pretend) = opt0 "print-ghci-args" args
  withOpts args' putStrLn $
    if pretend then mapM_ putStrLn
    else \opts → do
          putStrLn $ "Executing ghci with the following options: " ++ unwords opts
          rawSystem "ghci" opts >> return ()
