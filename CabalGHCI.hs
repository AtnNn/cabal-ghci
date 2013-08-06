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
    withOpts args' errCont $
        if pretend then mapM_ putStrLn
        else \opts → do
          putStrLn $ "Executing ghci with the following options: " ++ unwords opts
          rawSystem "ghci" opts >> return ()
  where
    errCont "Current directory is not a cabal project" = do
          putStrLn "WARNING: no .cabal file found, falling back to default GHCi session & ignoring any passed args"
          rawSystem "ghci" []
          return ()
    errCont msg = putStrLn msg
