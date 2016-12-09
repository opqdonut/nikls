module Main where

import Test.QuickCheck
import System.Exit

main = do
  r <- quickCheckResult True
  case r of
    Success{} -> putStrLn "OK"
    _ -> putStrLn "FAIL" >> exitFailure

