module Main where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

prop_True = True

tests = [ testProperty "True" prop_True ]

main = defaultMain tests
