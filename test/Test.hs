{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import Model

import Test.QuickCheck
import Test.QuickCheck.Property.Common
import Test.QuickCheck.Property.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2

-- Model --

deriving instance Arbitrary Sum

prop_Sum_Monoid = eq $ prop_Monoid (T :: T Sum)

deriving instance Arbitrary Account
deriving instance Arbitrary Balances

prop_Balances_Monoid = eq $ prop_Monoid (T :: T Sum)

-- Running --

tests = [ testProperty "Sum_Monoid" prop_Sum_Monoid
        , testProperty "Balances_Monoid" prop_Balances_Monoid ]

main = defaultMain tests
