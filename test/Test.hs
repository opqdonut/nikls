{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import Model

import Data.List
import Test.QuickCheck
import Test.QuickCheck.Property.Common
import Test.QuickCheck.Property.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified Data.Map.Strict as M

-- Model --

deriving instance Arbitrary Sum

prop_Sum_Monoid = eq $ prop_Monoid (T :: T Sum)

deriving instance Arbitrary Account
deriving instance Arbitrary Balances

prop_Balances_Monoid = eq $ prop_Monoid (T :: T Sum)

forAllElements [] f = property True
forAllElements xs f = forAll (elements xs) f

prop_balanceFor :: [Balances] -> Property
prop_balanceFor bs = forAllElements accounts $
    \a -> balanceFor a together == mconcat (map (balanceFor a) bs)
  where together = mconcat bs
        accounts = balancesAccounts together

sameElements :: Ord a => [a] -> [a] -> Bool
sameElements xs ys = sort xs == sort ys

prop_div_names :: Sum -> NonEmptyList Account -> Bool
prop_div_names sum (NonEmpty accounts') = balancesAccounts (sum /// accounts) `sameElements` accounts
  where accounts = nub accounts'

prop_div_total :: Sum -> NonEmptyList Account -> Bool
prop_div_total sum (NonEmpty accounts') = mconcat sums == sum
  where sums = M.elems $ unBalances $ sum /// accounts
        accounts = nub accounts'

prop_div_fair :: Sum -> NonEmptyList Account -> Bool
prop_div_fair sum (NonEmpty accounts') = maxSum <= minSum + 1
  where sums = map sumCents . M.elems . unBalances $ sum /// accounts
        minSum = minimum sums
        maxSum = maximum sums
        accounts = nub accounts'
        
  

-- Running --

tests = [ testProperty "Sum_Monoid" prop_Sum_Monoid
        , testProperty "Balances_Monoid" prop_Balances_Monoid
        , testProperty "balanceFor" prop_balanceFor
        , testProperty "div_names" prop_div_names
        , testProperty "div_total" prop_div_total
        , testProperty "div_fair" prop_div_fair ]

main = defaultMain tests
