{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import Model

import Data.List
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified Data.Map.Strict as M

-- Utilities --

sameElements :: Ord a => [a] -> [a] -> Bool
sameElements xs ys = sort xs == sort ys

-- Model --

monoid_mempty_left :: (Eq a, Monoid a) => a -> Bool
monoid_mempty_left x = x == mappend mempty x

monoid_mempty_right :: (Eq a, Monoid a) => a -> Bool
monoid_mempty_right x = x == mappend x mempty

monoid_associative :: (Eq a, Monoid a) => a -> a -> a -> Bool
monoid_associative x y z = mappend x (mappend y z) == mappend (mappend x y) z

monoid :: (Arbitrary a, Show a, Eq a, Monoid a) => a -> Property
monoid x = monoid_mempty_left x .&&. monoid_mempty_right x .&&. monoid_associative x

deriving instance Arbitrary Sum

prop_Sum_monoid :: Sum -> Property
prop_Sum_monoid = monoid

deriving instance Arbitrary Account
deriving instance Arbitrary Balances

prop_Balances_monoid :: Balances -> Property
prop_Balances_monoid = monoid

forAllElements [] f = property True
forAllElements xs f = forAll (elements xs) f

prop_balanceFor :: [Balances] -> Property
prop_balanceFor bs = forAllElements accounts $
    \a -> balanceFor a together == mconcat (map (balanceFor a) bs)
  where together = mconcat bs
        accounts = balancesAccounts together

prop_div_names :: Sum -> NonEmptyList Account -> Bool
prop_div_names sum (NonEmpty accounts') = balancesAccounts (sum /// accounts) `sameElements` accounts
  where accounts = nub accounts'

prop_div_total :: Sum -> NonEmptyList Account -> Bool
prop_div_total sum (NonEmpty accounts') = total == sum
  where total = balancesTotal $ sum /// accounts
        accounts = nub accounts'

prop_div_fair :: Sum -> NonEmptyList Account -> Bool
prop_div_fair sum (NonEmpty accounts') = maxSum <= minSum + 1
  where sums = map sumCents . M.elems . unBalances $ sum /// accounts
        minSum = minimum sums
        maxSum = maximum sums
        accounts = nub accounts'

genAccounts :: Gen [Account]
genAccounts = fmap nub $ listOf1 arbitrary

genSimpleTransaction :: Gen SimpleTransaction
genSimpleTransaction = do
  sum <- arbitrary
  payers <- genAccounts
  sharedBy <- genAccounts
  return SimpleTransaction { simpleTransactionTime = Timestamp 0
                           , simpleTransactionDescription = ""
                           , simpleTransactionSum = sum
                           , simpleTransactionPayers = payers
                           , simpleTransactionSharedBy = sharedBy }

prop_makeTransaction_payers = forAll genSimpleTransaction $ \st ->
  (balancesAccounts . transactionPositive $ makeTransaction st)
  `sameElements`
  simpleTransactionPayers st

prop_makeTransaction_sharedBy = forAll genSimpleTransaction $ \st ->
  (balancesAccounts . transactionNegative $ makeTransaction st)
  `sameElements`
  simpleTransactionSharedBy st

prop_makeTransaction_valid = forAll genSimpleTransaction $ \st ->
  transactionValid $ makeTransaction st

prop_makeTransaction_sum = forAll genSimpleTransaction $ \st ->
  let t = makeTransaction st
      sum = simpleTransactionSum st
  in balancesTotal (transactionNegative t) == inverse sum
     &&
     balancesTotal (transactionPositive t) == sum


-- Running --

tests = [ testProperty "Sum_monoid" prop_Sum_monoid
        , testProperty "Balances_monoid" prop_Balances_monoid
        , testProperty "balanceFor" prop_balanceFor
        , testProperty "div_names" prop_div_names
        , testProperty "div_total" prop_div_total
        , testProperty "div_fair" prop_div_fair
        , testProperty "makeTransaction_payers" prop_makeTransaction_payers
        , testProperty "makeTransaction_sharedBy" prop_makeTransaction_sharedBy
        , testProperty "makeTransaction_valid" prop_makeTransaction_valid
        , testProperty "makeTransaction_sum" prop_makeTransaction_sum ]

main = defaultMain tests
