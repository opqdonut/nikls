{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-missing-signatures #-}

module Main where

import Model
import Render
import Db

import Data.List
import qualified Test.HUnit as H
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

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

forAllElements [] _ = property True
forAllElements xs f = forAll (elements xs) f

prop_balanceFor :: [Balances] -> Property
prop_balanceFor bs = forAllElements accounts $
    \a -> balanceFor a together == mconcat (map (balanceFor a) bs)
  where together = mconcat bs
        accounts = balancesAccounts together

prop_div_names :: Sum -> NonEmptyList Account -> Bool
prop_div_names s (NonEmpty accounts') = balancesAccounts (s /// accounts) `sameElements` accounts
  where accounts = nub accounts'

prop_div_total :: Sum -> NonEmptyList Account -> Bool
prop_div_total s (NonEmpty accounts') = total == s
  where total = balancesTotal $ s /// accounts
        accounts = nub accounts'

prop_div_fair :: Sum -> NonEmptyList Account -> Bool
prop_div_fair s (NonEmpty accounts') = maxSum <= minSum + 1
  where sums = map sumCents . M.elems . unBalances $ s /// accounts
        minSum = minimum sums
        maxSum = maximum sums
        accounts = nub accounts'

genAccounts :: Gen [Account]
genAccounts = fmap nub $ listOf1 arbitrary

deriving instance Arbitrary Timestamp

instance Arbitrary SimpleTransaction where
  arbitrary = do
    time <- arbitrary
    s <- arbitrary
    payers <- genAccounts
    sharedBy <- genAccounts
    return SimpleTransaction { simpleTransactionTime = time
                             , simpleTransactionDescription = ""
                             , simpleTransactionSum = s
                             , simpleTransactionPayers = payers
                             , simpleTransactionSharedBy = sharedBy }

prop_makeTransaction_payers :: SimpleTransaction -> Bool
prop_makeTransaction_payers st =
  (balancesAccounts . transactionPositive $ makeTransaction st)
  `sameElements`
  simpleTransactionPayers st

prop_makeTransaction_sharedBy :: SimpleTransaction -> Bool
prop_makeTransaction_sharedBy st =
  (balancesAccounts . transactionNegative $ makeTransaction st)
  `sameElements`
  simpleTransactionSharedBy st

prop_makeTransaction_valid :: SimpleTransaction -> Bool
prop_makeTransaction_valid st =
  transactionValid $ makeTransaction st

prop_makeTransaction_sum :: SimpleTransaction -> Bool
prop_makeTransaction_sum st =
  let t = makeTransaction st
      s = simpleTransactionSum st
  in balancesTotal (transactionNegative t) == inverse s
     &&
     balancesTotal (transactionPositive t) == s

instance Arbitrary Id where
  arbitrary = oneof [ return New
                    , Id <$> arbitrary ]

instance Arbitrary Transaction where
  arbitrary = do
    i <- arbitrary
    time <- arbitrary
    desc <- arbitrary
    cancelled <- arbitrary
    pos <- arbitrary
    neg <- arbitrary
    return $ Transaction i time desc cancelled pos neg
  shrink t =
    [ t {transactionPositive = p, transactionNegative = n}
    | (p, n) <- shrink (transactionPositive t, transactionNegative t) ]

-- Render --

json_roundtrip :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
json_roundtrip x = Right x == fromJSONString (toJSONString x)

prop_Sum_roundtrip :: Sum -> Bool
prop_Sum_roundtrip = json_roundtrip

prop_Account_roundtrip :: Account -> Bool
prop_Account_roundtrip = json_roundtrip

prop_Balances_roundtrip :: Balances -> Bool
prop_Balances_roundtrip = json_roundtrip

prop_Timestamp_roundtrip :: Timestamp -> Bool
prop_Timestamp_roundtrip = json_roundtrip

prop_Transaction_roundtrip :: Transaction -> Bool
prop_Transaction_roundtrip = json_roundtrip

-- Db --

test_db_add_get :: H.Assertion
test_db_add_get =
  do db <- openDatabasePath ""
     t0 <- generate arbitrary
     let t = t0 { transactionId = New }
     databaseAdd db t
     [t'] <- databaseTransactions db
     H.assertEqual "listed" t t' {transactionId = New}
     let i = transactionId t'
     Just t'' <- databaseGetTransaction db i
     H.assertEqual "got" t t'' {transactionId = New}

-- Running --

tests = [ testGroup "Model"
          [ testProperty "Sum_monoid" prop_Sum_monoid
          , testProperty "Balances_monoid" prop_Balances_monoid
          , testProperty "balanceFor" prop_balanceFor
          , testProperty "div_names" prop_div_names
          , testProperty "div_total" prop_div_total
          , testProperty "div_fair" prop_div_fair
          , testProperty "makeTransaction_payers" prop_makeTransaction_payers
          , testProperty "makeTransaction_sharedBy" prop_makeTransaction_sharedBy
          , testProperty "makeTransaction_valid" prop_makeTransaction_valid
          , testProperty "makeTransaction_sum" prop_makeTransaction_sum ]
        , testGroup "Render"
          [ testProperty "Sum_roundtrip" prop_Sum_roundtrip
          , testProperty "Account_roundtrip" prop_Account_roundtrip
          , testProperty "Balances_roundtrip" prop_Balances_roundtrip
          , testProperty "Timestamp_roundtrip" prop_Timestamp_roundtrip ]
        , testGroup "Db"
          [ testCase "test_db_add_get" test_db_add_get ] ]

main = defaultMain tests
