{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, DeriveDataTypeable #-}

module Domain where

import Control.Monad (msum, forM_)
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Ratio
import Data.Time
import Data.Typeable (Typeable)
import System.Locale (defaultTimeLocale)

type Map = M.Map

--------------------------
-- Persons and Balances --
--------------------------

newtype Person = Person { personName :: String }
               deriving (Show, Read, Eq, Ord)

newtype Balance = Balance { balanceCents :: Int }
                deriving (Show, Read, Eq, Ord)

instance Monoid Balance where
  mempty = Balance 0
  mappend (Balance a) (Balance b) = Balance (a+b)

-- XXX semigroup?
negateBalance :: Balance -> Balance
negateBalance (Balance b) = Balance (negate b)

newtype Balances = Balances { unBalances :: (Map Person Balance) }
                   deriving (Show, Eq, Read)

balancesValid :: Balances -> Bool
balancesValid (Balances m) =
  mempty == mconcat (M.elems m)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances a) (Balances b) = Balances $ M.unionWith mappend a b

-----------------
-- Debt Graphs --
-----------------

type DebtGraph = M.Map Person (M.Map Person Balance)

emptyDebtGraph = M.empty

printDebtGraph :: DebtGraph -> IO ()
printDebtGraph g = forM_ (M.assocs g) $ \(Person p,status) -> do
  putStr $ p ++ ": "
  forM_ (M.assocs status) $ \(Person p',Balance bal) ->
    putStr $ p'++","++show bal++" "
  putStrLn ""

balances :: DebtGraph -> Balances
balances g = Balances $ M.map (mconcat . M.elems) g

eliminateZeros :: M.Map Person Balance -> M.Map Person Balance
eliminateZeros = M.filter (/=mempty)

-- Make DebtGraph a newtype to avoid overlapping instance here
instance Monoid DebtGraph where
  mempty = M.empty
  mappend = M.unionWith (\a b -> eliminateZeros $ M.unionWith mappend a b)

addDebt :: DebtGraph -> Person -> Person -> Balance -> DebtGraph
addDebt g from to bal =
  mconcat [g, M.singleton from $ M.singleton to bal,
           M.singleton to $ M.singleton from (negateBalance bal)]

-- iteratively: find a person who isn't a sink or a source and make them one
propagateDebts :: DebtGraph -> DebtGraph
propagateDebts g = case next of Nothing -> g
                                Just g' -> propagateDebts g'
  where next = msum $ map (eliminate g) (M.keys g)

-- local
data Debt = Debt Person Person Balance
          deriving (Show, Read, Eq)

apply :: DebtGraph -> Debt -> DebtGraph
apply g (Debt from to bal) = addDebt g from to bal

applyMany :: DebtGraph -> [Debt] -> DebtGraph
applyMany g ps = foldl' apply g ps

eliminate :: DebtGraph -> Person -> Maybe DebtGraph
eliminate g p
  | null creditors || null debitors = Nothing
  | otherwise = Just $ eliminate' g p creditors debitors
  where status = M.findWithDefault M.empty p g
        creditors = M.keys $ M.filter (<mempty) status
        debitors = M.keys $ M.filter (>mempty) status

eliminate' :: DebtGraph -> Person -> [Person] -> [Person] -> DebtGraph
eliminate' g p [] _  = g
eliminate' g p _  [] = g
eliminate' g p (c:cs) (d:ds)
  = eliminate' (applyMany g [Debt p c $ Balance amount,
                             Debt c d $ Balance amount,
                             Debt d p $ Balance amount])
    p
    (if amount == abs csum then cs else c:cs)
    (if amount == abs dsum then ds else d:ds)
  where Just status = M.lookup p g
        Just (Balance csum) = M.lookup c status
        Just (Balance dsum) = M.lookup d status
        amount = min (abs csum) (abs dsum)

example1 :: DebtGraph
example1 = applyMany M.empty [Debt a b $ Balance 10,
                              Debt a b $ Balance 1,
                              Debt a c $ Balance 1,
                              Debt c b $ Balance 5,
                              Debt b d $ Balance 1,
                              Debt b a $ Balance 2]
  where a = Person "a"
        b = Person "b"
        c = Person "c"
        d = Person "d"

test1 :: Bool
test1 = balances example1 == balances (propagateDebts example1)

-------------------------
-- Simple transactions --
-------------------------

-- XXX a simpleTransaction should really contain the result of ///
-- instead of duplicating the /// logic everywhere...
data SimpleTransaction = SimpleTransaction { stPayer :: Person,
                                             stBenefitors :: [Person],
                                             stSum :: Balance,
                                             stDescription :: String,
                                             stTime :: UTCTime }
                         deriving (Show, Read, Typeable)

isInTransaction :: Person -> SimpleTransaction -> Bool
isInTransaction p (SimpleTransaction payer benefitors _ _ _) =
  p == payer || p `elem` benefitors

(///) :: Integral a => a -> a -> [a]
a /// b = zipWith (-) (tail steps) steps
  where steps = [truncate (a * x % b) | x<-[0..b]]

simpleTransactionDebts :: SimpleTransaction -> [Debt]
simpleTransactionDebts (SimpleTransaction payer benefitors sum _ _) =
  zipWith (\p b -> Debt payer p b) benefitors balances
  where balances = map Balance $
                   balanceCents sum /// length benefitors

applySimpleTransaction :: DebtGraph -> SimpleTransaction -> DebtGraph
applySimpleTransaction g st = applyMany g (simpleTransactionDebts st)

applySimpleTransactions :: DebtGraph -> [SimpleTransaction] -> DebtGraph
applySimpleTransactions = foldl' applySimpleTransaction


t = readTime defaultTimeLocale "%Y-%m-%d %R"

examplet1 = SimpleTransaction (Person "a") [Person "a", Person "b", Person "c"]
            (Balance 100) "Pizza" (t "2013-01-15 16:37")
examplet2 = SimpleTransaction (Person "b") [Person "a", Person "c"]
            (Balance 70) "Kebab" (t "2013-01-16 18:50")
examplet3 = SimpleTransaction (Person "c") [Person "a", Person "b", Person "c"]
            (Balance 80) "Beer" (t "2013-01-17 19:00")