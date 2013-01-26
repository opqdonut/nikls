module Domain where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Ratio
import Data.Time

type Map = M.Map

-------------------------------
-- Balances and Transactions --
-------------------------------

newtype Person = Person { personName :: String }
               deriving (Show, Read, Eq, Ord)

newtype Balance = Balance { balanceCents :: Int }
                deriving (Show, Read, Eq, Ord)

instance Monoid Balance where
  mempty = Balance 0
  mappend (Balance a) (Balance b) = Balance (a+b)

newtype Balances = Balances { unBalances :: (Map Person Balance) }
                   deriving (Show, Eq, Read)

balancesValid :: Balances -> Bool
balancesValid (Balances m) =
  mempty == mconcat (M.elems m)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances a) (Balances b) = Balances $ M.unionWith mappend a b

data Transaction = Transaction { transactionBalances :: Balances
                                 {-, transactionTime :: UTCTime
                                 ,transactionDescription :: String-} }
                   deriving (Show, Read, Eq)

applyTransaction :: Balances -> Transaction -> Balances
applyTransaction b t = mappend b (transactionBalances t)

-------------------------
-- Simple transactions --
-------------------------

-- XXX a simpleTransaction should really contain the result of ///
-- instead of duplicating the /// logic everywhere...
data SimpleTransaction = SimpleTransaction { stPayer :: Person,
                                             stBenefitors :: [Person],
                                             stSum :: Balance }

(///) :: Integral a => a -> a -> [a]
a /// b = zipWith (-) (tail steps) steps
  where steps = [truncate (a * x % b) | x<-[0..b]]

simpleTransactionToTransaction :: SimpleTransaction -> Transaction
simpleTransactionToTransaction (SimpleTransaction payer benefitors sum) =
  Transaction $ Balances $ M.fromList $ (payer,sum):benefitors'
  where balances = map Balance $
                   negate (balanceCents sum) /// length benefitors
        benefitors' = zip benefitors balances

-----------------------------
-- Flows, i.e. debt graphs --
-----------------------------

data Flow = Flow { flowBalance :: Balance,
                   flowFrom :: Person,
                   flowTo :: Person }
            deriving (Show, Eq)

-- XXX don't use lists?
-- XXX code duplication from simpleTransactionToTransactions
simpleTransactionFlows :: SimpleTransaction -> [Flow]
simpleTransactionFlows (SimpleTransaction payer benefitors sum) = flows
  where balances = map Balance $
                   -- XXX is negation appropriate here?
                   negate (balanceCents sum) /// length benefitors
        flows = zipWith (\person balance -> Flow balance payer person)
                benefitors balances
