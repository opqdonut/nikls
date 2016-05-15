module Model where

import qualified Data.Map.Strict as M
import Data.Word

newtype Sum = Sum { sumCents :: Int }
            deriving (Show, Read, Eq)

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a+b)

newtype Account = Account { accountName :: String }
                deriving (Show, Read, Eq, Ord)

newtype Balances = Balances { unBalances :: (M.Map Account Sum) }
                 deriving (Show, Read)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances a) (Balances b) = Balances $ M.unionWith mappend a b

balancesValid :: Balances -> Bool
balancesValid (Balances b) = mconcat (M.elems b) == mempty

balancesConcern :: Account -> Balances -> Bool
balancesConcern acc (Balances b) = M.member acc b

-- XXX unsafe
balanceFor :: Balances -> Account -> Sum
balanceFor (Balances b) acc = b M.! acc

newtype Timestamp = Timestamp { unTimestamp :: Word64 }
                    deriving (Show, Read, Eq, Ord)

data Transaction =
  Transaction {transactionTime :: Timestamp,
               transactionDescription :: String,
               transactionBalances :: Balances}
  deriving (Show, Read)

transactionValid :: Transaction -> Bool
transactionValid (Transaction _ _ bs) = balancesValid bs

concerns :: Account -> Transaction -> Bool
concerns acc (Transaction _ _ bs) = balancesConcern acc bs

summarize :: [Transaction] -> Balances
summarize = mconcat . map transactionBalances

-- XXX transactions don't represent shared expenses that well
-- A paying 15 and sharing it with B and C will show up as
-- A+10 B-5 C-5
--
-- consider storing some higher level representation that maps to
-- balances
--
-- this would also help in moving logic out of the frontend
