module Model where

import qualified Data.Map.Strict as M
import Data.Word

newtype Sum = Sum { sumCents :: Int }
            deriving (Show, Read, Eq)

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a+b)

inverse :: Sum -> Sum
inverse (Sum s) = Sum (negate s)

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

-- A transaction has separate positive and negative balances so that
-- we can properly represent a situation where A pays 15 and the
-- benefit is split among A, B and C. Without separate positive and
-- negative this would show up as A+10 B-5 C-5 and the original sum
-- would be lost.
data Transaction =
  Transaction {transactionTime :: Timestamp,
               transactionDescription :: String,
               transactionCancelled :: Bool,
               transactionPositive :: Balances,
               transactionNegative :: Balances}
  deriving (Show, Read)

transactionBalances :: Transaction -> Balances
transactionBalances (Transaction _ _ _ pos neg) = mappend pos neg

transactionValid :: Transaction -> Bool
transactionValid = balancesValid . transactionBalances

concerns :: Account -> Transaction -> Bool
concerns acc = balancesConcern acc . transactionBalances

summarize :: [Transaction] -> Balances
summarize = mconcat . map transactionBalances . filter (not . transactionCancelled)

----- Creating transactions -----

(///) :: Sum -> [Account] -> Balances
(Sum s) /// as = fair `mappend` fixup
  where n = length as
        (part,remainder) = divMod s n
        fair = Balances . M.fromList $ zip as (repeat (Sum part))
        fixup = Balances $ M.singleton (head as) (Sum remainder)

data SimpleTransaction =
  SimpleTransaction { simpleTransactionTime :: Timestamp,
                      simpleTransactionDescription :: String,
                      simpleTransactionSum :: Sum,
                      simpleTransactionPayers :: [Account],
                      simpleTransactionSharedBy :: [Account] }
  deriving (Read, Show)

makeTransaction :: SimpleTransaction -> Transaction
makeTransaction s = Transaction {
  transactionTime = simpleTransactionTime s,
  transactionDescription = simpleTransactionDescription s,
  transactionCancelled = False,
  transactionPositive = simpleTransactionSum s /// simpleTransactionPayers s,
  transactionNegative = inverse (simpleTransactionSum s)
                        /// simpleTransactionSharedBy s}
