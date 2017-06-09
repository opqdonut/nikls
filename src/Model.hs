module Model where

import qualified Data.Map.Strict as M
import Data.Word
import Data.Int

newtype Sum = Sum { sumCents :: Int }
            deriving (Show, Read, Eq)

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a+b)

inverse :: Sum -> Sum
inverse (Sum s) = Sum (negate s)

newtype Account = Account { accountName :: String }
                deriving (Show, Read, Eq, Ord)

newtype Balances = Balances { unBalances :: M.Map Account Sum }
                 deriving (Show, Read, Eq)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances a) (Balances b) = Balances $ M.unionWith mappend a b

balancesTotal :: Balances -> Sum
balancesTotal (Balances b) = mconcat $ M.elems b

balancesValid :: Balances -> Bool
balancesValid = (== mempty) . balancesTotal

balancesConcern :: Account -> Balances -> Bool
balancesConcern acc (Balances b) = M.member acc b

balancesAccounts :: Balances -> [Account]
balancesAccounts (Balances b) = M.keys b

balanceFor :: Account -> Balances -> Sum
balanceFor acc (Balances b) = M.findWithDefault mempty acc b

newtype Timestamp = Timestamp { unTimestamp :: Word64 }
                    deriving (Show, Read, Eq, Ord)

data Id = New | Id Int64
  deriving (Show, Read, Eq)

-- A transaction has separate positive and negative balances so that
-- we can properly represent a situation where A pays 15 and the
-- benefit is split among A, B and C. Without separate positive and
-- negative this would show up as A+10 B-5 C-5 and the original sum
-- would be lost.
data Transaction =
  Transaction {transactionId :: Id,
               transactionTime :: Timestamp,
               transactionDescription :: String,
               transactionCancelled :: Bool,
               transactionPositive :: Balances,
               transactionNegative :: Balances}
  deriving (Show, Read, Eq)

transactionBalances :: Transaction -> Balances
transactionBalances t = mappend (transactionPositive t) (transactionNegative t)

transactionValid :: Transaction -> Bool
transactionValid = balancesValid . transactionBalances

concerns :: Account -> Transaction -> Bool
concerns acc = balancesConcern acc . transactionBalances

summarize :: [Transaction] -> Balances
summarize = mconcat . map transactionBalances . filter (not . transactionCancelled)

----- Creating transactions -----

-- XXX as must be nonempty, elements must be unique
(///) :: Sum -> [Account] -> Balances
(Sum s) /// as = fair `mappend` fixup
  where n = length as
        (part,remainder) = divMod s n
        fair = Balances . M.fromList $ zip as (repeat (Sum part))
        fixup = Balances . M.fromList $ zip as (replicate remainder (Sum 1))

data SimpleTransaction =
  SimpleTransaction { simpleTransactionTime :: Timestamp,
                      simpleTransactionDescription :: String,
                      simpleTransactionSum :: Sum,
                      simpleTransactionPayers :: [Account],
                      simpleTransactionSharedBy :: [Account] }
  deriving (Read, Show)

makeTransaction :: SimpleTransaction -> Transaction
makeTransaction s = Transaction {
  transactionId = New,
  transactionTime = simpleTransactionTime s,
  transactionDescription = simpleTransactionDescription s,
  transactionCancelled = False,
  transactionPositive = simpleTransactionSum s /// simpleTransactionPayers s,
  transactionNegative = inverse (simpleTransactionSum s)
                        /// simpleTransactionSharedBy s}
