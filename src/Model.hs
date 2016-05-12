module Model where

import qualified Data.Map.Strict as M
import Data.Word

newtype Balance = Balance { balanceCents :: Int }
                deriving (Show, Read, Eq)

instance Monoid Balance where
  mempty = Balance 0
  mappend (Balance a) (Balance b) = Balance (a+b)

newtype Account = Account { accountName :: String }
                  deriving (Show, Read, Eq, Ord)

newtype Timestamp = Timestamp { unTimestamp :: Word64 }
                    deriving (Show, Read, Eq, Ord)

instance Monoid Timestamp where
  mempty = Timestamp 0
  mappend (Timestamp a) (Timestamp b) = Timestamp (max a b)

data Transaction =
  Transaction {transactionTime :: Timestamp,
               transactionBalances :: (M.Map Account Balance) }
  deriving (Show, Read)

instance Monoid Transaction where
  mempty = Transaction mempty mempty
  mappend (Transaction tx x) (Transaction ty y) =
    Transaction (mappend tx ty) (M.unionWith mappend x y)

transactionValid :: Transaction -> Bool
transactionValid (Transaction _ bs) = mconcat (M.elems bs) == mempty

concerns :: Account -> Transaction -> Bool
concerns acc (Transaction _ ts) = M.member acc ts

-- -- -- --

alan = Account "alan"
beck = Account "beck"
carl = Account "carl"
dave = Account "dave"

b = Balance
t time bs = Transaction (Timestamp time) (M.fromList bs)

database :: [Transaction]
database = [t 1 [(alan, b (-3)),(beck, b 3)]
           ,t 2 [(carl, b 10),(dave, b (-5)),(beck, b (-5))]
           ,t 3 [(beck, b (-6)),(alan, b 3),(carl, b 3)]]

state :: Transaction
state = mconcat database
