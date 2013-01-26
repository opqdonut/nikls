{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Store where

import Domain

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Typeable
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader

type TransactionID = Integer

data StoredTransaction = StoredTransaction { transactionid :: TransactionID
                                           , deleted :: Bool
                                           , transaction :: SimpleTransaction }
                         deriving Typeable

data Database = Database { transactions :: [StoredTransaction],
                           nextId :: TransactionID }
                deriving Typeable

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''Balance)
$(deriveSafeCopy 0 'base ''SimpleTransaction)
$(deriveSafeCopy 0 'base ''StoredTransaction)
$(deriveSafeCopy 0 'base ''Database)

addTransaction_ :: SimpleTransaction -> Update Database StoredTransaction
addTransaction_ st =
  do Database ts id <- get
     let stored = StoredTransaction id False st
     put $ Database (stored:ts) (id+1)
     return stored

deleteTransaction_ :: TransactionID -> Update Database ()
deleteTransaction_ id =
  do Database ts id <- get
     put $ Database (map del ts) id
  where del s@(StoredTransaction id' _ t)
          | id == id' = StoredTransaction id' True t
          | otherwise = s

viewTransactions_ :: Query Database [StoredTransaction]
viewTransactions_ = asks transactions

$(makeAcidic ''Database ['addTransaction_, 'viewTransactions_, 'deleteTransaction_])

type DB = AcidState Database

openDatabase :: IO DB
openDatabase = openLocalState (Database [] 1)

closeDatabase :: DB -> IO ()
closeDatabase = createCheckpointAndClose

addTransaction :: DB -> SimpleTransaction -> IO StoredTransaction
addTransaction db st = update db (AddTransaction_ st)

deleteTransaction :: DB -> TransactionID -> IO ()
deleteTransaction db id = update db (DeleteTransaction_ id)

viewTransactions :: DB -> IO [StoredTransaction]
viewTransactions db = query db ViewTransactions_

viewValidTransactions :: DB -> IO [StoredTransaction]
viewValidTransactions db = fmap (filter (not.deleted)) $ viewTransactions db

viewDebtGraph :: DB -> IO DebtGraph
viewDebtGraph db =
  fmap (applySimpleTransactions emptyDebtGraph . map transaction)
  $ viewValidTransactions db

viewTransactionsFor :: DB -> Person -> IO [StoredTransaction]
viewTransactionsFor db p = fmap (filter (isInTransaction p . transaction)) $
                           viewTransactions db