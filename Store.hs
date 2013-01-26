{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Store where

import Domain

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Typeable
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M

type TransactionID = Integer

data StoredTransaction = StoredTransaction { transactionid :: TransactionID
                                           , deleted :: Bool
                                           , transaction :: SimpleTransaction }
                         deriving Typeable

type Password = String

data Database_v0 = Database_v0 [StoredTransaction] TransactionID

data Database = Database { transactions :: [StoredTransaction]
                         , nextId :: TransactionID
                         , users :: M.Map Person Password }
                deriving Typeable

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''Balance)
$(deriveSafeCopy 0 'base ''SimpleTransaction)
$(deriveSafeCopy 0 'base ''StoredTransaction)
$(deriveSafeCopy 0 'base ''Database_v0)
$(deriveSafeCopy 1 'extension ''Database)

instance Migrate Database where
  type MigrateFrom Database = Database_v0
  migrate (Database_v0 ts id) = Database ts id M.empty

addTransaction_ :: SimpleTransaction -> Update Database StoredTransaction
addTransaction_ st =
  do d@Database{transactions = ts, nextId = id} <- get
     let stored = StoredTransaction id False st
     put $ d { transactions = stored:ts
             , nextId = (id+1) }
     return stored

deleteTransaction_ :: TransactionID -> Update Database ()
deleteTransaction_ id =
  do d@Database{transactions = ts} <- get
     put $ d { transactions = (map del ts) }
  where del s@(StoredTransaction id' _ t)
          | id == id' = StoredTransaction id' True t
          | otherwise = s

viewTransactions_ :: Query Database [StoredTransaction]
viewTransactions_ = asks transactions

viewPasswords_ :: Query Database (M.Map Person Password)
viewPasswords_ = asks users

setPassword_ :: Person -> Password -> Update Database ()
setPassword_ p pw = do d@Database{users = u} <- get
                       put $ d { users = M.insert p pw u }

$(makeAcidic ''Database
  ['addTransaction_, 'viewTransactions_, 'deleteTransaction_,
  'viewPasswords_, 'setPassword_])

type DB = AcidState Database

openDatabase :: IO DB
openDatabase = openLocalState (Database [] 1 M.empty)

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

viewPasswords :: DB -> IO (M.Map Person Password)
viewPasswords db = query db ViewPasswords_

setPassword :: DB -> Person -> Password -> IO ()
setPassword db p pw = update db (SetPassword_ p pw)