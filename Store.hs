{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

module Store where

import Domain

import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Typeable
import Data.SafeCopy
import Control.Monad.State
import Control.Monad.Reader

data Database = Database { transactions :: [SimpleTransaction] }
                deriving Typeable

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''Balance)
$(deriveSafeCopy 0 'base ''SimpleTransaction)
$(deriveSafeCopy 0 'base ''Database)

addTransaction_ :: SimpleTransaction -> Update Database ()
addTransaction_ st =
  do Database ts <- get
     put $ Database (st:ts)

viewTransactions_ :: Query Database [SimpleTransaction]
viewTransactions_ = asks transactions

$(makeAcidic ''Database ['addTransaction_, 'viewTransactions_])

type DB = AcidState Database

openDatabase :: IO DB
openDatabase = openLocalState (Database [])

closeDatabase :: DB -> IO ()
closeDatabase = createCheckpointAndClose

addTransaction :: DB -> SimpleTransaction -> IO ()
addTransaction db st = update db (AddTransaction_ st)

viewTransactions :: DB -> IO [SimpleTransaction]
viewTransactions db = query db ViewTransactions_

viewDebtGraph :: DB -> IO DebtGraph
viewDebtGraph db = fmap (applySimpleTransactions emptyDebtGraph) $
                   viewTransactions db