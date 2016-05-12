{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import Model
import Render

import Data.Aeson (encode, decode)
import Data.String
import Control.Monad.IO.Class
import Database.SQLite.Simple

instance FromRow Transaction where
  -- XXX handle parse failures
  fromRow = do (_time :: Int) <- field
               json <- field
               let Just t = decode json
               return t

instance ToRow Transaction where
  toRow t = toRow (unTimestamp $ transactionTime t, encode t)

dbPath :: String
dbPath = "nikls.sqlite"

dbInit :: Query
dbInit = fromString "create table if not exists transactions \
                    \(time INTEGER PRIMARY KEY, json TEXT)"

type Database = Connection

openDatabase :: MonadIO m => m Database
openDatabase = liftIO $ do
  conn <- open dbPath
  execute_ conn dbInit
  return conn

getAll :: Query
getAll = fromString "select * from transactions"

databaseTransactions :: MonadIO m => Database -> m [Transaction]
databaseTransactions conn = liftIO $ query_ conn getAll

databaseState :: MonadIO m => Database -> m Transaction
databaseState = fmap mconcat . databaseTransactions

add :: Query
add = fromString "insert into transactions (time, json) values (?,?)"

databaseAdd :: MonadIO m => Transaction -> Database -> m ()
databaseAdd t conn = liftIO $ execute conn add t
