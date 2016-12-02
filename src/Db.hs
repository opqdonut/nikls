{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import Model
import Render

import Data.Aeson (encode, decode)
import Data.String
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

instance FromRow Transaction where
  -- XXX handle parse failures
  fromRow = do (_time :: Int) <- field
               json <- field
               let Just t = decode json
               return t

instance ToField Transaction where
  toField = toField . encode

instance ToField Timestamp where
  toField = toField . unTimestamp

dbPath :: String
dbPath = "nikls.sqlite"

migrate :: Int -> Connection -> IO ()
-- current version
migrate 1 _ = return ()
-- fresh database
migrate 0 conn = do
  execute_ conn $ fromString "create table if not exists transactions \
                             \(time INTEGER PRIMARY KEY, json TEXT)"
  setVersion conn 1
-- otherwise
migrate x _ = error $ "Unknown db version: " ++ show x

getVersion :: Connection -> IO Int
getVersion conn = do
  [[x]] <- query_ conn $ fromString "pragma user_version"
  return x

setVersion :: Connection -> Int -> IO ()
setVersion conn ver = do
  -- no ?-argument support for pragmas
  execute_ conn . fromString $ "pragma user_version = " ++ show ver
  return ()

type Database = Connection

openDatabase :: MonadIO m => m Database
openDatabase = liftIO $ do
  conn <- open dbPath
  version <- liftIO $ getVersion conn
  liftIO $ migrate version conn
  return conn

getAll :: Query
getAll = fromString "select * from transactions"

databaseTransactions :: MonadIO m => Database -> m [Transaction]
databaseTransactions conn = liftIO $ query_ conn getAll

databaseState :: MonadIO m => Database -> m Balances
databaseState = fmap summarize . databaseTransactions

add :: Query
add = fromString "insert into transactions (time, json) values (?,?)"

databaseAdd :: MonadIO m => Transaction -> Database -> m ()
databaseAdd t conn = liftIO $ execute conn add (transactionTime t, t)

update :: Query
update = fromString "update transactions set (time, json) = (?,?) where time = ?"

databaseUpdate :: MonadIO m => Transaction -> Database -> m ()
databaseUpdate t conn = liftIO $ execute conn update (transactionTime t, t, transactionTime t)
