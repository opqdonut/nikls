{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Db where

import Model
import Render()

import Data.Aeson (encode, decode)
import Data.Int
import Data.Maybe (listToMaybe)
import Data.String
import Control.Monad
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

instance FromField Transaction where
  fromField f = do str <- fromField f
                   case decode str of
                     Just t -> return t
                     Nothing -> returnError ConversionFailed f "JSON decode failed"

instance ToField Transaction where
  toField = toField . encode

noId :: Maybe Int64
noId = Nothing

instance ToField Id where
  toField (Id i) = toField i
  toField New = toField noId

dbPath :: String
dbPath = "nikls.sqlite"

migrate :: Int -> Connection -> IO ()
-- current version
migrate 1 _ = return ()
-- fresh database
migrate 0 conn = do
  execute_ conn $ fromString "create table if not exists transactions \
                             \(id INTEGER PRIMARY KEY, json TEXT)"
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
openDatabase = openDatabasePath dbPath

openDatabasePath :: MonadIO m => String -> m Database
openDatabasePath path = liftIO $ do
  conn <- open path
  version <- liftIO $ getVersion conn
  liftIO $ migrate version conn
  return conn

getAll :: Query
getAll = fromString "select json from transactions"

databaseTransactions :: MonadIO m => Database -> m [Transaction]
databaseTransactions conn = map fromOnly <$> liftIO (query_ conn getAll)

get :: Query
get = fromString "select json from transactions where id = ?"

databaseGetTransaction :: MonadIO m => Database -> Id -> m (Maybe Transaction)
databaseGetTransaction db i =
  listToMaybe . map fromOnly <$> liftIO (query db get (Only i))

databaseState :: MonadIO m => Database -> m Balances
databaseState = fmap summarize . databaseTransactions

add :: Query
add = fromString "insert or replace into transactions (id, json) values (?, ?)"

databaseAdd :: MonadIO m => Database -> Transaction -> m Transaction
-- Due to IDs, add is a bit complicated. We add the object once to get
-- the id, then add it again with the right id.
databaseAdd conn t = liftIO $ withTransaction conn $ do
  unless (transactionId t == New) (fail "Transaction id not New in add")
  execute conn add (noId, t)
  rowId <- lastInsertRowId conn
  let final = t {transactionId = Id rowId}
  execute conn add (rowId, final)
  return final

update :: Query
update = fromString "update transactions set (json) = (?) where id = ?"

databaseUpdate :: MonadIO m => Database -> Transaction -> m ()
databaseUpdate conn t = liftIO $ do
  unless (transactionId t /= New) (fail "Transaction id is New in update")
  execute conn update (t, transactionId t)
