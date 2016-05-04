{-# LANGUAGE TypeOperators #-}

module Main where

import Api
import Model

import Data.IORef
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Map.Strict as M

type Database = IORef [Transaction]

dbState :: MonadIO m => Database -> m Transaction
dbState db = liftIO $ mconcat <$> readIORef db

dbTransactions :: MonadIO m => Database -> m [Transaction]
dbTransactions db = liftIO $ readIORef db

server :: Database -> Server Api
server db = account :<|> transaction :<|> allTransactions
  where account :: Account -> Handler Balance :<|> Handler [Transaction]
        account acc = balance acc :<|> transactionsFor acc
        balance :: Account -> Handler Balance
        balance acc = do state <- dbState db
                         return $ transactionBalances state M.! acc
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = filter (concerns acc) <$> dbTransactions db
        transaction :: Timestamp -> Handler Transaction
        transaction ts =
          head . filter (\t -> transactionTime t == ts) <$> dbTransactions db
        allTransactions :: Handler [Transaction]
        allTransactions = dbTransactions db

app :: Database -> Application
app db = serve Api.api (server db)

main :: IO ()
main = do db <- newIORef database
          run 8081 (app db)
