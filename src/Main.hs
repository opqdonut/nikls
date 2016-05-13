{-# LANGUAGE TypeOperators #-}

module Main where

import Api
import Model
import Render
import Db

import Data.IORef
import Data.Monoid
import Control.Applicative
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified Data.Map.Strict as M

server :: Database -> Server Api
server db = account :<|> transaction :<|> addTransaction :<|> allTransactions
  where account :: Account -> Handler Balance :<|> Handler [Transaction]
        account acc = balance acc :<|> transactionsFor acc
        balance :: Account -> Handler Balance
        balance acc = do state <- databaseState db
                         return $ transactionBalances state M.! acc
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = filter (concerns acc) <$> databaseTransactions db
        transaction :: Timestamp -> Handler Transaction
        transaction ts =
          head . filter (\t -> transactionTime t == ts) <$>
          databaseTransactions db
        addTransaction :: Transaction -> Handler String
        addTransaction t = "ok" <$ databaseAdd t db
        allTransactions :: Handler [Transaction]
        allTransactions = databaseTransactions db

mycors :: Request -> Maybe CorsResourcePolicy
-- allow Content-Type header:
mycors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }

app :: Database -> Application
app db = logStdoutDev . cors mycors $ serve Api.api (server db)

main :: IO ()
main = do db <- openDatabase
          run 8081 (app db)
