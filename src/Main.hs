{-# LANGUAGE TypeOperators #-}

module Main where

import Api
import Model
import Render
import Db

import Data.IORef
import Data.String
import Control.Applicative
import Control.Monad
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, only)

import qualified Data.Map.Strict as M

server :: Database -> Server Api
server db = account :<|>
            transaction :<|>
            balances
  where account :: Account -> Handler Sum :<|> Handler [Transaction]
        account acc = balance acc :<|> transactionsFor acc
        balance :: Account -> Handler Sum
        balance acc = do state <- databaseState db
                         return $ balanceFor state acc
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = filter (concerns acc) <$> databaseTransactions db

        transaction = getTransaction :<|> addTransaction :<|> allTransactions
        getTransaction :: Timestamp -> Handler Transaction
        getTransaction ts =
          head . filter (\t -> transactionTime t == ts) <$>
          databaseTransactions db
        addTransaction :: Transaction -> Handler String
        addTransaction t = do
          unless (transactionValid t) $
            throwError err500 { errBody = fromString "Invalid transaction." }
          databaseAdd t db
          return "ok"
        allTransactions :: Handler [Transaction]
        allTransactions = databaseTransactions db

        balances :: Handler Balances
        balances = databaseState db

mycors :: Request -> Maybe CorsResourcePolicy
-- allow Content-Type header:
mycors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }

app :: Database -> Application
app db = logStdoutDev . static . cors mycors $ serve Api.api (server db)
  where static = staticPolicy $ only [("", "frontend.html")]

main :: IO ()
main = do db <- openDatabase
          putStrLn "Serving on 8081"
          run 8081 (app db)
