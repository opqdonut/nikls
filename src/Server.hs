{-# LANGUAGE TypeOperators #-}

module Server where

import Api
import Model
import Render()
import Db

import Data.String
import Control.Monad
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, only)

notFound :: Handler a
notFound = throwError err404 { errBody = fromString "Transaction not found"}

invalid :: Handler a
invalid = throwError err500 { errBody = fromString "Invalid transaction." }

ok :: Handler String
ok = return "ok"

serveAccount :: Database -> Server AccountApi
serveAccount db = balance :<|> transactionsFor
  where balance :: Account -> Handler Sum
        balance acc = do state <- databaseState db
                         return $ balanceFor acc state
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = filter (concerns acc) <$> databaseTransactions db

serveTransactions :: Database -> Server TransactionApi
serveTransactions db = cancel :<|> uncancel :<|>
                       getTransaction :<|> allTransactions :<|> addTransaction
  where setTransactionCancelled :: Bool -> Timestamp -> Handler String
        setTransactionCancelled bool ts = do
          t <- getTransaction ts
          databaseUpdate db t {transactionCancelled = bool}
          ok
        cancel = setTransactionCancelled True
        uncancel = setTransactionCancelled False
        getTransaction :: Timestamp -> Handler Transaction
        getTransaction ts = do
          res <- databaseGetTransaction db ts
          case res of Nothing -> notFound
                      Just t -> return t
        addTransaction :: SimpleTransaction -> Handler String
        addTransaction st = do
          let t = makeTransaction st
          unless (transactionValid t) invalid
          databaseAdd db t
          ok
        allTransactions :: Handler [Transaction]
        allTransactions = databaseTransactions db

serveBalances :: Database -> Server BalancesApi
serveBalances db = databaseState db

server :: Database -> Server Api
server db = serveAccount db :<|>
            serveTransactions db :<|>
            serveBalances db

mycors :: Request -> Maybe CorsResourcePolicy
-- allow Content-Type header:
mycors _ = Just $ simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders }

app :: Database -> Application
app db = logStdoutDev . static . cors mycors $ serve Api.api (server db)
  where static = staticPolicy $ only [("", "frontend.html")]

main :: IO ()
main = do db <- openDatabase
          runEnv 8081 (app db)
