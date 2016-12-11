{-# LANGUAGE TypeOperators #-}

module Server where

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

notFound :: Handler a
notFound = throwError err404 { errBody = fromString "Transaction not found"}

invalid :: Handler a
invalid = throwError err500 { errBody = fromString "Invalid transaction." }

ok :: Handler String
ok = return "ok"

server :: Database -> Server Api
server db = account :<|>
            transactions :<|>
            balances
  where account :: Account -> Handler Sum :<|> Handler [Transaction]
        account acc = balance acc :<|> transactionsFor acc
        balance :: Account -> Handler Sum
        balance acc = do state <- databaseState db
                         return $ balanceFor acc state
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = filter (concerns acc) <$> databaseTransactions db

        transactions = transaction :<|> addTransaction :<|> allTransactions

        transaction :: Timestamp -> Handler String :<|> Handler String :<|> Handler Transaction
        transaction ts =
          setTransactionCancelled True ts :<|> setTransactionCancelled False ts :<|> getTransaction ts


        setTransactionCancelled :: Bool -> Timestamp -> Handler String
        setTransactionCancelled bool ts = do
          t <- getTransaction ts
          databaseUpdate db t {transactionCancelled = bool}
          ok
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
          runEnv 8081 (app db)
