{-# LANGUAGE TypeOperators #-}

module Main where

import Api
import Model

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Map.Strict as M

server :: Server Api
server = account :<|> transaction :<|> allTransactions
  where account :: Account -> Handler Balance :<|> Handler [Transaction]
        account acc = balance acc :<|> transactionsFor acc
        balance :: Account -> Handler Balance
        balance acc = return $ transactionBalances state M.! acc
        transactionsFor :: Account -> Handler [Transaction]
        transactionsFor acc = return $ filter (concerns acc) database
        transaction :: Timestamp -> Handler Transaction
        transaction ts =
          return $ head $ filter (\t -> transactionTime t == ts) database
        allTransactions :: Handler [Transaction]
        allTransactions = return database

app :: Application
app = serve Api.api server

main :: IO ()
main = run 8081 app
