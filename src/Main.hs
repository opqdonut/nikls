module Main where

import Api
import Model

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Map.Strict as M

server :: Server Api
server = balance :<|> transaction
  where balance :: Account -> Handler Balance
        balance acc = return $ transactionBalances state M.! acc
        transaction :: Timestamp -> Handler Transaction
        transaction ts =
          return $ head $ filter (\t -> transactionTime t == ts) database

app :: Application
app = serve Api.api server

main :: IO ()
main = run 8081 app
