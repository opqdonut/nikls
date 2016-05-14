{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Model

import Servant.API
import Data.Proxy

-- Api definition

type Content = '[JSON]

type AccountApi =
  "account" :> Capture "account" Account :>
    ("balance" :> Get Content Sum
     :<|> "transactions" :> Get Content [Transaction])

type TransactionApi =
  "transaction" :> (
    Capture "timestamp" Timestamp :> Get Content Transaction
    :<|> ReqBody Content Transaction :> Post Content String
    :<|> Get Content [Transaction])

type Api = "v0" :> (AccountApi :<|> TransactionApi)

api :: Proxy Api
api = Proxy
