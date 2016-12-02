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
  "transaction" :>
  ((Capture "timestamp" Timestamp :> ("cancel" :> Post Content String
                                      :<|> "uncancel" :> Post Content String
                                      :<|> Get Content Transaction))
    :<|> ReqBody Content Transaction :> Post Content String
    :<|> Get Content [Transaction])

type BalancesApi = "balances" :> Get Content Balances

type Api = "v0" :> (AccountApi :<|> TransactionApi :<|> BalancesApi)

api :: Proxy Api
api = Proxy
