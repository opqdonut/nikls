{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Model

import Servant.API
import Data.Proxy

-- Api definition

type Content = '[JSON]

type AccountApi
  =    Capture "account" Account :> "balance" :> Get Content Sum
  :<|> Capture "account" Account :> "transactions" :> Get Content [Transaction]

type TransactionApi
  =    Capture "id" Id :> "cancel" :> Post Content String
  :<|> Capture "id" Id :> "uncancel" :> Post Content String
  :<|> Capture "id" Id :> Get Content Transaction
  :<|> Get Content [Transaction]
  :<|> ReqBody Content SimpleTransaction :> Post Content String

type BalancesApi = Get Content Balances

type Api = "v0" :> ("account" :> AccountApi
                    :<|> "transaction" :> TransactionApi
                    :<|> "balances" :> BalancesApi)

api :: Proxy Api
api = Proxy
