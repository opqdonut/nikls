{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Model

import Servant.API
import Data.Proxy

-- Api definition

type Content = '[PlainText, JSON]

type AccountApi =
  "account" :> Capture "account" Account :>
    ("balance" :> Get Content Balance
     :<|> "transactions" :> Get Content [Transaction])

type TransactionApi =
  "transaction" :> (
    Capture "timestamp" Timestamp :> Get Content Transaction
    -- XXX no FromJSON instance yet, thus PlainText:
    :<|> ReqBody '[PlainText] Transaction :> Post '[PlainText] String
    :<|> Get Content [Transaction])

type Api = "v0" :> (AccountApi :<|> TransactionApi)

api :: Proxy Api
api = Proxy
