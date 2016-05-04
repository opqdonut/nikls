{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Model

import Servant.API
import Servant.API.ContentTypes
import Data.Proxy
import Control.Applicative
import Data.Text as T

-- Api definition

type BalanceApi =
  "balance" :> Capture "account" Account :> Get '[PlainText] Balance
type TransactionApi =
  "transaction" :> Capture "timestamp" Timestamp :> Get '[PlainText] Transaction

type Api = "v0" :> (BalanceApi :<|> TransactionApi)

api :: Proxy Api
api = Proxy

-- Instances for url fragments

instance FromHttpApiData Account where
  parseUrlPiece t = return $ Account (T.unpack t)

instance FromHttpApiData Timestamp where
  parseUrlPiece t = Timestamp <$> parseUrlPiece t

-- Instances for rendering

plaintext :: Proxy PlainText
plaintext = Proxy

instance MimeRender PlainText Transaction where
  mimeRender _ a = mimeRender plaintext $ show a

instance MimeRender PlainText Balance where
  mimeRender _ (Balance b) = mimeRender plaintext $ show b
