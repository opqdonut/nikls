{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import Model

import Servant.API
import Servant.API.ContentTypes
import Data.Proxy
import Control.Applicative
import Data.Text as T
import Data.ByteString.Lazy.Char8 as BS

-- Api definition

type AccountApi =
  "account" :> Capture "account" Account :>
    ("balance" :> Get '[PlainText] Balance
     :<|>
     "transactions" :> Get '[PlainText] [Transaction])

type TransactionApi =
  "transaction" :> Capture "timestamp" Timestamp :> Get '[PlainText] Transaction
  :<|> "transactions" :> Get '[PlainText] [Transaction]

type Api = "v0" :> (AccountApi :<|> TransactionApi)

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

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

instance MimeRender PlainText Transaction where
  mimeRender _ = bshow

instance MimeRender PlainText [Transaction] where
  mimeRender _ = bshow

instance MimeRender PlainText Balance where
  mimeRender _ (Balance b) = bshow b
