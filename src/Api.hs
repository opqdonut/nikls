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
import Text.Read

-- Api definition

type Content = '[PlainText]

type AccountApi =
  "account" :> Capture "account" Account :>
    ("balance" :> Get Content Balance
     :<|> "transactions" :> Get Content [Transaction])

type TransactionApi =
  "transaction" :> (
    Capture "timestamp" Timestamp :> Get Content Transaction
    :<|> ReqBody Content Transaction :> Post Content String
    :<|> Get Content [Transaction])

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

instance MimeUnrender PlainText Transaction where
  mimeUnrender _ bs =
    case readEither (BS.unpack bs) of
      Left err -> Left ("Couldn't read "++show bs++": "++err)
      x -> x

instance MimeRender PlainText [Transaction] where
  mimeRender _ = bshow

instance MimeRender PlainText Balance where
  mimeRender _ (Balance b) = bshow b
