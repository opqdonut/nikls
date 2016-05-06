-- various conversion instances for things in Model

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Render where

import Model

import Data.Proxy
import Servant.API
--import Servant.API.ContentTypes
import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readEither)
import Data.Aeson (ToJSON(..), Value(..), (.=), object, encode)
import qualified Data.Map.Strict as M

-- parsing url frangments

instance FromHttpApiData Account where
  parseUrlPiece t = return $ Account (T.unpack t)

instance FromHttpApiData Timestamp where
  parseUrlPiece t = Timestamp <$> parseUrlPiece t

-- rendering plaintext

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

-- rendering json

-- XXX implement ToJSON.toEncoding for better performance
instance ToJSON Transaction where
  toJSON (Transaction time balances) =
    object [T.pack "time" .= toJSON time,
            T.pack "balances" .= toJSON bs]
    where bs = [object [T.pack "account" .= toJSON a,
                        T.pack "balance" .= toJSON b]
               | (a,b) <- M.assocs balances]

instance ToJSON Balance where
  toJSON (Balance b) = Number (fromIntegral b)

instance ToJSON Account where
  toJSON (Account n) = String (T.pack n)

instance ToJSON Timestamp where
  -- XXX strings instead?
  toJSON (Timestamp t) = Number (fromIntegral t)
