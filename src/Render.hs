-- various conversion instances for things in Model

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Render where

import Model

import Control.Monad
import Data.Proxy
import Servant.API
import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readEither)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.=), (.:), object, encode)
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

-- parsing & rendering json

-- XXX implement ToJSON.toEncoding for better performance
instance ToJSON Transaction where
  toJSON (Transaction time balances) =
    object [T.pack "time" .= toJSON time,
            T.pack "balances" .= toJSON bs]
    where bs = [object [T.pack "account" .= toJSON a,
                        T.pack "balance" .= toJSON b]
               | (a,b) <- M.assocs balances]

instance FromJSON Transaction where
  parseJSON (Object o) =
    do t <- o .: T.pack "time" >>= parseJSON
       bs <- o .: T.pack "balances" >>= mapM parsePair
       return $ Transaction t (M.fromList bs)
    where
      parsePair (Object o) =
        do a <- o .: T.pack "account" >>= parseJSON
           b <- o .: T.pack "balance" >>= parseJSON
           return (a,b)
      parsePair _          = mzero
  parseJSON _          = mzero

instance ToJSON Balance where
  toJSON (Balance b) = Number (fromIntegral b)

instance FromJSON Balance where
  -- XXX check integerness
  parseJSON (Number b) = return . Balance . round $ b
  parseJSON _          = mzero

instance ToJSON Account where
  toJSON (Account n) = String (T.pack n)

instance FromJSON Account where
  parseJSON (String n) = return . Account . T.unpack $ n
  parseJSON _          = mzero

instance ToJSON Timestamp where
  -- XXX strings instead?
  toJSON (Timestamp t) = Number (fromIntegral t)

instance FromJSON Timestamp where
  -- XXX check integerness
  parseJSON (Number t) = return . Timestamp . round $ t
  parseJSON _          = mzero
