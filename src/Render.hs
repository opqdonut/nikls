-- various conversion instances for things in Model

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Render (Data.Aeson.ToJSON(..), Data.Aeson.FromJSON(..),
               toJSONString, fromJSONString)
  where

import Model

import Control.Monad
import Servant.API
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson (ToJSON(..), FromJSON(..),
                   Value(..), Object,
                   (.=), (.:), object,
                   encode, eitherDecode)
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import Data.String

-- parsing url fragments

instance FromHttpApiData Account where
  parseUrlPiece t = return $ Account (T.unpack t)

instance FromHttpApiData Id where
  parseUrlPiece t = Id <$> parseUrlPiece t

-- parsing & rendering json

extract :: FromJSON a => Object -> String -> Parser a
extract o f = o .: fromString f >>= parseJSON

-- XXX implement ToJSON.toEncoding for better performance

instance ToJSON Sum where
  toJSON (Sum b) = Number (fromIntegral b)

instance FromJSON Sum where
  parseJSON s = Sum <$> parseJSON s

instance ToJSON Account where
  toJSON (Account n) = String (T.pack n)

instance FromJSON Account where
  parseJSON (String n) = return . Account . T.unpack $ n
  parseJSON _          = mzero

instance ToJSON Balances where
  toJSON (Balances bal) = toJSON pairs
    where pairs = [object [fromString "account" .= toJSON a,
                           fromString "balance" .= toJSON b]
                  | (a,b) <- M.assocs bal]

instance FromJSON Balances where
  parseJSON arr@(Array _) =
    parseJSON arr >>= mapM parsePair >>= return . Balances . M.fromList
    where
      parsePair (Object o) =
        do a <- extract o "account"
           b <- extract o "balance"
           return (a,b)
      parsePair _          = mzero
  parseJSON _           = mzero

instance ToJSON Timestamp where
  -- XXX strings instead?
  toJSON (Timestamp t) = Number (fromIntegral t)

instance FromJSON Timestamp where
  parseJSON t = Timestamp <$> parseJSON t

instance ToJSON Id where
  toJSON New = toJSON "New"
  toJSON (Id i) = toJSON i

instance FromJSON Id where
  parseJSON (String s)   = if s == T.pack "New" then return New else mzero
  parseJSON i            = Id <$> parseJSON i

instance ToJSON Transaction where
  toJSON (Transaction tid time description cancelled positive negative) =
    object [fromString "id" .= toJSON tid,
            fromString "time" .= toJSON time,
            fromString "description" .= toJSON description,
            fromString "cancelled" .= toJSON cancelled,
            fromString "positive" .= toJSON positive,
            fromString "negative" .= toJSON negative]

instance FromJSON Transaction where
  -- XXX enforce positive/negative
  parseJSON (Object o) =
    do tid <- extract o "id"
       t <- extract o "time"
       descr <- extract o "description"
       cancelled <- extract o "cancelled"
       pos <- extract o "positive"
       neg <- extract o "negative"
       return $ Transaction tid t descr cancelled pos neg
  parseJSON _          = mzero

instance FromJSON SimpleTransaction where
  parseJSON (Object o) =
    do t <- extract o "time"
       descr <- extract o "description"
       s <- extract o "sum"
       payers <- extract o "payers"
       sharedBy <- extract o "shared_by"
       return $ SimpleTransaction t descr s payers sharedBy
  parseJSON _ = mzero

-- re-exports for testing

toJSONString :: ToJSON a => a -> String
toJSONString = BS.unpack . encode

fromJSONString :: FromJSON a => String -> Either String a
fromJSONString = eitherDecode . BS.pack
