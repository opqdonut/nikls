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

-- parsing url fragments

instance FromHttpApiData Account where
  parseUrlPiece t = return $ Account (T.unpack t)

instance FromHttpApiData Id where
  parseUrlPiece t = Id <$> parseUrlPiece t

-- parsing & rendering json

extract :: FromJSON a => Object -> String -> Parser a
extract o f = o .: T.pack f >>= parseJSON

-- XXX implement ToJSON.toEncoding for better performance

instance ToJSON Sum where
  toJSON (Sum b) = Number (fromIntegral b)

instance FromJSON Sum where
  -- XXX check integerness
  parseJSON (Number b) = return . Sum . round $ b
  parseJSON _          = mzero

instance ToJSON Account where
  toJSON (Account n) = String (T.pack n)

instance FromJSON Account where
  parseJSON (String n) = return . Account . T.unpack $ n
  parseJSON _          = mzero

instance ToJSON Balances where
  toJSON (Balances bal) = toJSON pairs
    where pairs = [object [T.pack "account" .= toJSON a,
                           T.pack "balance" .= toJSON b]
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
  -- XXX check integerness
  parseJSON (Number t) = return . Timestamp . round $ t
  parseJSON _          = mzero

instance ToJSON Id where
  toJSON New = toJSON "New"
  toJSON (Id i) = toJSON i

instance FromJSON Id where
  -- XXX check integerness
  parseJSON (Number i) = return . Id . round $ i
  parseJSON (String s) = if (s == T.pack "New") then (return New) else mzero
  parseJSON _          = mzero

instance ToJSON Transaction where
  toJSON (Transaction tid time description cancelled positive negative) =
    object [T.pack "id" .= toJSON tid,
            T.pack "time" .= toJSON time,
            T.pack "description" .= toJSON description,
            T.pack "cancelled" .= toJSON cancelled,
            T.pack "positive" .= toJSON positive,
            T.pack "negative" .= toJSON negative]

instance FromJSON Transaction where
  -- TODO: enforce positive/negative
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
