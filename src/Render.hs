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
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), Object, (.=), (.:), object, encode)
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M

-- parsing url frangments

instance FromHttpApiData Account where
  parseUrlPiece t = return $ Account (T.unpack t)

instance FromHttpApiData Timestamp where
  parseUrlPiece t = Timestamp <$> parseUrlPiece t

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
  toJSON (Balances b) = toJSON pairs
    where pairs = [object [T.pack "account" .= toJSON a,
                           T.pack "balance" .= toJSON b]
                  | (a,b) <- M.assocs b]

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

instance ToJSON Transaction where
  toJSON (Transaction time description cancelled positive negative) =
    object [T.pack "time" .= toJSON time,
            T.pack "description" .= toJSON description,
            T.pack "cancelled" .= toJSON cancelled,
            T.pack "positive" .= toJSON positive,
            T.pack "negative" .= toJSON negative]

instance FromJSON Transaction where
  -- TODO: enforce positive/negative
  parseJSON (Object o) =
    do t <- extract o "time"
       descr <- extract o "description"
       cancelled <- extract o "cancelled"
       pos <- extract o "positive"
       neg <- extract o "negative"
       return $ Transaction t descr cancelled pos neg
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
