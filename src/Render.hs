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

-- parsing & rendering json

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
        do a <- o .: T.pack "account" >>= parseJSON
           b <- o .: T.pack "balance" >>= parseJSON
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
  toJSON (Transaction time description positive negative) =
    object [T.pack "time" .= toJSON time,
            T.pack "description" .= toJSON description,
            T.pack "positive" .= toJSON positive,
            T.pack "negative" .= toJSON negative]

instance FromJSON Transaction where
  -- TODO: enforce positive/negative
  parseJSON (Object o) =
    do t <- o .: T.pack "time" >>= parseJSON
       descr <- o .: T.pack "description" >>= parseJSON
       pos <- o .: T.pack "positive" >>= parseJSON
       neg <- o .: T.pack "negative" >>= parseJSON
       return $ Transaction t descr pos neg
  parseJSON _          = mzero
