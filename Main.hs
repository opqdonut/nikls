{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Domain
import Store

import Control.Applicative (optional)
import Control.Exception
import Control.Monad (msum, forM_)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Server
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = bracket openDatabase
               closeDatabase
               (\db -> simpleHTTP nullConf (route db))

route :: DB -> ServerPart Response
route db = msum
  [ dir "transactions" $ page_transactions db
  , dir "debtgraph" $ page_debtgraph db
  , homePage ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage = ok $ template "Home Page" $ do
  H.h1 "Hello!"
  H.p $ a ! href "/transactions" $ "transactions"
  H.p $ a ! href "/debtgraph" $ "debtgraph"

page_transactions :: DB -> ServerPart Response
page_transactions db = do
  ts <- liftIO $ viewTransactions db
  ok $ template "Transactions" $ do
    H.ul $ forM_ ts (H.li . toHtml . show)

page_debtgraph :: DB -> ServerPart Response
page_debtgraph db = do
  g <- liftIO $ viewDebtGraph db
  ok $ template "Debt Graph" $ do
    H.p $ toHtml (show g)
