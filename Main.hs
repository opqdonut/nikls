{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Domain
import Store
import View

import Control.Applicative (optional, (<$>))
import Control.Exception
import Control.Monad (msum, forM_)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as M
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
  , homePage db]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage db = do
  g <- liftIO $ viewDebtGraph db
  let bal = unBalances $ balances g
  ok $ template "Home Page" $ do
    H.h1 "Balances"
    H.ul $ forM_ (M.assocs $ g) $ \(p,status) -> H.li $ do
      toHtml p
      toHtml (" : " :: String)
      toHtml (M.findWithDefault (Balance 0) p bal)
      H.ul $ forM_ (M.assocs $ status) $ \(p',balance) -> H.li $ do
        toHtml p'
        toHtml (" : " :: String)
        toHtml balance
    H.p $ a ! href "/transactions" $ "transactions"
    H.p $ a ! href "/debtgraph" $ "debtgraph"

page_transactions :: DB -> ServerPart Response
page_transactions db = do
  ts <- liftIO $ viewTransactions db
  ok $ template "Transactions" $ do
    H.ul $ forM_ ts (H.li . toHtml)
