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
import Data.Time.Clock (getCurrentTime)
import Happstack.Server hiding (method)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (href, type_, name, size, action, method)
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = bracket openDatabase
               closeDatabase
               (\db -> simpleHTTP nullConf (route db))

route :: DB -> ServerPart Response
route db = msum
  [ dirs "transaction/list" $ page_transaction_list db
  , dirs "transaction/add" $ page_transaction_add db
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
  let pro = propagateDebts g
  let people = M.keys g
  ok $ template "Home Page" $ do
    H.h1 "Balances"
    H.ul $ forM_ people $ \p -> H.li $ do
      toHtml p
      toHtml (" : " :: String)
      toHtml (M.findWithDefault (Balance 0) p bal)
      H.ul $ H.li $ forM_ (M.assocs $ M.findWithDefault M.empty p g) $ \(p',balance) -> do
        toHtml p'
        toHtml (" : " :: String)
        toHtml balance
        toHtml (", " :: String)
      H.ul $ H.li $ forM_ (M.assocs $ M.findWithDefault M.empty p pro) $ \(p',balance) -> do
        toHtml p'
        toHtml (" : " :: String)
        toHtml balance
        toHtml (", " :: String)

    transaction_add_form

    H.p $ a ! href "/transaction/list" $ "transactions"

transaction_add_form :: Html
transaction_add_form = do
  H.form ! method "GET" ! action "/transaction/add" $ do
    H.p $ do H.label "Payer: " >> H.input ! type_ "text" ! name "payer" ! size "10"
             H.label "Benefitors: " >> H.input ! type_ "text" ! name "benefitors" ! size "40"
             H.label "Sum: " >> H.input ! type_ "text" ! name "sum" ! size "5"
    H.p $ H.label "Description: " >> H.input ! type_ "text" ! name "description" ! size "40"
    H.p $ H.input ! type_ "submit" ! name "add"

page_transaction_add :: DB -> ServerPart Response
page_transaction_add db = do
  payer <- look "payer"
  benefitors_ <- look "benefitors"
  let benefitors = words benefitors_
  sum_ <- look "sum"
  let sum = read sum_ :: Int
  description <- look "description"
  time <- liftIO getCurrentTime
  let st = SimpleTransaction (Person payer) (map Person benefitors)
           (Balance sum) description time
  liftIO $ addTransaction db st
  ok $ template "Transaction added!" $
    toHtml st

page_transaction_list :: DB -> ServerPart Response
page_transaction_list db = do
  ts <- liftIO $ viewTransactions db
  ok $ template "Transactions" $ do
    H.ul $ forM_ ts (H.li . toHtml)
