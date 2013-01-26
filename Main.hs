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
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Time.Clock (getCurrentTime)
import Happstack.Server hiding (method)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (href, type_, name, size, action, method, value)
import qualified Text.Blaze.Html5 as H

main :: IO ()
main = bracket openDatabase
               closeDatabase
               (\db -> simpleHTTP nullConf (route db))

route :: DB -> ServerPart Response
route db = msum
  [ dirs "transaction/list" $ page_transaction_list db
  , dirs "transaction/add" $ page_transaction_add db
  , dirs "transaction/delete" $ page_transaction_delete db
  , dirs "transaction/show" $ page_transaction_show db
  , dir "person" $ page_person db
  , nullDir >> homePage db]

template :: String -> Html -> Response
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
      H.ul $ H.li $ forM_ (M.assocs $ fromJust $ M.lookup p g) $ \(p',balance) -> do
        toHtml p'
        toHtml (" : " :: String)
        toHtml balance
        toHtml (", " :: String)
      H.ul $ H.li $ forM_ (M.assocs $ fromJust $ M.lookup p pro) $ \(p',balance) -> do
        toHtml p'
        toHtml (" : " :: String)
        toHtml balance
        toHtml (", " :: String)

    transaction_add_form

    H.p $ a ! href "/transaction/list" $ "transactions"

redir_to_transaction :: TransactionID -> ServerPart Response
redir_to_transaction id =
  tempRedirect ("/transaction/show/"++show id)
  (toResponse ("" :: String))

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
  stored <- liftIO $ addTransaction db st
  redir_to_transaction (transactionid stored)

page_transaction_list :: DB -> ServerPart Response
page_transaction_list db = do
  ts <- liftIO $ viewTransactions db
  ok $ template "Transactions" $ do
    H.ul $ forM_ ts (H.li . toHtml)

transaction_delete_form :: TransactionID -> Html
transaction_delete_form id =
  H.form ! method "GET" ! action (H.toValue $ "/transaction/delete/"++show id) $
  H.p $ H.button ! type_ "submit" ! name "delete" $
  toHtml ("Delete transaction "++show id)

page_transaction_show :: DB -> ServerPart Response
page_transaction_show db = path $ \id -> do
  ts <- liftIO $ viewTransactions db
  let [t] = filter ((==id).transactionid) ts
  ok $ template ("Transaction " ++ show id) $ do
    H.p $ toHtml t
    transaction_delete_form id

page_transaction_delete :: DB -> ServerPart Response
page_transaction_delete db = path $ \id -> do
  liftIO $ deleteTransaction db id
  redir_to_transaction id

page_person db = path $ \p -> do
  let person = Person p
  ts <- liftIO $ viewTransactionsFor db person
  g <- liftIO $ viewDebtGraph db
  let bal = unBalances $ balances g
  let opt = propagateDebts g
  ok $ template ("Person " ++ p) $ do
    H.p $ do "Balance: "
             toHtml (fromJust $ M.lookup person bal)
    H.p $ do "Debts: "
             H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person g) $
               \(p',balance) -> H.li $ do
                 toHtml p'
                 " : "
                 toHtml balance
    H.p $ do "Propagated debts: "
             H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person opt) $
               \(p',balance) -> H.li $ do
                 toHtml p'
                 " : "
                 toHtml balance
    H.p $ do "Transactions: "
             H.ul $ forM_ ts (H.li . toHtml)