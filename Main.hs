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
import qualified Text.Blaze.Html5.Attributes as Attr
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
  , dir "static" $ serveDirectory EnableBrowsing ["index.html"] "static"
  , nullDir >> homePage db]

template :: String -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
      H.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css"
        ! Attr.media "screen"
        ! Attr.href "/static/bootstrap/css/bootstrap.min.css"
      H.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css"
        ! Attr.href "/static/style.css"
    H.body $ do
      H.div ! Attr.id "header" $ do
        H.h1 ! Attr.class_ "title" $ H.a ! Attr.href "/" $ do
          H.img ! Attr.src "/static/buffalo_small.png"
          "NIKLS"
        H.h2 $ toHtml title
      H.div ! Attr.id "body" $ do
        body
      H.div ! Attr.id "footer" $
        "footer footer footer footer"

homePage db = do
  g <- liftIO $ viewDebtGraph db
  let bal = unBalances $ balances g
  let pro = propagateDebts g
  let people = M.keys g
  ok $ template "Home Page" $ do
    H.h3 "Balances"
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
    H.h3 "More"
    H.ul $ H.li $ a ! Attr.href "/transaction/list" $ "transactions"

redir_to_transaction :: TransactionID -> ServerPart Response
redir_to_transaction id =
  tempRedirect ("/transaction/show/"++show id)
  (toResponse ("" :: String))

transaction_add_form :: Html
transaction_add_form = H.div ! Attr.class_ "addform" $ do
  H.h3 "Add transaction"
  H.form ! Attr.method "GET" ! Attr.action "/transaction/add" $ do
    H.p $ do H.label "Payer: "
             H.input ! Attr.type_ "text" ! Attr.name "payer" ! Attr.size "10"
             H.label "Benefitors: "
             H.input ! Attr.type_ "text" ! Attr.name "benefitors" ! Attr.size "40"
             H.label "Sum: "
             H.input ! Attr.type_ "text" ! Attr.name "sum" ! Attr.size "5"
    H.p $ do H.label "Description: "
             H.input ! Attr.type_ "text" ! Attr.name "description" ! Attr.size "40"
    H.p $ H.button ! Attr.type_ "submit" $ "Transact"

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
  H.form ! Attr.method "GET"
  ! Attr.action (H.toValue $ "/transaction/delete/"++show id) $
  H.p $ H.button ! Attr.type_ "submit" ! Attr.name "delete" $
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
    H.h3 "Balance: "
    H.p $ toHtml (fromJust $ M.lookup person bal)
    H.h3 "Debts: "
    H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person g) $
      \(p',balance) -> H.li $ do
        toHtml p'
        " : "
        toHtml balance
    H.h3 "Propagated debts: "
    H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person opt) $
      \(p',balance) -> H.li $ do
        toHtml p'
        " : "
        toHtml balance
    H.h3 "Transactions: "
    H.ul $ forM_ ts (H.li . toHtml)