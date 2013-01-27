{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Domain
import Store
import View

import Control.Applicative (optional, (<$>))
import Control.Exception
import Control.Monad (msum, forM_, when)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Data.Time.Clock (getCurrentTime)
import Happstack.Server hiding (method, escape)
import Happstack.Server.SURI (escape)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Text.Blaze.Html5 as H

-- XXX _disallow_ uploads
bodyPolicy = defaultBodyPolicy "/tmp/" 4096 4096 4096

main :: IO ()
main = bracket openDatabase
               closeDatabase
               (\db -> simpleHTTP nullConf (route db))

route :: DB -> ServerPart Response
route db = do
  decodeBody bodyPolicy
  msum
    [ dirs "transaction/list" $ page_transaction_list db
    , dirs "transaction/add" $ page_transaction_add db
    , dirs "transaction/delete" $ page_transaction_delete db
    , dirs "transaction/show" $ page_transaction_show db
    , dir "person" $ page_person db
    , dir "set_password" $ page_set_password db
    , dir "static" $ serveDirectory EnableBrowsing ["index.html"] "static"
      -- XXX require ssl
    , dir "login" $ page_login db
    , nullDir >> page_home db]

alert :: Maybe String -> Maybe String -> Html
alert Nothing _ = toHtml ("" :: String) -- XXX emptyHtml?
alert (Just msg) typ = H.div ! Attr.class_ cl $ toHtml msg
  where cl = H.toValue $ case typ of Nothing -> "alert"
                                     Just typ -> "alert alert-"++typ

mkPage :: String -> Html -> ServerPart Response
mkPage title body = do
  msg <- optional $ look "msg"
  msgType <- optional $ look "msgtype"
  ok $ toResponse $
    H.html $ do
      H.head $ do
        H.title (toHtml title)
        H.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css"
          ! Attr.media "screen"
          ! Attr.href "/static/bootstrap/css/bootstrap.min.css"
        H.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css"
          ! Attr.href "/static/style.css"
      H.body $ do
        H.div ! Attr.class_ "container" $ do
          alert msg msgType
          H.div ! Attr.id "page-header" $ do
            H.h1 ! Attr.class_ "title" $ H.a ! Attr.href "/" $ do
              H.img ! Attr.src "/static/buffalo_small.png"
              " NIKLS "
              H.small $ "NIKLS Is Kinda Like Scred"
            H.h2 $ toHtml title
          H.div ! Attr.id "body" $ do
            body
          H.div ! Attr.id "footer" $
            "footer footer footer footer"

page_home db = do
  g <- liftIO $ viewDebtGraph db
  let bal = unBalances $ balances g
  let pro = propagateDebts g
  let people = M.keys g
  mkPage "Home Page" $ do
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
    H.ul $ do H.li $ a ! Attr.href "/transaction/list" $ "transactions"
              H.li $ a ! Attr.href "/login" $ "login"

redir_with_message :: String -> String -> String -> ServerPart Response
redir_with_message addr msg msgtype =
  -- XXX use Network.URI
  let uri = addr++"?msg="++escape msg++"&msgtype="++escape msgtype
  in tempRedirect uri
     (toResponse ("" :: String))

page_login :: DB -> ServerPart Response
page_login db = do
  passwords_ <- liftIO $ viewPasswords db
  let passwords = M.mapKeys personName passwords_
  basicAuth "NIKLS" passwords $
    redir_with_message "/" "Login succesful!" "success"

transaction_add_form :: Html
transaction_add_form = H.div ! Attr.class_ "well" $ do
  H.h3 "Add transaction"
  H.form ! Attr.method "GET" ! Attr.action "/transaction/add"
    ! Attr.class_ "form-inline" $ do
    H.p $ do H.input ! Attr.type_ "text" ! Attr.name "payer"
               ! Attr.class_ "input-small" ! Attr.placeholder "Payer"
             H.input ! Attr.type_ "text" ! Attr.name "benefitors"
               ! Attr.placeholder "Benefitors"
             H.input ! Attr.type_ "text" ! Attr.name "sum"
               ! Attr.class_ "input-small" ! Attr.placeholder "Sum"
    H.p $ do H.input ! Attr.type_ "text" ! Attr.name "description"
               ! Attr.placeholder "Description"
               ! Attr.class_ "input-xlarge"
             H.button ! Attr.type_ "submit" ! Attr.class_ "btn" $ "Transact"

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
  redir_with_message "/" "Transaction added!" "success"

page_transaction_list :: DB -> ServerPart Response
page_transaction_list db = do
  ts <- liftIO $ viewTransactions db
  mkPage "Transactions" $ do
    H.ul $ forM_ ts (H.li . toHtml)

transaction_delete_form :: TransactionID -> Html
transaction_delete_form id =
  H.form ! Attr.method "GET"
  ! Attr.action (H.toValue $ "/transaction/delete/"++show id) $
  H.p $ H.button ! Attr.type_ "submit" ! Attr.name "delete"
  ! Attr.class_ "btn" $ do
    H.i ! Attr.class_ "icon-remove" $ ""
    toHtml (" Delete transaction "++show id)

page_transaction_show :: DB -> ServerPart Response
page_transaction_show db = path $ \id -> do
  ts <- liftIO $ viewTransactions db
  let [t] = filter ((==id).transactionid) ts
  mkPage ("Transaction " ++ show id) $ do
    H.p $ toHtml t
    transaction_delete_form id

page_transaction_delete :: DB -> ServerPart Response
page_transaction_delete db = path $ \id -> do
  liftIO $ deleteTransaction db id
  redir_with_message ("/transaction/show/"++show id)
    "Transaction deleted!" "success"

claim_person_form :: Person -> Html
claim_person_form p =
  H.form ! Attr.method "POST"
  ! Attr.action (H.toValue $ "/set_password/"++personName p)
  $ H.p $ do
    "Are you " >> toHtml p >> "?"
    H.input ! Attr.type_ "password" ! Attr.name "password"
      ! Attr.placeholder "Choose a password!"

page_person db = path $ \p -> do
  let person = Person p
  ts <- liftIO $ viewTransactionsFor db person
  g <- liftIO $ viewDebtGraph db
  let bal = unBalances $ balances g
  let opt = propagateDebts g
  pw <- fmap (M.lookup person) $ liftIO $ viewPasswords db
  mkPage ("Person " ++ p) $ do
    when (isNothing pw) $ claim_person_form person
    H.h3 "Balance: "
    H.p ! Attr.class_ "lead" $ toHtml (fromJust $ M.lookup person bal)
    H.h3 "Breakdown: "
    H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person g) $
      \(p',balance) -> H.li $ do
        toHtml p'
        " : "
        toHtml balance
    H.h3 "Propagated: "
    H.ul $ forM_ (M.assocs $ fromJust $ M.lookup person opt) $
      \(p',balance) -> H.li $ do
        toHtml p'
        " : "
        toHtml balance
    H.h3 "Transactions: "
    H.ul $ forM_ ts (H.li . toHtml)

page_set_password :: DB -> ServerPart Response
page_set_password db = path $ \person -> do
  let p = Person person
  passwords <- liftIO $ viewPasswords db

  password <- look "password"
  if not (M.member p passwords)
    then do liftIO $ setPassword db p password
            redir_with_message ("/person/"++person) "Password set!" "success"
    else unauthorized $ toResponse ("Rejected." :: String)