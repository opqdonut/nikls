{-# Language OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
  [ dir "echo" $ echo
  , dir "query" $ queryParams
  , homePage ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage = ok $ template "home page" $ do
  H.h1 "Hello!"
  H.p "Lorem ipsum"
  H.p $ a ! href "/echo/secret%20message" $ "echo"
  H.p $ a ! href "/query?foo=bar" $ "query parameters"

echo = path $ \(msg :: String) ->
  ok $ template "echo" $ do
    p $ "echo says :" >> toHtml msg
    p "Change the url to echo something else."

queryParams :: ServerPart Response
queryParams =
  do mFoo <- optional $ lookText "foo"
     ok $ template "query params" $ do
       p $ "foo is set to: " >> toHtml (show mFoo)
       p $ "change the url to set it to something else."
