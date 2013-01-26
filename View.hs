module View where

import Domain

import Control.Monad (forM_)
import Text.Blaze (ToMarkup(..))
import Text.Blaze.Html5 (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)

instance ToMarkup Person where
  toMarkup (Person s) = H.span ! class_ (toValue "person") $
                        H.a ! href (toValue $ "/person/"++s) $
                        toHtml s

instance ToMarkup Balance where
  toMarkup (Balance b) = H.span ! class_ (toValue "balance") $
                         toHtml b

instance ToMarkup SimpleTransaction where
  toMarkup (SimpleTransaction payer benefitors sum descr time) =
    H.span ! class_ (toValue "transaction") $ do
      toHtml "Payer: "
      H.span ! class_ (toValue "payer") $ toHtml payer
      toHtml " Benefitors: "
      forM_ benefitors (\b -> H.span ! class_ (toValue "benefitor") $
                              toHtml b >> toHtml ", ")
      toHtml " Sum: "
      toHtml sum
      toHtml " "
      toHtml (show descr)
      toHtml " @ "
      toHtml (show time)
