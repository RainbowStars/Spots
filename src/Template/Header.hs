{-# LANGUAGE OverloadedStrings #-}
module Template.Header where

import Prelude hiding (div, head, id, span)
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

spotsHeader :: Html
spotsHeader = do
    H.span ! id "header" $ do
        p "hi"

spotsFooter :: Html
spotsFooter = do
    div ! id "footer" $ do
        p "bye"