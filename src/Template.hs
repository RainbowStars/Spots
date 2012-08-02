{-# LANGUAGE OverloadedStrings #-}
module Template where

import Types

import Template.Header
import Template.Thumbs

import Prelude hiding (head, id, div)
import Data.List hiding (head)
import System.Directory
import System.IO.Unsafe
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

{-# NOINLINE index #-}
index :: Html
index = html (do
    head (do
        H.title "some kind of title should go here I guess"
        link ! href "Spots.css" ! rel "stylesheet")
    body (do
        spotsHeader
        div $ do
            toHtml . unsafePerformIO $ (thumbs (rootDir spotsConf))))   -- FIXME unsafe IO