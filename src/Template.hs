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

index :: SpotsConf -> Html
index conf = html (do
    head (do
        H.title "some kind of title should go here I guess"
        link ! href "Spots.css" ! rel "stylesheet")
    body (do
        toHtml . unsafePerformIO $ (thumbs conf (rootDir conf))))   -- FIXME unsafe IO

thumbs :: SpotsConf -> FilePath -> IO [Html]
thumbs conf path = do
    fileList <- getDirectoryContents path
    return (map (thumb conf) (filter isValid fileList)) where
            isValid file = or [isImage file,
                        and [unsafePerformIO (isDirectory file),    -- FIXME unsafe IO
                       (not (isSpecial conf file))]]