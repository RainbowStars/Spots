{-# LANGUAGE OverloadedStrings #-}
module Template where

import Types

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
        spotsHeader
        thumbs conf))

spotsHeader :: Html
spotsHeader = do
    div ! id "header" $ do
        p "hello"

thumbs :: SpotsConf -> Html
thumbs conf = toHtml (unsafePerformIO (do       -- generate list of thumbs  -- FIXME unsafe IO
    fileList <- getDirectoryContents "."        -- we should have cd'd to the rootDir in the main function
    return (map (thumb conf)
        (filter (\file -> or [
            (isImage file),
            (and [isDirectory file,
                (not (isSpecial conf file))])])
        fileList)))) where
        isDirectory file = unsafePerformIO (doesDirectoryExist file)    -- FIXME unsafe IO
        isImage file = or (map (\extension -> isSuffixOf extension file) imageExtensions)
        isSpecial conf file = or (map (\file' -> file == file') [
            ".",
            "..",
            ".DS_Store",
            (thumbsDir conf)])

thumb :: SpotsConf -> FilePath -> Html
thumb conf file = do        -- html for a single thumbnail box containing thumbnail and caption
    div ! id "thumb" $ do
        a ! href file' $ do
            img ! src thumbPath ! alt file'
        p (string file) where
        file' = stringValue file
        thumbPath = stringValue (((thumbsDir conf) ++ "/") ++ file'') where
            file'' = case isDirectory of
                True -> file ++ "/cover.jpg"
                False -> file
            isDirectory = unsafePerformIO (doesDirectoryExist file) -- FIXME unsafe IO