{-# LANGUAGE OverloadedStrings #-}
module Template.Thumbs where

import Types

import Prelude hiding (head, id, div)
import Data.List hiding (head)
import System.Directory
import System.FilePath ((</>))
import System.IO.Unsafe
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- returns (IO) list of thumbnail html, NOINLINE so it calls this every time a request is made (in case the contents of the directory change) FIXME should probably do some kind of caching
{-# NOINLINE thumbs #-}
thumbs :: FilePath -> IO [Html]
thumbs path = do
    fileList <- getDirectoryContents path
    return (map thumb (filter isValid fileList)) where
            isValid file = or [isImage file,
                               and [unsafePerformIO (doesDirectoryExist file),    -- FIXME unsafe IO
                                    (not (isSpecial file))]]

-- html for a single thumbnail box containing thumbnail and caption
--{-# NOINLINE thumb #-}
thumb :: FilePath -> Html
thumb file = do
    div ! id "thumb" $ do
        a ! href file' $ do
            img ! src thumbPath' ! alt file'
        p (string file) where
        file' = stringValue file
        thumbPath' = stringValue ((thumbsDir spotsConf) </> file'') where
            file'' = case isDirectory of
                True -> file ++ "/cover.jpg"
                False -> file
            isDirectory = unsafePerformIO (doesDirectoryExist file) -- FIXME unsafe IO