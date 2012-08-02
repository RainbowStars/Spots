{-# OPTIONS_GHC -XNoMonomorphismRestriction#-}
module Types where

-- program-wide types and also miscellaneous functions

import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.IORef
import Happstack.Server(Conf, nullConf)
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe

data SpotsConf = SpotsConf {
    httpConf    :: Conf,
    rootDir     :: FilePath,
    thumbsDir   :: FilePath,
    thumbSize   :: (Int, Int),   -- maximum (height, width)
    thumbJPEGQuality    :: Int
}

data ImageNode = Image {
    imagePath   :: FilePath,
    imageCaption    :: String
}              | Directory {
    dirPath     :: FilePath,
    dirCaption  :: String,
    dirCover    :: FilePath
}

data ImageType = GIF
               | JPG
               | PNG
                 deriving (Show, Read, Eq, Ord, Enum, Bounded)     -- TODO add more image types? GD only supports these three so maybe not but some kind of image type abstraction would be nice

defaultHTTPConf :: Conf
defaultHTTPConf = nullConf

defaultSpotsConf :: SpotsConf
defaultSpotsConf = SpotsConf {
    httpConf    = defaultHTTPConf,
    rootDir     = ".",
    thumbsDir   = "thumbs",
    thumbSize   = (175, 175),
    thumbJPEGQuality    = 100
}

{-# NOINLINE spotsConfIO #-}
spotsConfIO :: IORef SpotsConf
spotsConfIO = unsafePerformIO (newIORef (defaultSpotsConf))

{-# NOINLINE spotsConf #-}
spotsConf :: SpotsConf
spotsConf = unsafePerformIO (readIORef spotsConfIO)

-- compute the path to a node's thumbnail
thumbPath' :: ImageNode -> FilePath
thumbPath' (Image path _) = (thumbsDir spotsConf) </> path
thumbPath' (Directory path _ cover) = (thumbsDir spotsConf) </> path </> cover

thumbPath :: FilePath -> FilePath
thumbPath path = (thumbsDir spotsConf) </> path

{-# INLINE imageExtensions #-}
imageExtensions :: [String]
imageExtensions = ["jpg", "jpeg", "gif", "png"]

imageType :: FilePath -> Maybe ImageType
imageType file = case extension of
    "jpg"   -> Just JPG
    "jpeg"  -> Just JPG
    "gif"   -> Just GIF
    "png"   -> Just PNG
    _       -> Nothing
    where
        extension = let
            tail [] = []
            tail xs = Prelude.tail xs in
                toLowerS (tail (takeExtension file))

isDirectory :: FilePath -> IO Bool
isDirectory path = doesDirectoryExist path

isImage :: FilePath -> Bool
isImage file = case imageType file of
    Nothing -> False
    _       -> True

isSpecial :: FilePath -> Bool
isSpecial path = or (map (path ==) [
    ".",
    "..",
    ".DS_Store",
    (thumbsDir spotsConf)])

toLowerS :: String -> String
toLowerS = map toLower