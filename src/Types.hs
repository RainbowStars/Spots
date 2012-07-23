module Types where

import Happstack.Server(Conf)

data SpotsConf = SpotsConf {
    httpConf    :: Conf,
    rootDir     :: FilePath,
    thumbsDir   :: FilePath,
    thumbSize   :: (Int, Int)   -- maximum (height, width)
}

data ImageNode = Image {
    imagePath   :: FilePath,
    imageCaption    :: String
}              | Directory {
    dirPath     :: FilePath,
    dirCaption  :: String,
    dirCover    :: FilePath
}

imageExtensions = ["jpg", "jpeg", "gif", "png", "bmp", "tif", "tiff"]