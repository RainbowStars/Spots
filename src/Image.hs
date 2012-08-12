module Image where

import Types

import Control.Monad
import Control.Monad.Maybe
import Graphics.GD
import System.Directory
import System.IO.Unsafe

-- TODO abstract all of this load<filetype>File nonsense away

-- generate all of the thumbnails from the rootDir and put them in the thumbsDir
-- TODO there should probably be some kind of more comprehensive test for checking if a thumbnail exists and also is correct (of the right dimensions). This is important if thumbSize ever changes I guess ????????
-- FIXME unsafe IO
--generateThumbs :: FilePath -> IO ()
generateThumbs path = do
    contents <- return (filter isValidImageOrDirectory (unsafePerformIO (getDirectoryContents path)))
    isDirectory <- doesDirectoryExist path
    isFile <- doesFileExist path
    case (isDirectory, isFile) of
        (True, False)   -> head $ map generateThumbs contents
        (False, True)   -> generateThumb path (thumbPath path)
        (False, False)  -> return ()  -- FIXME generate some kind of log message for calling this function on a nonexistant filepath
        (_, _)           -> return () -- FIXME generate log message on this ???? it shouldn't ever happen but I guess it's technically possible

-- generate a single thumb
--generateThumb :: FilePath -> FilePath -> MaybeT IO ()
generateThumb src dest = do
    withImage (loadImage src) (thumb dest)
     where
        thumb :: FilePath -> Image -> IO ()
        thumb dest image = do
            resizeImage x y image
            saveImage image dest where
                (x, y) = thumbSize spotsConf

loadImage :: FilePath -> IO Image
loadImage path = case (imageType path) of
    Just JPG -> loadJpegFile path
    Just PNG -> loadPngFile path
    Just GIF -> loadGifFile path
    Nothing -> error "uh oh something broke"

saveImage :: Image -> FilePath -> IO ()
saveImage image path = case (imageType path) of
    Just JPG -> saveJpegFile (thumbJPEGQuality spotsConf) path image
    Just PNG -> savePngFile path image
    Just GIF -> saveGifFile path image
    Nothing -> error "uh oh something broke"