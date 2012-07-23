{-# LANGUAGE OverloadedStrings #-}
module Main where

import Template
import Types

import Control.Concurrent
import Control.Monad
import Happstack.Server -- hiding (Conf)
--import qualified Happstack.Server(Conf) as S
--import Text.Blaze.Renderer.Utf8
import System.Directory
import System.IO.Unsafe
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html.Renderer.Pretty

defaultHTTPConf :: Conf
defaultHTTPConf = nullConf

defaultSpotsConf :: SpotsConf
defaultSpotsConf = SpotsConf {
    httpConf    = defaultHTTPConf,
    rootDir     = ".",
    thumbsDir   = "thumbs",
    thumbSize   = (175, 175)
}

main :: IO ()
main = do
    conf <- return SpotsConf {
        httpConf    = nullConf,
        rootDir     = "/Users/Stars/src/spots/test2",
        thumbsDir   = "thumbs",
        thumbSize   = (175, 175)
    }    -- FIXME parse this out of getOpts
    setCurrentDirectory (rootDir conf)
    createDirectoryIfMissing True (thumbsDir conf)
    simpleHTTP (httpConf conf) (route conf)

--route :: SpotsConf -> 
route conf = msum [
    uriRest (spots conf),
    serveDirectory DisableBrowsing [] (rootDir conf)] 

spots conf request = do
    return $ putStrLn (show request)
    ok . toResponse . preEscapedString . renderHtml $ (index conf)