{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XFlexibleContexts#-}
module Main where

import Image
import Template
import Types

import Control.Concurrent
import Control.Monad
import Data.IORef
import Happstack.Server -- hiding (Conf)
--import qualified Happstack.Server(Conf) as S
--import Text.Blaze.Renderer.Utf8
import System.Directory
import System.IO.Unsafe
import System.Log.Logger
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html.Renderer.Pretty

main :: IO ()
main = do
    conf <- return SpotsConf {
        httpConf    = nullConf,
        rootDir     = "/Users/Stars/src/spots/test2",
        thumbsDir   = "thumbs",
        thumbSize   = (175, 175),
        thumbJPEGQuality    = 100
    }    -- FIXME parse this out of getOpts
    writeIORef spotsConfIO conf
    setCurrentDirectory (rootDir spotsConf)
    createDirectoryIfMissing True (thumbsDir spotsConf)
    simpleHTTP (httpConf spotsConf) route

route :: ServerPartT IO Response
route = msum [
    nullDir >> uriRest spots,
    trailingSlash >> uriRest spots,
    serveDirectory DisableBrowsing [] (rootDir spotsConf)] 

spots :: FilterMonad Response m => t -> m Response
spots request = do     -- TODO do some kind of intelligent routing based on request
    ok . toResponse . preEscapedString . renderHtml $ index