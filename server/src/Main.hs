{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Types

site :: Snap ()
site = route [ ("/about", serveFile "static/about.html")
             , ("/code",  serveFile "static/code.html")
             ]
       <|> serveDirectory "static"
       <|> serveFile "static/404.html"


main :: IO ()
main = quickHttpServe site
