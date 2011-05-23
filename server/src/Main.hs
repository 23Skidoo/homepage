{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Types

site :: Snap ()
site = route [ ("/blog", serveFile "static/entries.html")
             , ("/blog/:entry",  serveBlogEntry "static/blog")
             ]
       <|> serveDirectory "static"
       <|> serveFile "static/404.html"

serveBlogEntry :: B.ByteString -> Snap ()
serveBlogEntry p = do postName <- decodedParam "entry"
                      let postName' = mkPostName postName
                      serveFile . C.unpack $ B.concat [p, "/", postName']
    where
      decodedParam par = fromMaybe "" <$> getParam par
      mkPostName n = if ".html" `B.isSuffixOf` n
                     then n
                     else B.concat [n, ".html"]




main :: IO ()
main = quickHttpServe site
