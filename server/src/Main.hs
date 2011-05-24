{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Types

rootFile :: FilePath
rootFile = "static/entries.html"

fileNotFound :: Snap ()
fileNotFound =  do modifyResponse $ setResponseStatus 404 "File Not Found"
                   serveFile "static/404.html"

site :: Snap ()
site = route [ ("/blog", serveFile rootFile)
             , ("/blog/:id", serveHtmlFile "static/blog")
             , ("/tags", serveFile rootFile)
             , ("/tags/:id", serveHtmlFile "static/tags")
             ]
       <|> serveDirectory "static"
       <|> fileNotFound

serveHtmlFile :: B.ByteString -> Snap ()
serveHtmlFile pth = do
  fileId <- decodedParam "id"
  case fileId of
    "" -> serveFile rootFile
    fi -> serveFile . C.unpack $ B.concat [pth, "/", mkFileId fi]
    where
      decodedParam par = fromMaybe "" <$> getParam par
      mkFileId n = if ".html" `B.isSuffixOf` n
                   then n
                   else B.concat [n, ".html"]


main :: IO ()
main = quickHttpServe site
