{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid(mempty, mconcat)
import Control.Arrow (arr, (>>>))
import Control.Monad (forM_)

import Hakyll

import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..), defaultWriterOptions)

main :: IO ()
main = hakyll $ do
         -- CSS
         match "css/*" $ do
                  route idRoute
                  compile compressCssCompiler

         -- Favicon
         match "favicon.ico" $ do
                  route idRoute
                  compile copyFileCompiler

         -- Images
         match "img/*" $ do
                  route idRoute
                  compile copyFileCompiler

         -- Files
         match "files/*" $ do
                  route idRoute
                  compile copyFileCompiler

         -- JavaScript
         match "js/*" $ do
                  route idRoute
                  compile copyFileCompiler

         -- Templates
         match "templates/*" $ compile templateCompiler

         -- Index
         forM_ ["index.md", "404.md"] $ \page ->
             match page $ do
                  route $ setExtension "html"
                  compile $ myPageCompiler
                          >>> applyTemplateCompiler "templates/index.html"
                          >>> applyTemplateCompiler "templates/base.html"
                          >>> relativizeUrlsCompiler

         -- Posts
         match "blog/*" $ do
                  route $ setExtension ".html"
                  compile $ pageCompiler
                   >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
                   >>> renderTagsField "prettytags" (fromCapture "tags/*")
                   >>> applyTemplateCompiler "templates/entry.html"
                   >>> applyTemplateCompiler "templates/base.html"

         -- Post list
         match "entries.html" $ route idRoute
         create "entries.html" $ constA mempty
                >>> arr (setField "title" "Posts")
                >>> requireAllA "blog/*" addPostList
                >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                >>> applyTemplateCompiler "templates/entries.html"
                >>> applyTemplateCompiler "templates/base.html"

         -- Tags
         create "tags" $
                requireAll "blog/*" (\_ ps -> readTags ps :: Tags String)

         -- Add a tag list compiler for every tag
         match "tags/*" $ route $ setExtension ".html"
         metaCompile $ require_ "tags"
                         >>> arr tagsMap
                         >>> arr (map (\(t, p) ->
                                           (tagIdentifier t, makeTagList t p)))

         -- RSS
         match "rss.xml" $ route idRoute
         create "rss.xml" $ requireAll_ "blog/*"
                    >>> renderRss feedConfiguration

    where

      renderTagCloud' :: Compiler (Tags String) String
      renderTagCloud' = renderTagCloud tagIdentifier 100 120

      tagIdentifier :: String -> Identifier
      tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "entries" $
    arr (reverse . sortByBaseName)
        >>> require "templates/entryitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged '" ++ tag ++ "'"))
        >>> applyTemplateCompiler "templates/tags.html"
        >>> applyTemplateCompiler "templates/base.html"

-- | Make Pandoc and MathJax play together.
myPageCompiler :: Compiler Resource (Page String)
myPageCompiler = pageCompilerWith defaultHakyllParserState myWriterOptions

myWriterOptions :: WriterOptions
myWriterOptions = defaultWriterOptions { writerLiterateHaskell = True
                                       , writerHTMLMathMethod = MathJax "" }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Churning and Churning"
    , feedDescription = "Much wailing and gnashing of teeth"
    , feedAuthorName = "Mikhail Glushenkov"
    , feedRoot = "http://dissocial.st"
    }
