{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow (arr, (>>>))
import Control.Monad (forM_)
import Data.List(isPrefixOf, isSuffixOf)
import Data.Monoid(mappend, mempty, mconcat)
import System.FilePath (dropExtension)

import Data.Map((!))
import qualified Data.Map as M

import Hakyll

import Text.HTML.TagSoup
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

         -- Templates
         match "templates/*" $ compile templateCompiler

         -- Index
         forM_ ["index.md", "404.md"] $ \page ->
             match page $ do
                  route $ setExtension ".html"
                  compile $ myPageCompiler
                          >>> applyTemplateCompiler "templates/index.html"
                          >>> applyTemplateCompiler "templates/base.html"
                          >>> processLocalUrlsCompiler

         -- Posts
         match "blog/*" $ do
                  route $ setExtension ".html"
                  compile $ pageCompiler
                   >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
                   >>> renderTagsField "prettytags" (fromCapture "tags/*")
                   >>> applyTemplateCompiler "templates/entry.html"
                   >>> applyTemplateCompiler "templates/base.html"
                   >>> processLocalUrlsCompiler

         -- Post list
         match "entries.html" $ route idRoute
         create "entries.html" $ constA mempty
                >>> arr (setField "title" "Posts")
                >>> requireAllA ("blog/*" `mappend` inGroup Nothing) addPostList
                >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                >>> applyTemplateCompiler "templates/entries.html"
                >>> applyTemplateCompiler "templates/base.html"
                >>> processLocalUrlsCompiler

         -- Tags
         create "tags" $
                requireAll ("blog/*" `mappend` inGroup Nothing)
                               (\_ ps -> readTags ps :: Tags String)

         -- Add a tag list compiler for every tag
         match "tags/*" $ route $ setExtension ".html"
         metaCompile $ require_ "tags"
                         >>> arr tagsMap
                         >>> arr (map (\(t, p) ->
                                           (tagIdentifier t, makeTagList t p)))

         -- RSS
         group "rss" $ do
                  match "blog/*" $ do
                         -- No route
                         compile $ pageCompiler
                            >>> (arr setPageUrl)

         match "rss.xml" $ route idRoute
         create "rss.xml" $ requireAll_ ("blog/*" `mappend`
                                                   inGroup (Just "rss"))
                    >>> arr (reverse . takeLast 10)
                    >>> arr (map $ copyBodyToField "description")
                    >>> renderRss feedConfiguration

    where

      renderTagCloud' :: Compiler (Tags String) String
      renderTagCloud' = renderTagCloud tagIdentifier 100 120

      tagIdentifier :: String -> Identifier (Page String)
      tagIdentifier = fromCapture "tags/*"

-- Make $url from $path
setPageUrl :: Page a -> Page a
setPageUrl p = let metaData = pageMetadata p
                   pth = dropExtension $ metaData ! "path"
                   newMetadata = M.insert "url" ("/" ++ pth) metaData
               in p { pageMetadata = newMetadata }

-- Take N last items from the list
takeLast :: Int -> [a] -> [a]
takeLast n xs = let l = length xs
                in drop (min l (l - n)) xs

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "entries" $
    arr (reverse . chronological)
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
        >>> processLocalUrlsCompiler

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

-- | path/to/smthng.html -> path/to/smthng
processLocalUrlsCompiler :: Compiler (Page String) (Page String)
processLocalUrlsCompiler = arr $ fmap (processHtml
                                       $ map (processAttrs processHref))

processHtml :: ([Tag String] -> [Tag String]) -- ^ HTML processing function
               -> String                      -- ^ HTML to process
               -> String                      -- ^ Resulting HTML
processHtml f = renderTags . f . parseTags

processAttrs :: (Attribute String -> Attribute String)
             -> Tag String -> Tag String
processAttrs f (TagOpen s a) = TagOpen s $ map f a
processAttrs _ x = x

processHref :: Attribute String -> Attribute String
processHref (key, value)
    | key == "href"
      && "http://" `isNotPrefixOf` value
      && htmlSuffix `isSuffixOf` value = (key, dropHtmlSuffix value)
    | otherwise = (key, value)
  where
    htmlSuffix = ".html"

    isNotPrefixOf l = not . isPrefixOf l
    dropHtmlSuffix s = take ((length s) - (length htmlSuffix)) s
