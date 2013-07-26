{-# LANGUAGE OverloadedStrings #-}

import Data.List(isPrefixOf)
import Data.Monoid(mappend, mconcat)

import Hakyll

import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..))

main :: IO ()
main = hakyll $ do
  -- CSS
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Images / Files / Favicon
  match ("img/*" .||. "files/*" .||. "favicon.ico") $ do
    route idRoute
    compile copyFileCompiler

  -- Index / 404
  match (fromList ["index.md", "404.md"]) $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/index.html" defaultContext
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= myRelativizeUrls

  -- Tags
  tags <- buildTags "blog/*" (fromCapture "tags/*.html")

  -- Posts
  match "blog/*" $ do
    route $ setExtension ".html"
    compile $ myPageCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/entry.html" (postCtx tags)
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= myRelativizeUrls

  -- Post list
  create ["entries.html"] $ do
    route idRoute
    compile $ do
      list <- postList tags "blog/*" recentFirst
      tagCloud <- renderTagCloud 100 120 tags
      makeItem ""
        >>= loadAndApplyTemplate "templates/entries.html"
            (constField "title"    "Posts"  `mappend`
             constField "tagcloud" tagCloud `mappend`
             constField "entries"  list     `mappend` defaultContext)
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
        >>= myRelativizeUrls

  -- Tags
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged '" ++ tag ++ "'"
    route idRoute
    compile $ do
      list <- postList tags pattern recentFirst
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html"
            (constField "title"   title `mappend`
             constField "entries" list  `mappend` defaultContext)
        >>= loadAndApplyTemplate "templates/base.html" defaultContext
        >>= myRelativizeUrls

  -- Templates
  match "templates/*" $ compile templateCompiler

  -- RSS
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots "blog/*" "content"
        >>= fmap (take 10) . recentFirst
        >>= renderAtom feedConfiguration feedCtx

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/entryitem.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

-- | Make Pandoc and MathJax play together.
myPageCompiler :: Compiler (Item String)
myPageCompiler = pandocCompilerWith defaultHakyllReaderOptions myWriterOptions

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions
                  { writerHTMLMathMethod  = MathJax "" }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Churning and Churning"
    , feedDescription = "Much wailing and gnashing of teeth"
    , feedAuthorName  = "Mikhail Glushenkov"
    , feedAuthorEmail = "mikhail.glushenkov@gmail.com"
    , feedRoot        = "http://coldwa.st/e"
    }

-- HACK. Relativize all urls except those that start with '/e/'. Code copied
-- from Hakyll.Web.Html.RelativizeUrls.

myRelativizeUrls :: Item String -> Compiler (Item String)
myRelativizeUrls item = do
  route' <- getRoute $ itemIdentifier item
  return $ case route' of
    Nothing -> item
    Just r  -> fmap (myRelativizeUrlsWith $ toSiteRoot r) item

myRelativizeUrlsWith :: String  -- ^ Path to the site root
                        -> String  -- ^ HTML to relativize
                        -> String  -- ^ Resulting HTML
myRelativizeUrlsWith root = withUrls rel
  where
    isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
              -- The only thing changed
              && not ("/e/" `isPrefixOf` x)
    rel x   = if isRel x then root ++ x else x
