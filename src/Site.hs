{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import Hakyll

main :: IO ()
main = hakyll $
  do images
     styles
     pages
     posts
     templates

-------------------------------------------------------------------------------

templates :: Rules ()
templates = match "template/*" (compile templateCompiler)

images :: Rules ()
images = match "image/*" $
  do route   idRoute
     compile copyFileCompiler

styles :: Rules ()
styles = match "style/*.hs" $
  do route (setExtension "css")
     compile (getResourceString >>= withItemBody (unixFilter "runghc" []))

pages :: Rules ()
pages = match "page/*.html" $
  do route (gsubRoute "page/" (const ""))
     compile $ getResourceBody
           >>= loadAndApplyTemplate "template/site.html" defaultContext
           >>= relativizeUrls

posts :: Rules ()
posts = match "post/*/*/*/*.html" $
  do route idRoute
     compile $ getResourceBody
           >>= loadAndApplyTemplate "template/site.html" defaultContext
           >>= relativizeUrls

-------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    ps      <- sortFilter <$> loadAll "post/*"
    itemTpl <- loadBody "template/post-item.html"
    list    <- applyTemplateList itemTpl postCtx ps
    return list

