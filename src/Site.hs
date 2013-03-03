{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

main :: IO ()
main = hakyll $
  do images
     styles
     pages
     postsMarkdown
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

postsMarkdown :: Rules ()
postsMarkdown = match "post/*/*/*/*.markdown" $
  do route (setExtension "html")
     compile $ pandocCompiler
           >>= loadAndApplyTemplate "template/site.html" defaultContext
           >>= relativizeUrls

