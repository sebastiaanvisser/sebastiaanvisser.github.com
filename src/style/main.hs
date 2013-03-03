{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay hiding (menu, contents)
import Data.Monoid
import Data.Text (Text)
import Prelude hiding (all)

import qualified Data.Text.Lazy.IO as Text
import qualified Clay.Media        as Media

main :: IO ()
main = Text.putStrLn
     . renderWith compact []
     $ do site
          column
          menu
          contents
          theArticle
          theFooter
          overview

-------------------------------------------------------------------------------

site :: Css
site =
  do html <> body ?
       do sym margin  nil
          sym padding nil

     body ?  background (png "bg", bgC)

column :: Css
column = body |> "div" ?
  do centered
     marginBottom (unit 5)
     -- backgroundImage (png "tile")

menu :: Css
menu = nav ?
  do uiFont
     alignCenter
     marginTop   u1
     paddingLeft u1
     lineHeight  u2
     a ? marginRight (px 20)

contents :: Css
contents = ".content" ?
  do backgroundColor (setA 200 white)
     padding         u1 u1 u3 u1

theFooter :: Css
theFooter = footer ?
  do uiFont
     alignCenter
     color     (setA 70 txtC)
     fontSize  (px 14)
     margin    u1 nil u4 nil

-------------------------------------------------------------------------------

overview :: Css
overview =
  ".read-more" ?
    do marginTop (unit (-1))
       fontSize  (pct 85)

theArticle :: Css
theArticle = article ?
  do contentFont
     marginBottom u1
     Main.meta

     star ?
       do sym padding nil
          sym margin  nil

     hr ?
       do border none nil white
          paddingBottom u1

     h1 <> h2 ?
       do sym margin   nil
          lineHeight   u2
          marginTop    u1
          color emC

     h1 ?
       do fontSize (px 24)
          marginBottom u1

     h2 ?
       do fontSize (px 18)

     p  ? marginBottom u1
     ul ? paddingLeft  u2

     a ?
       do animate
          textDecoration none
          color          linkC
          hover & backgroundColor bgC

     "em" ?
       do fontWeight bold
          fontStyle  normal
          color emC

     code ?
       do color           (txtC +. 60)
          backgroundColor bgC
          border          solid (px 1) (bgC -. 20)

     img ?
       do marginLeft  auto
          marginRight auto
          display     block

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     Clay.span ?
       do display  block
          fontSize (pct 85)
          color    (setA 160 txtC)

-------------------------------------------------------------------------------

centered :: Css
centered =
  do boxSizing   borderBox
     whenWide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     whenNarrow $
       do width       (pct 100)

contentFont :: Css
contentFont =
  do fontFamily ["Merriweather", "Georgia", "Times"] [serif]
     fontSize   (px 16)
     lineHeight u1
     color      txtC

uiFont :: Css
uiFont =
  do fontFamily    ["Open Sans", "Helvetixa", "Arial"] [sansSerif]
     fontSize      (px 20)
     lineHeight    u1
     textTransform uppercase
     a ?
       do color          linkC
          textDecoration none
          animate
          hover &
            do color      black
               background white

bgC, txtC, emC, linkC :: Color

bgC   = rgb 246 246 246
txtC  = rgb   0  20  40
emC   = rgb  40  20   0
linkC = rgb   0 100 180

-------------------------------------------------------------------------------

animate :: Css
animate =
  transitions
    [ ("background-color" , sec 0.5, ease, sec 0)
    , ("color"            , sec 0.2, ease, sec 0)
    ]

alignCenter :: Css
alignCenter = textAlign (alignSide sideCenter)

png :: Text -> BackgroundImage
png im = url ("../image/" <> im <> ".png")

unit :: Integer -> Size Abs
unit = px . (* 24)

pageWidth :: Size Abs
pageWidth = unit 25

nil :: Size Abs
nil = px 0

u1, u2, u3, u4 :: Size Abs
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

whenNarrow :: Css -> Css
whenNarrow  = query Media.all [Media.maxWidth pageWidth]

whenWide :: Css -> Css
whenWide = query Media.all [Media.minWidth pageWidth]

