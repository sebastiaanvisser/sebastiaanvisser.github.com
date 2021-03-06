{-# LANGUAGE OverloadedStrings #-}
module Main where

import Clay hiding (menu, contents)
import Data.Monoid
import Data.Text (Text)
import Prelude hiding (all)

import qualified Clay.Media as Media

main :: IO ()
main = putCss $
  do site
     column
     menu
     contents
     theArticle
     theFooter
     overview

bgC, txtC, emC, linkC :: Color
bgC   = rgb 246 246 246
txtC  = rgb   0  20  40
emC   = rgb  40  20   0
linkC = rgb   0 100 180

-------------------------------------------------------------------------------

site :: Css
site =
  do html <> body ?
       do sym margin  nil
          sym padding nil

     body ? bg

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
     bgBorder        5
     padding         u1 u1 u2 u1

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
  do ".date" ?
       do smallFont
          float floatRight
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
       do bg
          height       (unit 1)
          border       none nil white
          marginBottom (unit 1)

     h1 <> h2 ?
       do sym margin   nil
          lineHeight   u2
          marginTop    u1
          color emC

     h1 ?
       do fontSize (px 24)
          marginBottom u1

     h2 ?
       do fontSize (px 17)

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

     strong ?
       do fontWeight normal
          fontStyle  italic
          fontSize   (px 15)
          color emC

     p ? code ?
       do codeBlocks
          sym2 padding 0 (px 4)

     p ? a ? code ?  color linkC

     pre ?
       do box
          codeBlocks
          margin       nil (unit (0)) u1 (unit (0))
          sym padding  (half 1)
          narrow $ fontSize  (px 14)
          wide   $ fontSize  (px 16)
          overflowX    auto

     img ?
       do marginLeft  auto
          marginRight auto
          display     block

meta :: Css
meta = ".meta" ?
  do textAlign (alignSide sideRight)
     float     floatRight
     marginTop (unit (-1))
     Clay.span ?
       do display block
          smallFont

-------------------------------------------------------------------------------

centered :: Css
centered =
  do box
     wide $
       do width       pageWidth
          marginLeft  auto
          marginRight auto
     narrow $
       do width       (pct 100)

contentFont :: Css
contentFont =
  do merriWeather
     fontSize   (px 16)
     lineHeight u1
     color      txtC

uiFont :: Css
uiFont =
  do openSans
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

smallFont :: Css
smallFont =
  do openSans
     fontSize (pct 85)
     color    (setA 120 txtC)

codeBlocks :: Css
codeBlocks = 
  do bgBorder 20
     fontFamily ["Courier"] [monospace]
     syntax

syntax :: Css
syntax =
  do color (rgb 0 60 100)
     ".kw" ? fontWeight bold
     ".kw" ? color (rgb   0   0   0)
     ".dt" ? color (rgb  20  60 180)
     ".dv" ? color (rgb 100   0 200)
     ".st" ? color (rgb 200  80 100)
     ".ot" ? color (rgb 160  80  80)
     ".fu" ? color (rgb   0 160 120)
     ".co" ? color (rgb 160 160 160)

-------------------------------------------------------------------------------

bg :: Css
bg = background (png "bg", bgC)

bgBorder :: Integer -> Css
bgBorder o = outline solid (px 1) (setA o black)

box :: Css
box = boxSizing borderBox

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

unit, half :: Integer -> Size Abs
unit = px . (* 24)
half = px . (* 12)

pageWidth :: Size Abs
pageWidth = unit 25

u1, u2, u3, u4 :: Size Abs
u1 = unit 1
u2 = unit 2
u3 = unit 3
u4 = unit 4

openSans :: Css
openSans = fontFamily ["Open Sans", "Helvetixa", "Arial"] [sansSerif]

merriWeather :: Css
merriWeather = fontFamily ["Merriweather", "Georgia", "Times"] [serif]

narrow :: Css -> Css
narrow = query all [Media.maxWidth pageWidth]

wide :: Css -> Css
wide = query all [Media.minWidth pageWidth]

