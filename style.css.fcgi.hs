#!/usr/bin/ghc -outputdir /tmp/
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.FastCGI
import System.Environment.Executable 
import System.Directory
import Data.Text.Lazy

import Prelude hiding (div)
import Data.Monoid
import Clay hiding (output)

import qualified Classes as C
import Classes (sel)
import Control.Monad (forM_)
import Data.String (fromString)

modTime = getScriptPath >>= \script -> getModificationTime $ case script of
    Executable fp -> fp
    RunGHC fp -> fp

main = modTime >>= mainn
mainn started = modTime >>= \modified -> if modified /= started then (return ()) else runOneFastCGIorCGI app >>= \isFast -> if isFast then mainn started else return ()

app = do
  modified <- liftIO modTime
  setHeader "Content-type" "text/css"
  setHeader "Last-Modified" (show modified)
  setHeader "Cache-Control" $ "max-age=60, must-revalidate"
  output $ unpack $ renderWith compact css

lightGray = rgb 200 200 200
lightBlue = "#61CDF5"

roundCorders = borderRadius (em 0.4) (em 0.4) (em 0.4) (em 0.4)

css :: Css
css = do
    body ? do
        position relative
    sel C.container ? do
        marginLeft auto
        marginRight auto
        textAlign (alignSide sideCenter)
    sel C.header ? do
        textShadow 0 0 (px 50) lightBlue
        color lightGray
        padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
        "white-space" -: "nowrap"
    sel C.header <> sel C.section ? do
        margin (em 1) (em 1) (em 1) (em 1)
        roundCorders
        fontFamily ["cursive"] []
    sel C.footer ? do
        color $ grayish 142
        fontStyle italic
        position absolute
        bottom 0
        right 0
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        fontSize (em 0.75)
        background (linearGradient (angular (deg 120)) [(grayish 255,0), (grayish 242,40), (grayish 255,100)])
        opacity 0.5
        transform $ translate3d 0 0 1
        roundCorders
    sel C.books ? div ? div ? h2 ? do
        display none
    sel C.boxcontent ? do
        maxHeight (px 350)
        overflow scroll
    sel C.section ? do
        boxShadow (px (-3)) (px 1) (px 15) lightBlue
        background (linearGradient (angular (deg 30)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        display inlineBlock
        textAlign (alignSide sideLeft)
        width (px 420)
        h2 ? do
            color lightGray
            let pad = (em 0.6)
            padding pad pad pad pad
            margin 0 (em 1.2) 0 (em 1.2)
            borderBottom solid (px 1) lightGray
            fontVariant smallCaps
            textAlign (alignSide sideRight)
        div <? do
            padding (em 1) (em 1) (em 1) (em 1)
    forM_ [0..9] rotatedSection
    sel C.subsection ? a ? do
        paddingLeft (em 0.5)
    a ? do
        color lightBlue
        textDecoration none
        transition "color" (ms 100) ease 0
    a # hover ? do
        color blue
    ol ? do
        padding 0 0 0 0
        "list-style" -: "none"
    ul ? do
        paddingLeft (em 1)
    li ? do
        padding (em 0.2) 0 (em 0.2) 0
    h3 ? do
        fontStyle italic
        marginBottom (em 0.5)
    iframe ? do
        width (pct 100)
        height (em 15)

rotatedSection :: Int -> Css
rotatedSection n = sel C.section # nthChild (fromString $ show n) ? do
    transforms [translate3d 0 0 1, rotateZ (deg $ 2 + fromIntegral (negate n))]
    where negate n = case n of
                 _ | mod n 2 == 0 -> n
                 otherwise -> n * (-1)
