#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [text clay])"
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy

import Prelude hiding (div)
import Data.Monoid
import Clay hiding (output)

import Control.Monad (forM_)
import Data.String (fromString)

main = putStrLn $ unpack $ renderWith compact [] css

lightGray = rgb 200 200 200
lightBlue = "#61CDF5"

roundCorners = borderRadius (em 0.4) (em 0.4) (em 0.4) (em 0.4)

css :: Css
css = do
    body ? do
        position relative
    ".container" ? do
        marginLeft auto
        marginRight auto
        textAlign (alignSide sideCenter)
    ".header" ? do
        textShadow 0 0 (px 50) lightBlue
        color lightGray
        padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
    ".header" <> ".section" ? do
        margin (em 1) (em 1) (em 1) (em 1)
        roundCorners
        fontFamily ["cursive"] []
    ".footer" ? do
        color $ grayish 142
        fontStyle italic
        position absolute
        bottom nil
        right nil
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        fontSize (em 0.75)
        background (linearGradient (angular (deg 120)) [(grayish 255,0), (grayish 242,40), (grayish 255,100)])
        opacity 0.5
        transform $ translate3d nil nil  1
        roundCorners
    ".books" ? div ? div ? h2 ? do
        display none
    ".boxcontent" ? do
        maxHeight (px 350)
        overflow scroll
    ".section" ? do
        background (linearGradient (angular (deg 30)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        maxWidth (em 25)        
    "div.sourceCode" ? do
        background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        roundCorners
    ".section" <> "div.sourceCode" ? do
        boxShadow (px (-3)) (px 1) (px 15) lightBlue
        display inlineBlock
        textAlign (alignSide sideLeft)
        width (pct 85)
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
    ".highlight" ? do
        backgroundColor "#222"
        ".section" ? do
            opacity 0.15
        ".section.lifted" ? do
            opacity 1.0
            position fixed
            zIndex 999
            marginRight auto
            top (px 90)
            left (pct 50)
            marginLeft (px (-210))
    ".subsection" ? a ? do
        paddingLeft (em 0.5)
    a ? do
        color lightBlue
        textDecoration none
        transition "color" (ms 100) ease 0
    a # hover ? do
        color blue
    ol ? do
        padding nil nil nil nil
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
rotatedSection n = ".section" # nthChild (fromString $ show n) ? do
    transforms [translate3d nil nil 1, rotateZ (deg $ 2 + fromIntegral (negate n))]
    where negate n = case n of
                 _ | mod n 2 == 0 -> n
                 otherwise -> n * (-1)
