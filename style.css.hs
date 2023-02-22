#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [text (pkgs.haskell.lib.dontCheck clay)])"
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy hiding (center)

import Prelude hiding (div)
import Data.Monoid
import Clay hiding (output)
import Clay.Stylesheet (Feature(..), MediaType(..))

import Control.Monad (forM_)
import Data.String (fromString)

main = putStrLn $ unpack $ renderWith compact [] css

lightGray = rgb 200 200 200
lightBlue = "#61CDF5"

roundCorners = borderRadius (em 0.4) (em 0.4) (em 0.4) (em 0.4)

css :: Css
css = do
    html ? do
        backgroundColor white
    body ? do
        color black
        backgroundColor white
        position relative
        margin (em $ -1) 0 0 0
    ".container" ? do
        textAlign (alignSide sideCenter)
        display flex
        flexDirection column
    ".content" ? do
        maxWidth (pct 100)
    ".carousel" ? do
        display none
    ".header" <> ".section" ? do
        margin (em 1) (em 1) (em 1) (em 1)
        roundCorners
        fontFamily ["cursive"] []
    ".header" ? do
        textShadow 0 0 (px 50) lightBlue
        color lightGray
        padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
        marginBottom (px 0)
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
    ".section-wrapper" ? do
        display inlineBlock
        overflow hidden
    ".section" ? do
        background (linearGradient (angular (deg 30)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        maxWidth (em 25)
        overflow hidden
        display inlineFlex
        flexDirection column
    "div.sourceCode" ? do
        background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        roundCorners
        display inlineBlock
    ".section" <> "div.sourceCode" ? do
        boxShadow' (px (-3)) (px 1) (px 15) lightBlue
        textAlign (alignSide sideLeft)
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
        ".container" ? do
            height (vh 100)
            alignItems center
        ".content" ? do
            display flex
            overflow hidden
        ".section-wrapper" ? do
            display flex
            flexDirection column
        ".section" ? do
            display none
            marginBottom (em 4.5)
        ".carousel" ? do
            width (em 1)
            position absolute
            bottom (em 3)
            marginLeft (pct 50)
            display inline
        ".carousel:checked ~ .section" ? do
            display inlineFlex
        ".section-wrapper:nth-child(1) .carousel" ? do
            left (em (-9.0))
        ".section-wrapper:nth-child(2) .carousel" ? do
            left (em (-7.5))
        ".section-wrapper:nth-child(3) .carousel" ? do
            left (em (-6))
        ".section-wrapper:nth-child(4) .carousel" ? do
            left (em (-4.5))
        ".section-wrapper:nth-child(5) .carousel" ? do
            left (em (-3.0))
        ".section-wrapper:nth-child(6) .carousel" ? do
            left (em (-1.5))
        ".section-wrapper:nth-child(7) .carousel" ? do
            left (em 0)
        ".section-wrapper:nth-child(8) .carousel" ? do
            left (em 1.5)
        ".section-wrapper:nth-child(9) .carousel" ? do
            left (em 3.0)
        ".section-wrapper:nth-child(10) .carousel" ? do
            left (em 4.5)
        ".section-wrapper:nth-child(11) .carousel" ? do
            left (em 6.0)
        ".boxcontent" ? do
            maxHeight inherit
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
        clear both
    h3 ? do
        fontStyle italic
        marginBottom (em 0.5)
    object ? do
        position absolute
        bottom (px 0)
        width (px 0)
        height (px 0)
    iframe ? do
        width (pct 100)
        height (em 15)
    ".animate" ? do
        animationDuration (sec 0.5)
        animationFillMode forwards
    ".animateLeft" ? do
        animationName "slideLeft"
        transitionTimingFunction easeOut
    ".animateRight" ? do
        animationName "slideRight"
        transitionTimingFunction easeOut
    ".animateFromLeft" ? do
        animationName "slideFromLeft"
        transitionTimingFunction easeIn
        ".section" ? do
            display inlineFlex
    ".animateFromRight" ? do
        animationName "slideFromRight"
        transitionTimingFunction easeIn
        ".section" ? do
            display inlineFlex
    keyframes "slideLeft" [(0,marginLeft (pct 0)), (100,marginLeft (pct (-150)))]
    keyframes "slideRight" [(0,marginLeft (pct 0) <> marginRight (pct 0)), (100,marginLeft (pct 150) <> marginRight (pct (-150)))]
    keyframes "slideFromLeft" [(0,marginLeft (pct (-150))), (100,marginLeft (pct 0))]
    keyframes "slideFromRight" [(0,marginLeft (pct 150) <> marginRight (pct (-150))), (100,marginLeft (pct 0) <> marginRight (pct 0))]
    ".lightmode" <> ".darkmode" ? do
        position sticky
        float floatRight
        top (em 1.5)
        right (em 1.5)
        zIndex 1
    "input.lightmode" <> "input.darkMode" ? do
        display none
    "label.lightmode:before" <> "label.darkmode:before" ? do
        content $ stringContent "🌓"
    ".lightmode:checked~.container" ? do
        color black
        backgroundColor white
        ".section" ? do
            background (linearGradient (angular (deg 30)) [(lightBlue,pct (-50)), (white,50), (white,100)])
        "div.sourceCode" ? do
            background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (white,50), (white,100)])
    ".darkmode:checked~.container" ? do
        color "#eeeeee"
        backgroundColor "#121212"
        ".section" ? do
            background (linearGradient (angular (deg 30)) [(lightBlue,pct (-80)), (grayish 30,80), (grayish 142,100)])
        "div.sourceCode" ? do
            color black
            background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (grayish 142,50), (grayish 142,100)])
    query (MediaType "all") [ Feature "prefers-color-scheme" $ Just "light" ] $ do
        ".lightmode" ? do
            display none
    query (MediaType "all") [ Feature "prefers-color-scheme" $ Just "dark" ] $ do
        ".darkmode" ? do
            display none
        html ? do
            backgroundColor "#121212"
        body ? do
            color "#eeeeee"
            backgroundColor "#121212"
        ".section" ? do
            background (linearGradient (angular (deg 30)) [(lightBlue,pct (-80)), (grayish 30,80), (grayish 142,100)])
        "div.sourceCode" ? do
            color black
            background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (grayish 142,50), (grayish 142,100)])

rotatedSection :: Int -> Css
rotatedSection n = "html:not(.highlight) .section-wrapper" # nthChild (fromString $ show n) ? do
    transforms [translate3d nil nil 1, rotateZ (deg $ 2 + fromIntegral (negate n))]
    where negate n = case n of
                 _ | mod n 2 == 0 -> n
                 otherwise -> n * (-1)
