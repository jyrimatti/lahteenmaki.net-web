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

main :: IO ()
main = putStrLn $ unpack $ renderWith compact [] css

lightGray, mediumGray, darkGray, lightBlue, almostWhite, almostBlack :: Color

lightGray = rgb 200 200 200
mediumGray = rgb 142 142 142
darkGray = rgb 30 30 30
lightBlue = rgb 97 205 245
almostWhite = rgb 238 238 238
almostBlack = rgb 18 18 18

roundCorners :: Css
roundCorners = borderRadius (em 0.4) (em 0.4) (em 0.4) (em 0.4)

rotatedSection :: Int -> Css
rotatedSection n = "html:not(.highlight) .section-wrapper" # nthChild (fromString $ show n) ? do
    transforms [translate3d nil nil 1, rotateZ (deg $ 2 + fromIntegral (negate n))]
    where negate n = case n of
                 _ | mod n 2 == 0 -> n
                 otherwise -> n * (-1)

light :: Css
light = do
    color black
    backgroundColor white
    ".section" ? do
        background (linearGradient (angular (deg 30)) [(lightBlue,pct (-50)), (white,50), (white,100)])
    "div.sourceCode" ? do
        background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (white,50), (white,100)])

dark :: Css
dark = do
    color almostWhite
    backgroundColor almostBlack
    ".section" ? do
        background (linearGradient (angular (deg 30)) [(lightBlue,pct (-80)), (darkGray,80), (mediumGray,100)])
    "div.sourceCode" ? do
        background (linearGradient (angular (deg 210)) [(lightBlue,pct (-50)), (mediumGray,50), (mediumGray,100)])

css :: Css
css = do
    body ? do
        position relative
        margin (px 0) 0 0 0
    a ? do
        color lightBlue
        textDecoration none
        transition "color" (ms 100) ease 0
    "a:hover" ? do
        color blue
    ol ? do
        padding nil nil nil nil
        listStyleType none
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
    
    ".container" ? do
        textAlign (alignSide sideCenter)
        display flex
        flexDirection column
        width (pct 100)
    
    ".menu" ? do
        display flex
        visibility hidden
    ".menu-wrapper" ? do
        display none
        position absolute
        left (em 1.5)
        top (em 1)
        width (em 8)
        textAlign (alignSide sideLeft)
        ".menu" ? do
            flexDirection column
            backgroundColor almostWhite
            color black
            padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
            lineHeight (em 1.5)
            fontVariant smallCaps
            "label" ? do
                borderLeftWidth (px 1)
                borderLeftStyle solid
                borderLeftColor transparent
                paddingLeft (em 0.25)
            "label:hover" <> "label:target" ? do
                borderLeftColor lightBlue
                fontStyle italic
        ".icon" ? do
            fontSize (em 2)
    query (MediaType "all") [ Feature "hover" $ Just "hover", Feature "pointer" $ Just "fine" ] $ do
        ".menu-wrapper:hover" ? do
            ".menu" ? do
                --display flex
                visibility visible
    ".menu-wrapper:focus" ? do
        ".menu" ? do
            --display flex
            visibility visible
    
    ".header" ? do
        textShadow 0 0 (px 50) lightBlue
        color lightGray
        margin 0 (em 1.5) 0 (em 3.5)
        h1 ? do
            fontSize (other "min(2em,6vw)")
            marginTop (em 0)
    ".footer" ? do
        color $ mediumGray
        fontStyle italic
        position absolute
        bottom nil
        right (em 1)
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        fontSize (em 0.75)
        background (linearGradient (angular (deg 120)) [(white,0), (lightGray,40), (white,100)])
        opacity 0.5
        roundCorners
    
    ".section-wrapper" ? do
        display inlineBlock
        overflow hidden
    ".section" ? do
        margin (em 1) (em 1) (em 1) (em 1)
        maxWidth (em 25)
        overflow hidden
        display inlineFlex
        flexDirection column
        roundCorners
        fontFamily ["cursive"] []
    ".off" ? do
        display none
    ":target ~ * .off" <> ":target .off" ? do
        display inline
    ":target ~ * .on" <> ":target .on" ? do
        display none
    ".boxcontent" ? do
        maxHeight (px 350)
        overflow scroll
    ".subsection" ? do
        a ? do
            paddingLeft (em 0.5)

    "div.sourceCode" ? do
        color black
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

    ".carousel" ? do
        display none
    ".highlight" ? do
        ".container" ? do
            height (vh 80) -- fallback
            height (other "99dvh")
            alignItems center
        ".menu-wrapper" ? do
            display block
        ".content" ? do
            display flex
            overflowX scroll
            width (vw 100)
            "scroll-snap-type" -: "x mandatory"
        ".section-wrapper" ? do
            display flex
            flexDirection column
            overflow visible
            "scroll-snap-stop" -: "always"
            "scroll-snap-align" -: "start"
        ".section" ? do
            marginBottom (em 5)
            marginTop (em 1)
            marginLeft (other "calc(calc(100vw - 25em) / 2)")
            marginRight (other "calc(calc(100vw - 25em) / 2)")
            width (em 25)
        ".boxcontent" ? do
            maxHeight inherit
        
        ".carousel" ? do
            position absolute
            bottom (em 3)
            marginLeft (pct 50)
            display inline
        ".section-wrapper:nth-child(1) .carousel" ? do
            left (em (-11))
        ".section-wrapper:nth-child(2) .carousel" ? do
            left (em (-9))
        ".section-wrapper:nth-child(3) .carousel" ? do
            left (em (-7))
        ".section-wrapper:nth-child(4) .carousel" ? do
            left (em (-5))
        ".section-wrapper:nth-child(5) .carousel" ? do
            left (em (-3))
        ".section-wrapper:nth-child(6) .carousel" ? do
            left (em (-1))
        ".section-wrapper:nth-child(7) .carousel" ? do
            left (em 1)
        ".section-wrapper:nth-child(8) .carousel" ? do
            left (em 3)
        ".section-wrapper:nth-child(9) .carousel" ? do
            left (em 5)
        ".section-wrapper:nth-child(10) .carousel" ? do
            left (em 7)
        ".section-wrapper:nth-child(11) .carousel" ? do
            left (em 9)
    
    ".books" ? do
        div ? do
            div ? do
                h2 ? do
                    display none
    
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
    keyframes "slideLeft"      [(0,marginLeft (pct 0))                              , (100,marginLeft (pct (-150)))]
    keyframes "slideRight"     [(0,marginLeft (pct 0) <> marginRight (pct 0))       , (100,marginLeft (pct 150) <> marginRight (pct (-150)))]
    keyframes "slideFromLeft"  [(0,marginLeft (pct (-150)))                         , (100,marginLeft (pct 0))]
    keyframes "slideFromRight" [(0,marginLeft (pct 150) <> marginRight (pct (-150))), (100,marginLeft (pct 0) <> marginRight (pct 0))]

    ".lightmode" <> ".darkmode" ? do
        position (other "-webkit-sticky")
        position sticky
        alignSelf flexEnd
        top (em 1.5)
        right (em 1.5)
        zIndex 1
    "input.lightmode" <> "input.darkmode" ? do
        display none

    query (MediaType "all") [ Feature "prefers-color-scheme" $ Just "light" ] $ do
        html ? do
            backgroundColor white
        ".lightmode" ? do
            display none
        ".container" ? do
            light
    query (MediaType "all") [ Feature "prefers-color-scheme" $ Just "dark" ] $ do
        html ? do
            backgroundColor black
        ".darkmode" ? do
            display none
        ".container" ? do
            dark

    ".lightmode:checked ~ .container" ? do
        light
    ".darkmode:checked ~ .container" ? do
        dark
