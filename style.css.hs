#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [text (pkgs.haskell.lib.dontCheck clay)])"
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy hiding (center,foldl1)

import Prelude hiding (div)
import Data.Monoid
import Clay hiding (output)
import Clay.Stylesheet (Feature(..), MediaType(..))

import Control.Monad (forM_)
import Data.String (fromString)

main :: IO ()
main = putStrLn $ unpack $ renderWith compact [] css

sections :: [String]
sections = ["presentations", "java-stuff", "blog", "toots", "tweets", "read-books", "railway-stuff", "dev", "famiglia", "contact", "this-site"]

lightGray, mediumGray, darkGray, lightBlue, almostWhite, almostBlack :: Color

lightGray   = rgb 200 200 200
mediumGray  = rgb 142 142 142
darkGray    = rgb 30 30 30
lightBlue   = rgb 97 205 245
almostWhite = rgb 238 238 238
almostBlack = rgb 18 18 18

roundCorners :: Css
roundCorners = borderRadius (em 0.4) (em 0.4) (em 0.4) (em 0.4)

rotatedSection :: Int -> Css
rotatedSection n = ".section-wrapper" # nthChild (fromString $ show n) ? do
    transforms [translate3d nil nil 1, rotateZ (deg $ 2 + fromIntegral (negate n))]
    where negate n = case n of
                 _ | even n -> n
                 _          -> n * (-1)

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

highlight :: Css
highlight = do
        ":is(.container)" ? do
            height (vh 80) -- fallback
            height (other "99dvh")
            alignItems center
        ".menu-wrapper" ? do
            display block
        "main" ? do
            display flex
            overflowX scroll
            width (vw 100)
            "scroll-snap-type" -: "x mandatory"
        ".section-wrapper" ? do
            display flex
            flexDirection column
            overflow visible
            transform none
            "scroll-snap-stop" -: "always"
            "scroll-snap-align" -: "start"
            ".section" ? do
                marginBottom (em 5)
                marginTop (em 1)
                marginLeft  (other "calc(calc(100vw - 21em) / 2)")
                marginRight (other "calc(calc(100vw - 21em) / 2)")
                width (em 21)
        query (MediaType "all") [ Feature "min-width" $ Just "860px" ] $ do
            ".off" ? do
                display inline
            ".on" ? do
                display none
        ".boxcontent" ? do
            maxHeight (other "calc(100vh - 17em)")
        ".carousel" ? do
            position absolute
            bottom (em 3)
            marginLeft (pct 50)
            display inline
            backgroundColor almostWhite
            transition "background-color" (ms 250) ease 0
            width (em 0.3)
            height (em 0.3)
            borderStyle solid
            borderWidth (em 0.2)
            borderColor white
            borderRadius (em 1) (em 1) (em 1) (em 1)
            padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
        ":target .carousel" ? do
            backgroundColor black
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

css :: Css
css = do
    body ? do
        position relative
        margin nil nil nil nil
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
        bottom nil
        width nil
        height nil
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
        position absolute
        left (px (-99999))
        opacity 0
        listStyleType none
        margin nil nil nil nil
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
            padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
            lineHeight (em 1.5)
            fontVariant smallCaps
            transitions [("opacity",(ms 250), ease, (ms 250)),
                         ("left",   (ms 250), ease, (ms 0))]
            "a" ? do
                color almostBlack
                borderLeftWidth (px 1)
                borderLeftStyle solid
                borderLeftColor transparent
                paddingLeft (em 0.25)
            "a:hover" ? do
                borderLeftColor lightBlue
                fontStyle italic
        ".icon" ? do
            fontSize (em 2)
    (foldl1 (<>) $ fmap (\s -> element $ fromString $ "body:has(#" <> s <> ":target) #menu-" <> s) sections) ? do
        borderLeftColor lightBlue
        fontStyle italic
    query (MediaType "all") [ Feature "hover" $ Just "hover", Feature "pointer" $ Just "fine" ] $ do
        ".menu-wrapper" ? do
            ".icon:hover ~ .menu" <> ".menu:hover" ? do
                left nil
                opacity 1
    ".menu-wrapper" ? do
        ".icon:focus ~ .menu" <> ".menu:active" <> ".menu:focus-within" ? do
            left nil
            opacity 1
    
    ".header" ? do
        textShadow 0 0 (px 50) lightBlue
        color lightGray
        margin 0 (em 1.5) 0 (em 3.5)
        h1 ? do
            fontSize (other "min(2em,6vw)")
            marginTop nil
    ".footer" ? do
        color mediumGray
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
    ".boxcontent" ? do
        minHeight (px 200)
        maxHeight (px 350)
        overflow scroll
        overflowWrap breakWord
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
            padding (em 0.6) (em 0.6) (em 0.6) (em 0.6)
            margin 0 (em 1.2) 0 (em 1.2)
            borderBottom (px 1) solid lightGray
            fontVariant smallCaps
            textAlign (alignSide sideRight)
        div <? do
            padding (em 1) (em 1) (em 1) (em 1)
    forM_ [0..9] rotatedSection

    ".carousel" ? do
        display none
    "body:has(:target)" ? do
        highlight
    query (MediaType "all") [ Feature "max-width" $ Just "860px" ] $ do
        "#menu-home" ? do
            display none
        ".container" ? do
            highlight
    
    ".books" ? do
        div ? do
            div ? do
                h2 ? do
                    display none
    
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
