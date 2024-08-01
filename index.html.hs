#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [text (pkgs.haskell.lib.dontCheck clay)])"
{-# OPTIONS_GHC -Wall -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts, TypeFamilies #-}
import Data.Text.Lazy (unpack)

import Util.Markup
import Util.HTML.HTML5

main :: IO ()
main = putStrLn $ unpack $ render content

content :: Markup
content = ("text/html",) $ Html << Lang "en" ? do
    Head ? do
        metaCharset "UTF-8"
        Title ? "Lähteenmäki.net | jyri-matti lähteenmäki"
        metaDescription "Homepage of Jyri-Matti Lähteenmäki | programmer, designer, architect, haskeller. Father and husband."
        metaKeywords ["jyri-matti lähteenmäki", "programming", "developer", "software", "computer science", "haskell", "scala"]
        metaOGTitle "Lähteenmäki.net | jyri-matti lähteenmäki"
        metaOGType "website"
        metaOGUrl "http://www.lahteenmaki.net"
        metaOGSiteName "Lähteenmäki.net"
        Meta << Name "apple-mobile-web-app-title" << Content "lähteenmäki.net" ? empty
        Meta << Name "application-name" << Content "lähteenmäki.net" ? empty
        Meta << Name "apple-mobile-web-app-capable" << Content "yes" ? empty
        Meta << Name "mobile-web-app-capable" << Content "yes" ? empty
        Meta << Name "viewport" << Content "width=device-width, initial-scale=1.0" ? empty
        javascript "htmx.min.js"
        javascript "_hyperscript.js"
        javascript "fix-relative-hrefs.js"
        javascript "client-side-templates.js"
        javascript "swap-notitle.js"
        css "style.css"
        Script ? "window.onload = function() { htmx.trigger(window, 'customLoad'); }" -- TODO: remove when HTMX bug fixed
        Script ? analytics
    Body << HxExt "swap-notitle,fix-relative-hrefs" ? do
        Object << Id "template"  << Tabindex "-1" << Data_ "rss.xml" << TitleA "XSLT for Mastodon RSS" ? empty
        Object << Id "template2" << Tabindex "-1" << Data_ "goodreads.xml" << TitleA "XSLT for GoodReads RSS" ? empty
        Input << Id "lightmode" << Class "lightmode" << Type "checkbox" << AriaP "label" "Toggle between lightmode and darkmode" ? empty
        Input << Id "darkmode"  << Class "darkmode"  << Type "checkbox" << AriaP "label" "Toggle between lightmode and darkmode" ? empty
        Div << Class "container" ? do
            menu
            Label << Class "lightmode" << For "lightmode" << Tabindex "0" << Script_ "on keydown[code is 'Enter' or code is 'Space'] set checked of #lightmode to not(checked) of #lightmode" << TitleA "Switch between lightmode/darkmode" ? "🌓"
            Label << Class "darkmode"  << For "darkmode"  << Tabindex "0" << Script_ "on keydown[code is 'Enter' or code is 'Space'] set checked of #darkmode to not(checked) of #darkmode" << TitleA "Switch between lightmode/darkmode" ? "🌓"
            Header << Class "header" ? do
                H1 ? do
                    A << Href "https://lahteenmaki.net" ? "jyri-matti lähteenmäki"
            Main << Class "content" ? do
                section "presentations" presentations
                section "java-stuff" javastuff
                section "blog" blog
                section "electricity-spot-prices" spot
                section "toots" toots
                section "read-books" books
                section "railway-stuff" junailua
                section "dev" ohjelmointi
                section "famiglia" perhe
                section "contact" yhteys
                section "this-site" tamasivu
                Footer << Class "footer" ? "© Jyri-Matti Lähteenmäki 2023"

section secName body = do
    Div << Id secName <<  Class "section-wrapper" ? do
        A << Href ("#" <> secName) << Class "carousel" << Tabindex "-1" << Onclick "this.click()" << AriaP "label" ("Show section " <> secName) ? "" -- Chrome needs this onclick handler for whatever reason...
        Section << Class "section" << Class secName << Script_ "on intersection(intersecting) having threshold 0.75 if intersecting and (location.hash of window != '' or window.visualViewport.width <= 860) then trigger click on previous <a/>" ? body 

menu = do
    Nav << Class "menu-wrapper" ? do
        Div << Class "icon" << Tabindex "0" ? "☰"
        Ul << Class "menu" ? do
            Li ? do
                A << Id "menu-home" << Href "https://lahteenmaki.net" ? "home"
            menuItem "presentations" 
            menuItem "java-stuff" 
            menuItem "blog" 
            menuItem "electricity-spot-prices" 
            menuItem "toots" 
            menuItem "read-books" 
            menuItem "railway-stuff" 
            menuItem "dev" 
            menuItem "famiglia" 
            menuItem "contact"
            menuItem "this-site" 

menuItem secName = do
    Li ? do
        A << Id ("menu-" <> secName) << Href ("#" <> secName) ? text secName

box secName body = do
    H2 ? do
        A << Class "on" << Href ("#" ++ map (\c -> if c == ' ' then '-' else c) secName) ? text secName
        A << Class "off" << Href "#" ? text secName
    Div << Class "boxcontent" << Tabindex "0" ? body

block title href body = Div << Class "subsection" ? do
                           H3 ? do
                                A << Href href ? title
                           body

javastuff = box "java-stuff" $ do
    block "functional-utils" "https://github.com/solita/functional-utils" $
        Div ? "More-or-less functional util library for Java"
    block "meta-utils" "https://github.com/solita/meta-utils" $
        Div ? "Annotation processors to enable 1st-class-functions pre-java8"
    block "query-utils" "https://github.com/solita/query-utils" $
        Div ? "A layer on top of JPA2 to make querying a database even more complex ;)"
    block "api-utils" "https://github.com/solita/api-utils" $
        Div ? "Base utilities for making versatile APIs"

presentations = box "presentations" $ do
    block "Routinely coroutining" "https://lahteenmaki.net/dev_*21/" $
        Div ? "About coroutines and concurrent programming"
    block "The point of birds" "https://lahteenmaki.net/dev_*19/" $
        Div ? "About point-free style, tacit programming and combinator logic"
    block "Curse explicit recursion!" "https://lahteenmaki.net/dev_*18/" $
        Div ? "Demonstrating basics of Recursion Schemes"
    block "Optics in Programming" "https://lahteenmaki.net/dev_*16/" $
        Div ? "Studying lenses and prisms, profunctors and other category theory"
    block "Composing functions and beyond" "https://lahteenmaki.net/dev_*15/" $
        Div ? "Investigating composition for different kinds of functions"
    block "Functors, Applicatives, Monads ...WAT?" "https://lahteenmaki.net/dev_*14/" $
        Div ? "Explaining Monads and stuff"
    block "to Type a Class" "https://lahteenmaki.net/dev_*/" $
        Div ? "Thinking of type classes, Haskell, Scala and Java"

perhe = box "famiglia" $ do
    Div ? do
        H3 ? "joona"
    Div ? do
        H3 ? "juuso"
    Div ? do
        H3 ? "iina"
    Div ? do
        H3 ? do
            A << Href "http://www.facebook.com/laura.lahteenmaki.1" ? "laura"
    Div ? do
        H3 ? do
            A << Href "http://www.facebook.com/jyrimatti.lahteenmaki" ? "myself"

yhteys = box "contact" $ do
    Ol ? do
        Li ? "first-name@lahteenmaki.net"
        Li ? ahref "https://keybase.io/jyrimatti"
        Li ? do A << Href "https://pgp.mit.edu/pks/lookup?search=0x1959b8a4f22fec94&fingerprint=on" ? "F22FEC94"
        Li ? "Mastodon: @jyrimatti@lahteenmaki.net"
        Li ? ahref "https://twitter.com/jyrimatti"
        Li ? ahref "https://pinboard.in/u:jyrimatti/"
        Li ? ahref "https://github.com/jyrimatti"
        Li ? ahref "https://www.linkedin.com/in/jyrimatti"
        Li ? ahref "https://www.goodreads.com/jyrimatti"
        Li ? ahref "https://www.facebook.com/jyrimatti.lahteenmaki"

ohjelmointi = box "dev" $ do
    subs "flow" $
        Div ? do
            A << Href "https://mastodon.online/@jyrimatti" ? "Mastodon"
    subs "blog" $
        Div ? do
            ahref "https://blog.lahteenmaki.net"
    subs "links" $
        Div ? do
            A << Href "https://pinboard.in/u:jyrimatti/" ? "Pinboard"
    subs "projects" $
        Div ? do
            A << Href "http://github.com/jyrimatti" ? "GitHub"
  where subs title body = Div << Class "subsection" ? do
                               H3 ? title
                               body

toots = box "toots" $ do
  Div << Script_ "on htmx:afterSwap repeat in (<blockquote /> in me) set its innerHTML to its innerText"
      << HxGet "https://mastodon.online/@jyrimatti.rss"
      << HxExt "client-side-templates"
      << HxTrigger "customLoad from:window"
      << XsltTemplate "template" ?
          "loading..."

books = box "read books" $ do
  Div << Script_ "on htmx:afterSwap repeat in (<blockquote /> in me) set its innerHTML to its innerText"
      << HxGet "/goodreads/29730596?shelf=read"
      << HxExt "client-side-templates"
      << HxTrigger "customLoad from:window"
      << XsltTemplate "template2" ?
          "loading..."

blog = box "blog" $ do
    Div << HxGet "https://blog.lahteenmaki.net"
        << HxSelect ".posts"
        << HxTrigger "load from:window" ?
            "loading..."

junailua = box "railway stuff" $ do
    block "Rafiikka" "https://rafiikka.lahteenmaki.net" $
        Div ? "Experimental railway real-time graphics, work gaps, map and statistics"
    block "Infra-API" "https://rata.digitraffic.fi/infra-api" $
        Div ? "Finnish railway infrastructure, which I have something to do with"
    block "Jeti-API" "https://rata.digitraffic.fi/jeti-api" $
        Div ? "Finnish railway planned restrictions data, which I have something to do with"

spot = box "electricity spot prices" $ do
    block "spot.lahteenmaki.net" "https://spot.lahteenmaki.net" $ do
        P ? "Interactive graphs and API endpoints"
        IFrame << Src "https://spot.lahteenmaki.net" ? empty

tamasivu = box "this site" $ do
    Ul ? do
        Li ? "Haskell. Nix. Nixos."
        Li ? "Really-type-safe HTML. Haskell."
        Li ? do
            "Styles "
            A << Href "http://fvisser.nl/clay/" ? "Clay"
            ". Haskell."
        Li ? do
            A << Href "https://github.com/jyrimatti/lahteenmaki.net" ? "https://github.com/jyrimatti/lahteenmaki.net"

analytics = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\
\  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\
\  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\
\  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');\
\\
\  ga('create', 'UA-20222288-1', 'auto');\
\  ga('send', 'pageview');"
