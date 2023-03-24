#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages(p: with p; [text (pkgs.haskell.lib.dontCheck clay)])"
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts, TypeFamilies #-}
import Data.Text.Lazy (unpack)

import Util.Markup
import Util.HTML.HTML5
import Util.HTML.HTML5.Attributes (Class(..), Type(..))

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
        javascript "highlight.js"
        css "style.css"
        Script ? analytics
    Body << HxExt "swap-notitle,fix-relative-hrefs" ? do
        Object << Onload "this.loaded=true" << Id "template" << Data_ "rss.xml" ? empty
        Object << Onload "this.loaded=true" << Id "template2" << Data_ "goodreads.xml" ? empty
        Input << Id "lightmode" << Class "lightmode" << Type "checkbox" ? empty
        Input << Id "darkmode" << Class "darkmode" << Type "checkbox" ? empty
        Div << Class "container" ? do
            Label << Class "lightmode" << For "lightmode" << TitleA "Switch between lightmode/darkmode" ? "🌓"
            Label << Class "darkmode" << For "darkmode" << TitleA "Switch between lightmode/darkmode" ? "🌓"
            menu
            Div << Class "header" ? do
                H1 ? "jyri-matti lähteenmäki"
            Div << Class "content" ? do
                section "presentations" presentations True
                section "java-stuff" javastuff False
                section "blog" blog False
                section "toots" toots False
                section "tweets" tweets False
                section "read-books" books False
                section "railway-stuff" junailua False
                section "dev" ohjelmointi False
                section "famiglia" perhe False
                section "contact" yhteys False
                section "this-site" tamasivu False
                Div << Class "footer" ? "© Jyri-Matti Lähteenmäki 2023"

section name body checked = do
    Div << Id name <<  Class "section-wrapper" ? do
        Input << Id ("carousel-" <> name) << Type "radio" << Class "carousel" << Name "carousel" << Checked checked ? empty
        Div << Class "section" << Class name ? body 

menu = do
    Div << Class "menu-wrapper" << Tabindex "1" ? do
        Div << Class "icon" ? "☰"
        Nav << Class "menu" ? do
            menuItem "presentations" 
            menuItem "java-stuff" 
            menuItem "blog" 
            menuItem "toots" 
            menuItem "tweets" 
            menuItem "read-books" 
            menuItem "railway-stuff" 
            menuItem "dev" 
            menuItem "famiglia" 
            menuItem "contact"
            menuItem "this-site" 

menuItem name = do
    Label << Id ("item-" <> name) << For ("carousel-" <> name) ? text name

box name body = do
    H2 ? do
        A << Class "on" << Href ("#" ++ map (\c -> if c == ' ' then '-' else c) name) ? text name
        A << Class "off" << Href "#" ? text name
    Div << Class "boxcontent" ? body

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
    block "flow" $
        Div ? do
            A << Href "https://twitter.com/jyrimatti" ? "Twitter"
    block "blog" $
        Div ? do
            ahref "https://blog.lahteenmaki.net"
    block "links" $
        Div ? do
            A << Href "https://pinboard.in/u:jyrimatti/" ? "Pinboard"
    block "projects" $
        Div ? do
            A << Href "http://github.com/jyrimatti" ? "GitHub"
  where block title body = Div << Class "subsection" ? do
                               H3 ? title
                               body

toots = box "toots" $ do
  Div << Script_ "on htmx:afterSwap repeat in (<blockquote /> in me) set its innerHTML to its innerText" ? do
    Div << HxGet "https://mastodon.online/@jyrimatti.rss"
        << HxExt "client-side-templates"
        << HxTrigger "every 1s [document.getElementById('template').loaded]" -- try repeatedly, since template may not load immediately
        << HxSwap "outerHTML" -- stop polling
        << XsltTemplate "template" ?
            "loading..."

tweets = box "tweets" $ do
    A << Class "twitter-timeline" << Href "https://twitter.com/jyrimatti" << Data "widget-id" "331834452940570626" << Data "chrome" "noheader nofooter transparent noborders" ? "Tweets by @jyrimatti"
    Script ? raw "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+\"://platform.twitter.com/widgets.js\";fjs.parentNode.insertBefore(js,fjs);}}(document,\"script\",\"twitter-wjs\");"

books = box "read books" $ do
  Div << Script_ "on htmx:afterSwap repeat in (<blockquote /> in me) set its innerHTML to its innerText" ? do
    Div << HxGet "/goodreads/29730596?shelf=read"
        << HxExt "client-side-templates"
        << HxTrigger "every 1s [document.getElementById('template2').loaded]" -- try repeatedly, since template may not load immediately
        << HxSwap "outerHTML" -- stop polling
        << XsltTemplate "template2" ?
            "loading..."

blog = box "blog" $ do
    Div << HxGet "https://blog.lahteenmaki.net" << HxSelect ".posts" << HxTrigger "load" ? "loading..."

junailua = box "railway stuff" $ do
    block "Rafiikka" "https://rafiikka.lahteenmaki.net" $
        Div ? "Experimental railway real-time graphics, work gaps, map and statistics"
    block "Infra-API" "https://rata.digitraffic.fi/infra-api" $
        Div ? "Finnish railway infrastructure, which I have something to do with"
    block "Jeti-API" "https://rata.digitraffic.fi/jeti-api" $
        Div ? "Finnish railway planned restrictions data, which I have something to do with"

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
