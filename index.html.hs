#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p 'haskellPackages.ghcWithPackages(p: with p; [text clay])'
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts, TypeFamilies #-}
import Data.Text.Lazy (unpack)

import Util.Markup
import Util.HTML.HTML5
import Util.HTML.HTML5.Attributes (Class(..))

main = putStrLn $ unpack $ render content

content :: Markup
content = ("text/html",) $ Html ? do
    Head ? do
        Title ? "Lähteenmäki.net | jyri-matti lähteenmäki"
        metaDescription "Homepage of Jyri-Matti Lähteenmäki | programmer, designer, architect, haskeller. Father and husband."
        metaKeywords ["jyri-matti lähteenmäki", "programming", "developer", "software", "computer science", "haskell", "scala"]
        metaOGTitle "Lähteenmäki.net | jyri-matti lähteenmäki"
        metaOGType "website"
        metaOGUrl "http://www.lahteenmaki.net"
        metaOGSiteName "Lähteenmäki.net"
        Meta << Name "viewport" << Content "width=500, initial-scale=1" ? empty
        Script << Type "text/javascript" ? "setTimeout(function() {document.getElementById('businfo').outerHTML = 'Sovellus on käytettävissä osoitteessa: http://lahteenmaki.net/bus';}, 30000)"
        css "style.css"
        Script ? analytics
    Body ? do
        Div << Class "container" ? do
            Div << Class "header" ? do
                H1 ? "jyri-matti lähteenmäki"
            Div << Class "section" ? presentations
            Div << Class "section" ? javastuff
            Div << Class "section" ? businfo
            Div << Class "section" ? tweets
            Div << Class "section" << Class "books" ? books
            Div << Class "section" ? ohjelmointi
            Div << Class "section" ? perhe
            Div << Class "section" ? yhteys
            Div << Class "section" ? tamasivu
            Div << Class "footer" ? "© Jyri-Matti Lähteenmäki 2014"

box name body = do
    H2 ? name
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

presentations = box "presentations" $ do
    block "Composing functions and beyond" "http://lahteenmaki.net/dev_*15/" $
        Div ? "Investigating composition for different kinds of functions"
    block "Functors, Applicatives, Monads ...WAT?" "http://lahteenmaki.net/dev_*14/" $
        Div ? "Explaining Monads and stuff"
    block "to Type a Class" "http://lahteenmaki.net/dev_*/" $
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
        Li ? "etunimi@sukunimi.net"
        Li ? do A << Href "https://pgp.mit.edu/pks/lookup?search=0x1959b8a4f22fec94&fingerprint=on" ? "F22FEC94"
        Li ? ahref "https://plus.google.com/+Jyri-MattiLähteenmäki"
        Li ? ahref "https://twitter.com/jyrimatti"
        Li ? ahref "https://pinboard.in/u:jyrimatti/"
        Li ? ahref "https://github.com/jyrimatti"
        Li ? ahref "http://www.linkedin.com/in/jyrimatti"
        Li ? ahref "https://www.goodreads.com/jyrimatti"
        Li ? ahref "https://www.facebook.com/jyrimatti.lahteenmaki"
        Li ? ahref "https://vimeo.com/jyrimatti"

ohjelmointi = box "dev" $ do
    block "flow" $
        Div ? do
            A << Href "https://twitter.com/jyrimatti" ? "Twitter"
    block "blog" $
        Div ? do
            ahref "http://blog.lahteenmaki.net"
    block "thoughts" $
        Div ? do
            A << Href "https://plus.google.com/102784575413360548836/posts" ? "google+"
    block "links" $
        Div ? do
            A << Href "https://pinboard.in/u:jyrimatti/" ? "Pinboard"
    block "projects" $
        Div ? do
            A << Href "http://github.com/jyrimatti" ? "GitHub"
  where block title body = Div << Class "subsection" ? do
                               H3 ? title
                               body

tweets = box "tweets" $ do
    A << Class "twitter-timeline" << Href "https://twitter.com/jyrimatti" << Data "widget-id" "331834452940570626" << Data "chrome" "noheader nofooter transparent noborders" ? "Tweets by @jyrimatti"
    Script ? raw "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+\"://platform.twitter.com/widgets.js\";fjs.parentNode.insertBefore(js,fjs);}}(document,\"script\",\"twitter-wjs\");"

books = box "read books" $ do
    Script << Src "https://www.goodreads.com/review/custom_widget/29730596.-?cover_position=left&cover_size=small&num_books=100&order=d&shelf=read&show_author=1&show_cover=1&show_rating=0&show_review=0&show_tags=0&show_title=1&sort=date_read&widget_bg_color=FFFFFF&widget_bg_transparent=true&widget_border_width=1&widget_id=1435785230&widget_text_color=000000&widget_title_size=small&widget_width=full" ? empty

businfo = box "bus info" $ do
    IFrame << Src "/bus" << Id "businfo" ? empty

tamasivu = box "this site" $ do
    Ul ? do
        Li ? "FastCGI. Apache. Haskell."
        Li ? "Really-type-safe HTML. Haskell."
        Li ? do
            "Styles "
            A << Href "http://fvisser.nl/clay/" ? "Clay"
            ". Haskell."

analytics = "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\
\  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\
\  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\
\  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');\
\\
\  ga('create', 'UA-20222288-1', 'auto');\
\  ga('send', 'pageview');"
