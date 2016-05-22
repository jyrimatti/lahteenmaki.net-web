{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Util.HTML where

import Util.HTML.HTML5.Attributes
import Util.HTML.HTML5.Elements
import Util.HTML.HTML5.Constraints()
import Util.HTML.HTML5
import Util.Markup


program :: Markup
program = ("text/html",) $ Html << Id "foo" << Id "bar" ? do
     Head << Id "" ? do
        Link ? empty
        Title << Id "" ? "moi"
     Body ? do
        Div ? do
            Div << Class "baz" ? "3 < 4"
            ahref "http://"
        Div << Id "dd" ? empty
    

bar = render program
