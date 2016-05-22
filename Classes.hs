module Classes where

import Util.HTML.HTML5.Attributes (Class(..))
import Data.Text
import Clay.Selector (text)

container = Class "container"
section = Class "section"
subsection = Class "subsection"
header = Class "header"
footer = Class "footer"
books = Class "books"
boxcontent = Class "boxcontent"

sel (Class c) = text $ pack ("." ++ c)
