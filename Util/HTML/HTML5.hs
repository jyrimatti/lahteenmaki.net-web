{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies  #-}
module Util.HTML.HTML5 (
    module X,
    ahref,
    css,
    javascript,
    metaDescription,
    metaKeywords,
    metaOGTitle,
    metaOGType,
    metaOGUrl,
    metaOGImage,
    metaOGSiteName,
    metaCharset
) where
import Util.HTML.HTML5.Attributes as X
import Util.HTML.HTML5.Elements as X
import Util.HTML.HTML5.Constraints as X ()

import Util.Markup

metaDescription s = Meta << Name "description" << Content s ? empty
metaKeywords words = Meta << Name "keywords" << Content (foldr1 (\a b -> a ++ ", " ++ b) words) ? empty

ahref href      = A << Href href ? text href
css href        = Link << Rel "stylesheet" << Type "text/css" << Href href ? empty

javascript src  = Script << Type "text/javascript" << Src src ? empty
coffeescript src  = Script << Type "text/coffeescript" << Src src ? empty

og p a = Meta << Property ("og:" ++ p) << Content a ? empty
metaOGTitle = og "title"
metaOGType = og "type"
metaOGUrl = og "url"
metaOGImage = og "image"
metaOGSiteName = og "site_name"
metaCharset = og "charset"
