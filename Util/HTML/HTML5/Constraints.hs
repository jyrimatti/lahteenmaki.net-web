{-# LANGUAGE TypeFamilies, DataKinds #-}
module Util.HTML.HTML5.Constraints where

import Util.Markup
import Util.HTML.HTML5.Elements
import Util.HTML.HTML5.Attributes


-- Other (unknown) element may have any attributes by default
type instance LegalAttribute Other any = Legal

-- Global attributes, legal for any element
type instance LegalAttribute any Id = Legal
type instance LegalAttribute any Class = Legal
type instance LegalAttribute any StyleA = Legal
type instance LegalAttribute any Accesskey = Legal
type instance LegalAttribute any Contenteditable = Legal
type instance LegalAttribute any Contextmenu = Legal
type instance LegalAttribute any Dir = Legal
type instance LegalAttribute any Draggable = Legal
type instance LegalAttribute any Dropzone = Legal
type instance LegalAttribute any Hidden = Legal
type instance LegalAttribute any Inert = Legal
type instance LegalAttribute any Itemid = Legal
type instance LegalAttribute any Itemprop = Legal
type instance LegalAttribute any Itemref = Legal
type instance LegalAttribute any Itemscope = Legal
type instance LegalAttribute any Itemtype = Legal
type instance LegalAttribute any Lang = Legal
type instance LegalAttribute any Spellcheck = Legal
type instance LegalAttribute any Tabindex = Legal
type instance LegalAttribute any TitleA = Legal
type instance LegalAttribute any Translate = Legal

-- Global event handler attributes
type instance LegalAttribute any Onabort = Legal
type instance LegalAttribute any Onblur = Legal
type instance LegalAttribute any Oncancel = Legal
type instance LegalAttribute any Oncanplay = Legal
type instance LegalAttribute any Oncanplaythrough = Legal
type instance LegalAttribute any Onchange = Legal
type instance LegalAttribute any Onclick = Legal
type instance LegalAttribute any Onclose = Legal
type instance LegalAttribute any Oncontextmenu = Legal
type instance LegalAttribute any Oncuechange = Legal
type instance LegalAttribute any Ondblclick = Legal
type instance LegalAttribute any Ondrag = Legal
type instance LegalAttribute any Ondragend = Legal
type instance LegalAttribute any Ondragenter = Legal
type instance LegalAttribute any Ondragexit = Legal
type instance LegalAttribute any Ondragleave = Legal
type instance LegalAttribute any Ondragover = Legal
type instance LegalAttribute any Ondragstart = Legal
type instance LegalAttribute any Ondrop = Legal
type instance LegalAttribute any Ondurationchange = Legal
type instance LegalAttribute any Onemptied = Legal
type instance LegalAttribute any Onended = Legal
type instance LegalAttribute any Onerror = Legal
type instance LegalAttribute any Onfocus = Legal
type instance LegalAttribute any Onhashchange = Legal
type instance LegalAttribute any Oninput = Legal
type instance LegalAttribute any Oninvalid = Legal
type instance LegalAttribute any Onkeydown = Legal
type instance LegalAttribute any Onkeypress = Legal
type instance LegalAttribute any Onkeyup = Legal
type instance LegalAttribute any Onload = Legal
type instance LegalAttribute any Onloadeddata = Legal
type instance LegalAttribute any Onloadedmetadata = Legal
type instance LegalAttribute any Onloadstart = Legal
type instance LegalAttribute any Onmousedown = Legal
type instance LegalAttribute any Onmouseenter = Legal
type instance LegalAttribute any Onmouseleave = Legal
type instance LegalAttribute any Onmousemove = Legal
type instance LegalAttribute any Onmouseout = Legal
type instance LegalAttribute any Onmouseover = Legal
type instance LegalAttribute any Onmouseup = Legal
type instance LegalAttribute any Onmousewheel = Legal
type instance LegalAttribute any Onpause = Legal
type instance LegalAttribute any Onplay = Legal
type instance LegalAttribute any Onplaying = Legal
type instance LegalAttribute any Onprogress = Legal
type instance LegalAttribute any Onratechange = Legal
type instance LegalAttribute any Onreset = Legal
type instance LegalAttribute any Onscroll = Legal
type instance LegalAttribute any Onseeked = Legal
type instance LegalAttribute any Onseeking = Legal
type instance LegalAttribute any Onselect = Legal
type instance LegalAttribute any Onshow = Legal
type instance LegalAttribute any Onsort = Legal
type instance LegalAttribute any Onstalled = Legal
type instance LegalAttribute any Onsubmit = Legal
type instance LegalAttribute any Onsuspend = Legal
type instance LegalAttribute any Ontimeupdate = Legal
type instance LegalAttribute any Onvolumechange = Legal
type instance LegalAttribute any Onwaiting = Legal

type instance LegalAttribute any Data = Legal

type instance LegalAttribute Link As = Legal
type instance LegalAttribute Link Type = Legal
type instance LegalAttribute Link Href = Legal
type instance LegalAttribute Link Rel = Legal

-- aria
type instance LegalAttribute any Role = Legal
type instance LegalAttribute any Aria = Legal

type instance LegalAttribute Script Type = Legal
type instance LegalAttribute Script Src = Legal
type instance LegalAttribute Link Rel = Legal
type instance LegalAttribute Link Href = Legal
type instance LegalAttribute Link Type = Legal
type instance LegalAttribute A Href = Legal
type instance LegalAttribute IFrame Src = Legal

type instance LegalAttribute Meta Name = Legal
type instance LegalAttribute Meta Content = Legal
type instance LegalAttribute Meta Property = Legal

type instance LegalAttribute Object Id = Legal
type instance LegalAttribute Object Data_ = Legal

type instance LegalAttribute Input Type = Legal
type instance LegalAttribute Input Name = Legal
type instance LegalAttribute Input Checked = Legal
type instance LegalAttribute Label For = Legal

type instance LegalAttribute any HxGet = Legal
type instance LegalAttribute any HxSelect = Legal
type instance LegalAttribute any HxTrigger = Legal
type instance LegalAttribute any HxSwap = Legal
type instance LegalAttribute any HxExt = Legal
type instance LegalAttribute any XsltTemplate = Legal
type instance LegalAttribute any Script_ = Legal



-- Other (unknown) elements may appear anywhere by default
type instance LegalChild Other any = Legal
type instance LegalChild any Other = Legal

type instance LegalChild Document Html = Legal
type instance LegalChild Html Head = Legal
type instance LegalChild Html Body = Legal
type instance LegalChild Head Title = Legal
type instance LegalChild Head Link = Legal
type instance LegalChild Head Script = Legal
type instance LegalChild Head Meta = Legal
type instance LegalChild Body Div = Legal
type instance LegalChild Body Input = Legal
type instance LegalChild Body Label = Legal
type instance LegalChild Body Object = Legal
type instance LegalChild Nav Label = Legal
type instance LegalChild Nav A = Legal
type instance LegalChild Label String = Legal
type instance LegalChild Label A = Legal
type instance LegalChild Div Div = Legal
type instance LegalChild Div H1 = Legal
type instance LegalChild Div H2 = Legal
type instance LegalChild Div H3 = Legal
type instance LegalChild Div A = Legal
type instance LegalChild Div Input = Legal
type instance LegalChild Div Label = Legal
type instance LegalChild Div Script = Legal
type instance LegalChild A Script = Legal
type instance LegalChild Div Nav = Legal
type instance LegalChild Div Section = Legal
type instance LegalChild Div Header = Legal
type instance LegalChild Div Footer = Legal
type instance LegalChild Div Article = Legal
type instance LegalChild Div Main = Legal
type instance LegalChild Main Div = Legal
type instance LegalChild Script String = Legal
type instance LegalChild H2 A = Legal
type instance LegalChild H2 Label = Legal
type instance LegalChild H3 A = Legal
type instance LegalChild Div Ul = Legal
type instance LegalChild Div Ol = Legal
type instance LegalChild Div String = Legal
type instance LegalChild Ul Li = Legal
type instance LegalChild Ol Li = Legal
type instance LegalChild Li A = Legal
type instance LegalChild Li String = Legal
type instance LegalChild H1 String = Legal
type instance LegalChild H2 String = Legal
type instance LegalChild H3 String = Legal
type instance LegalChild Title String = Legal
type instance LegalChild A String = Legal
type instance LegalChild Div IFrame = Legal
type instance LegalChild Section Div = Legal
type instance LegalChild Header H1 = Legal
type instance LegalChild Section H2 = Legal
type instance LegalChild Main Footer = Legal
type instance LegalChild Footer String = Legal
