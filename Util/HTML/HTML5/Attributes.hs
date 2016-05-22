{-# LANGUAGE TypeFamilies, DataKinds #-}
module Util.HTML.HTML5.Attributes where

import Util.Markup


-- Global attributes
newtype Id = Id String
newtype Class = Class String
newtype StyleA = StyleA String
newtype Accesskey = Accesskey String
newtype Contenteditable = Contenteditable String
newtype Contextmenu = Contextmenu String
newtype Dir = Dir String
newtype Draggable = Draggable String
newtype Dropzone = Dropzone String
newtype Hidden = Hidden String
newtype Inert = Inert String
newtype Itemid = Itemid String
newtype Itemprop = Itemprop String
newtype Itemref = Itemref String
newtype Itemscope = Itemscope String
newtype Itemtype = Itemtype String
newtype Lang = Lang String
newtype Spellcheck = Spellcheck String
newtype Tabindex = Tabindex String
newtype TitleA = TitleA String
newtype Translate = Translate String

-- Global event handler attributes
newtype Onabort = Onabort String
newtype Onblur = Onblur String
newtype Oncancel = Oncancel String
newtype Oncanplay = Oncanplay String
newtype Oncanplaythrough = Oncanplaythrough String
newtype Onchange = Onchange String
newtype Onclick = Onclick String
newtype Onclose = Onclose String
newtype Oncontextmenu = Oncontextmenu String
newtype Oncuechange = Oncuechange String
newtype Ondblclick = Ondblclick String
newtype Ondrag = Ondrag String
newtype Ondragend = Ondragend String
newtype Ondragenter = Ondragenter String
newtype Ondragexit = Ondragexit String
newtype Ondragleave = Ondragleave String
newtype Ondragover = Ondragover String
newtype Ondragstart = Ondragstart String
newtype Ondrop = Ondrop String
newtype Ondurationchange = Ondurationchange String
newtype Onemptied = Onemptied String
newtype Onended = Onended String
newtype Onerror = Onerror String
newtype Onfocus = Onfocus String
newtype Oninput = Oninput String
newtype Oninvalid = Oninvalid String
newtype Onkeydown = Onkeydown String
newtype Onkeypress = Onkeypress String
newtype Onkeyup = Onkeyup String
newtype Onload = Onload String
newtype Onloadeddata = Onloadeddata String
newtype Onloadedmetadata = Onloadedmetadata String
newtype Onloadstart = Onloadstart String
newtype Onmousedown = Onmousedown String
newtype Onmouseenter = Onmouseenter String
newtype Onmouseleave = Onmouseleave String
newtype Onmousemove = Onmousemove String
newtype Onmouseout = Onmouseout String
newtype Onmouseover = Onmouseover String
newtype Onmouseup = Onmouseup String
newtype Onmousewheel = Onmousewheel String
newtype Onpause = Onpause String
newtype Onplay = Onplay String
newtype Onplaying = Onplaying String
newtype Onprogress = Onprogress String
newtype Onratechange = Onratechange String
newtype Onreset = Onreset String
newtype Onscroll = Onscroll String
newtype Onseeked = Onseeked String
newtype Onseeking = Onseeking String
newtype Onselect = Onselect String
newtype Onshow = Onshow String
newtype Onsort = Onsort String
newtype Onstalled = Onstalled String
newtype Onsubmit = Onsubmit String
newtype Onsuspend = Onsuspend String
newtype Ontimeupdate = Ontimeupdate String
newtype Onvolumechange = Onvolumechange String
newtype Onwaiting = Onwaiting String

data Data = Data String String

-- aria
type AriaRole = String
type AriaState = String
type AriaProperty = String
newtype Role = Role AriaRole
data Aria = AriaS AriaState String
          | AriaP AriaProperty String

newtype Type = Type String
newtype Src = Src String
newtype Rel = Rel String
newtype Href = Href String

newtype Name = Name String
newtype Content = Content String
newtype Property = Property String

-- Global attributes
instance AttributeLike Id              where toAttr (Id v)              = Attribute "id" v
instance AttributeLike Class           where toAttr (Class v)           = Attribute "class" v
instance AttributeLike StyleA          where toAttr (StyleA v)          = Attribute "style" v
instance AttributeLike Accesskey       where toAttr (Accesskey v)       = Attribute "accesskey" v
instance AttributeLike Contenteditable where toAttr (Contenteditable v) = Attribute "contenteditable" v
instance AttributeLike Contextmenu     where toAttr (Contextmenu v)     = Attribute "contextmenu" v
instance AttributeLike Dir             where toAttr (Dir v)             = Attribute "dir" v
instance AttributeLike Draggable       where toAttr (Draggable v)       = Attribute "draggable" v
instance AttributeLike Dropzone        where toAttr (Dropzone v)        = Attribute "dropzone" v
instance AttributeLike Hidden          where toAttr (Hidden v)          = Attribute "hidden" v
instance AttributeLike Inert           where toAttr (Inert v)           = Attribute "inert" v
instance AttributeLike Itemid          where toAttr (Itemid v)          = Attribute "itemid" v
instance AttributeLike Itemprop        where toAttr (Itemprop v)        = Attribute "itemprop" v
instance AttributeLike Itemref         where toAttr (Itemref v)         = Attribute "itemref" v
instance AttributeLike Itemscope       where toAttr (Itemscope v)       = Attribute "itemscope" v
instance AttributeLike Itemtype        where toAttr (Itemtype v)        = Attribute "itemtype" v
instance AttributeLike Lang            where toAttr (Lang v)            = Attribute "lang" v
instance AttributeLike Spellcheck      where toAttr (Spellcheck v)      = Attribute "spellcheck" v
instance AttributeLike Tabindex        where toAttr (Tabindex v)        = Attribute "tabindex" v
instance AttributeLike TitleA          where toAttr (TitleA v)          = Attribute "tabindex" v
instance AttributeLike Translate       where toAttr (Translate v)       = Attribute "translate" v

-- Global event handler attributes
instance AttributeLike Onabort where toAttr (Onabort v)   = Attribute "onabort" v
instance AttributeLike Onblur where toAttr (Onblur v)   = Attribute "onblur" v
instance AttributeLike Oncancel where toAttr (Oncancel v)   = Attribute "oncancel" v
instance AttributeLike Oncanplay where toAttr (Oncanplay v)   = Attribute "oncanplay" v
instance AttributeLike Oncanplaythrough where toAttr (Oncanplaythrough v)   = Attribute "oncanplaythrough" v
instance AttributeLike Onchange where toAttr (Onchange v)   = Attribute "onchange" v
instance AttributeLike Onclick where toAttr (Onclick v)   = Attribute "onclick" v
instance AttributeLike Onclose where toAttr (Onclose v)   = Attribute "onclose" v
instance AttributeLike Oncontextmenu where toAttr (Oncontextmenu v)   = Attribute "oncontextmenu" v
instance AttributeLike Oncuechange where toAttr (Oncuechange v)   = Attribute "oncuechange" v
instance AttributeLike Ondblclick where toAttr (Ondblclick v)   = Attribute "ondblclick" v
instance AttributeLike Ondrag where toAttr (Ondrag v)   = Attribute "ondrag" v
instance AttributeLike Ondragend where toAttr (Ondragend v)   = Attribute "ondragend" v
instance AttributeLike Ondragenter where toAttr (Ondragenter v)   = Attribute "ondragenter" v
instance AttributeLike Ondragexit where toAttr (Ondragexit v)   = Attribute "ondragexit" v
instance AttributeLike Ondragleave where toAttr (Ondragleave v)   = Attribute "ondragleave" v
instance AttributeLike Ondragover where toAttr (Ondragover v)   = Attribute "ondragover" v
instance AttributeLike Ondragstart where toAttr (Ondragstart v)   = Attribute "ondragstart" v
instance AttributeLike Ondrop where toAttr (Ondrop v)   = Attribute "ondrop" v
instance AttributeLike Ondurationchange where toAttr (Ondurationchange v)   = Attribute "ondurationchange" v
instance AttributeLike Onemptied where toAttr (Onemptied v)   = Attribute "onemptied" v
instance AttributeLike Onended where toAttr (Onended v)   = Attribute "onended" v
instance AttributeLike Onerror where toAttr (Onerror v)   = Attribute "onerror" v
instance AttributeLike Onfocus where toAttr (Onfocus v)   = Attribute "onfocus" v
instance AttributeLike Oninput where toAttr (Oninput v)   = Attribute "oninput" v
instance AttributeLike Oninvalid where toAttr (Oninvalid v)   = Attribute "oninvalid" v
instance AttributeLike Onkeydown where toAttr (Onkeydown v)   = Attribute "onkeydown" v
instance AttributeLike Onkeypress where toAttr (Onkeypress v)   = Attribute "onkeypress" v
instance AttributeLike Onkeyup where toAttr (Onkeyup v)   = Attribute "onkeyup" v
instance AttributeLike Onload where toAttr (Onload v)   = Attribute "onload" v
instance AttributeLike Onloadeddata where toAttr (Onloadeddata v)   = Attribute "onloadeddata" v
instance AttributeLike Onloadedmetadata where toAttr (Onloadedmetadata v)   = Attribute "onloadedmetadata" v
instance AttributeLike Onloadstart where toAttr (Onloadstart v)   = Attribute "onloadstart" v
instance AttributeLike Onmousedown where toAttr (Onmousedown v)   = Attribute "onmousedown" v
instance AttributeLike Onmouseenter where toAttr (Onmouseenter v)   = Attribute "onmouseenter" v
instance AttributeLike Onmouseleave where toAttr (Onmouseleave v)   = Attribute "onmouseleave" v
instance AttributeLike Onmousemove where toAttr (Onmousemove v)   = Attribute "onmousemove" v
instance AttributeLike Onmouseout where toAttr (Onmouseout v)   = Attribute "onmouseout" v
instance AttributeLike Onmouseover where toAttr (Onmouseover v)   = Attribute "onmouseover" v
instance AttributeLike Onmouseup where toAttr (Onmouseup v)   = Attribute "onmouseup" v
instance AttributeLike Onmousewheel where toAttr (Onmousewheel v)   = Attribute "onmousewheel" v
instance AttributeLike Onpause where toAttr (Onpause v)   = Attribute "onpause" v
instance AttributeLike Onplay where toAttr (Onplay v)   = Attribute "onplay" v
instance AttributeLike Onplaying where toAttr (Onplaying v)   = Attribute "onplaying" v
instance AttributeLike Onprogress where toAttr (Onprogress v)   = Attribute "onprogress" v
instance AttributeLike Onratechange where toAttr (Onratechange v)   = Attribute "onratechange" v
instance AttributeLike Onreset where toAttr (Onreset v)   = Attribute "onreset" v
instance AttributeLike Onscroll where toAttr (Onscroll v)   = Attribute "onscroll" v
instance AttributeLike Onseeked where toAttr (Onseeked v)   = Attribute "onseeked" v
instance AttributeLike Onseeking where toAttr (Onseeking v)   = Attribute "onseeking" v
instance AttributeLike Onselect where toAttr (Onselect v)   = Attribute "onselect" v
instance AttributeLike Onshow where toAttr (Onshow v)   = Attribute "onshow" v
instance AttributeLike Onsort where toAttr (Onsort v)   = Attribute "onsort" v
instance AttributeLike Onstalled where toAttr (Onstalled v)   = Attribute "onstalled" v
instance AttributeLike Onsubmit where toAttr (Onsubmit v)   = Attribute "onsubmit" v
instance AttributeLike Onsuspend where toAttr (Onsuspend v)   = Attribute "onsuspend" v
instance AttributeLike Ontimeupdate where toAttr (Ontimeupdate v)   = Attribute "ontimeupdate" v
instance AttributeLike Onvolumechange where toAttr (Onvolumechange v)   = Attribute "onvolumechange" v
instance AttributeLike Onwaiting where toAttr (Onwaiting v)   = Attribute "onwaiting" v

instance AttributeLike Data   where toAttr (Data n v)   = Attribute ("data-" ++ n) v

-- aria
instance AttributeLike Role   where toAttr (Role v)   = Attribute "role" v
instance AttributeLike Aria   where toAttr (AriaS n v) = Attribute ("aria-" ++ n) v
                                    toAttr (AriaP n v) = Attribute ("aria-" ++ n) v

instance AttributeLike Type   where toAttr (Type v)   = Attribute "type" v
instance AttributeLike Src    where toAttr (Src v)    = Attribute "src" v
instance AttributeLike Rel    where toAttr (Rel v)    = Attribute "rel" v
instance AttributeLike Href   where toAttr (Href v)   = Attribute "href" v

instance AttributeLike Name     where toAttr (Name v)     = Attribute "name" v
instance AttributeLike Content  where toAttr (Content v)  = Attribute "content" v
instance AttributeLike Property where toAttr (Property v) = Attribute "property" v