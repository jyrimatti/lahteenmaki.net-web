{-# LANGUAGE TypeFamilies, DataKinds #-}
module Util.HTML.HTML5.Elements where

import Util.Markup


data Other  = Other String [Attribute]

newtype Html   = Html [Attribute]
newtype Head   = Head [Attribute]
newtype Title  = Title [Attribute]
newtype Body   = Body [Attribute]
newtype Nav    = Nav [Attribute]

newtype Div    = Div [Attribute]

newtype Input  = Input [Attribute]
newtype Label  = Label [Attribute]

newtype H1     = H1 [Attribute]
newtype H2     = H2 [Attribute]
newtype H3     = H3 [Attribute]
newtype H4     = H4 [Attribute]
newtype H5     = H5 [Attribute]
newtype H6     = H6 [Attribute]

newtype Ul     = Ul [Attribute]
newtype Ol     = Ol [Attribute]
newtype Dl     = Dl [Attribute]
newtype Li     = Li [Attribute]
newtype Dt     = Dt [Attribute]
newtype Dd     = Dd [Attribute]

newtype A       = A [Attribute]
newtype Link    = Link [Attribute]
newtype Script  = Script [Attribute]
newtype Style   = Style [Attribute]
newtype IFrame  = IFrame [Attribute]
newtype Section = Section [Attribute]
newtype Header  = Header [Attribute]
newtype Footer  = Footer [Attribute]
newtype Article = Article [Attribute]
newtype Main    = Main [Attribute]

newtype Meta   = Meta [Attribute]
newtype Object = Object [Attribute]

instance NodeLike Other  where val = Other ""; name (Other n _) = n; attr (Other _ aa) = aa
instance NodeLike Html   where val = Html    ; name _ = "html"     ; attr (Html aa)    = aa
instance NodeLike Head   where val = Head    ; name _ = "head"     ; attr (Head aa)    = aa
instance NodeLike Title  where val = Title   ; name _ = "title"    ; attr (Title aa)   = aa
instance NodeLike Body   where val = Body    ; name _ = "body"     ; attr (Body aa)    = aa
instance NodeLike Nav    where val = Nav     ; name _ = "nav"      ; attr (Nav aa)     = aa
instance NodeLike Main    where val = Main    ; name _ = "main"    ; attr (Main aa)    = aa
instance NodeLike Article where val = Article ; name _ = "article" ; attr (Article aa) = aa
instance NodeLike Header  where val = Header  ; name _ = "header"  ; attr (Header aa)  = aa
instance NodeLike Section where val = Section ; name _ = "section" ; attr (Section aa) = aa
instance NodeLike Footer  where val = Footer  ; name _ = "footer"  ; attr (Footer aa)  = aa

instance NodeLike Div    where val = Div     ; name _ = "div"      ; attr (Div aa)     = aa

instance NodeLike Input  where val = Input   ; name _ = "input"    ; attr (Input aa)   = aa
instance NodeLike Label  where val = Label   ; name _ = "label"    ; attr (Label aa)   = aa

instance NodeLike H1     where val = H1      ; name _ = "h1"       ; attr (H1 aa)      = aa
instance NodeLike H2     where val = H2      ; name _ = "h2"       ; attr (H2 aa)      = aa
instance NodeLike H3     where val = H3      ; name _ = "h3"       ; attr (H3 aa)      = aa
instance NodeLike H4     where val = H4      ; name _ = "h4"       ; attr (H4 aa)      = aa
instance NodeLike H5     where val = H5      ; name _ = "h5"       ; attr (H5 aa)      = aa
instance NodeLike H6     where val = H6      ; name _ = "h6"       ; attr (H6 aa)      = aa

instance NodeLike Ul     where val = Ul      ; name _ = "ul"       ; attr (Ul aa)      = aa
instance NodeLike Ol     where val = Ol      ; name _ = "ol"       ; attr (Ol aa)      = aa
instance NodeLike Dl     where val = Dl      ; name _ = "dl"       ; attr (Dl aa)      = aa
instance NodeLike Li     where val = Li      ; name _ = "li"       ; attr (Li aa)      = aa
instance NodeLike Dt     where val = Dt      ; name _ = "dt"       ; attr (Dt aa)      = aa
instance NodeLike Dd     where val = Dd      ; name _ = "dd"       ; attr (Dd aa)      = aa

instance NodeLike A      where val = A       ; name _ = "a"        ; attr (A aa)       = aa
instance NodeLike Link   where val = Link    ; name _ = "link"     ; attr (Link aa)    = aa ; isVoid _ = True
instance NodeLike Script where val = Script  ; name _ = "script"   ; attr (Script aa)  = aa
instance NodeLike Style  where val = Style   ; name _ = "style"    ; attr (Style aa)   = aa
instance NodeLike IFrame where val = IFrame  ; name _ = "iframe"   ; attr (IFrame aa)  = aa

instance NodeLike Meta   where val = Meta    ; name _ = "meta"     ; attr (Meta aa)    = aa ; isVoid _ = True
instance NodeLike Object where val = Object  ; name _ = "object"   ; attr (Object aa)  = aa
