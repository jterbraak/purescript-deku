module Deku.DOM.Attr.Href where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.RadialGradient (RadialGradient_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.LinearGradient (LinearGradient_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.A (A_)
import Deku.DOM.Elt.Area (Area_)
import Deku.DOM.Elt.Base (Base_)
import Deku.DOM.Elt.Link (Link_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Href = Href

instance Attr A_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr A_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Area_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Area_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Base_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Base_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Link_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Link_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Image_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Image_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr LinearGradient_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr LinearGradient_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Pattern_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Pattern_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr RadialGradient_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr RadialGradient_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr TextPath_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr TextPath_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr Use_ Href  String  where
  attr Href value = unsafeAttribute (  
    { key: "href", value: prop' value  } <$ _)
instance Attr Use_ Href (Event.Event  String ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \value ->
    { key: "href", value: prop' value }


instance Attr everything Href  Unit  where
  attr Href _ = unsafeAttribute (  { key: "href", value: unset'  } <$ _)
instance Attr everything Href (Event.Event  Unit ) where
  attr Href eventValue = unsafeAttribute \_ -> eventValue <#> \_ ->
    { key: "href", value: unset' }
