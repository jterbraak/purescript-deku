module Deku.DOM.Attr.PreserveAspectRatio where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.View (View_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.FeImage (FeImage_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data PreserveAspectRatio = PreserveAspectRatio

instance Attr FeImage_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr FeImage_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr Image_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr Image_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr Marker_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr Marker_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr Pattern_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr Pattern_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr Svg_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr Svg_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr Symbol_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr Symbol_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr View_ PreserveAspectRatio  String  where
  attr PreserveAspectRatio value = unsafeAttribute (  
    { key: "preserveAspectRatio", value: prop' value  } <$ _)
instance Attr View_ PreserveAspectRatio (Event.Event  String ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "preserveAspectRatio", value: prop' value }


instance Attr everything PreserveAspectRatio  Unit  where
  attr PreserveAspectRatio _ = unsafeAttribute (  
    { key: "preserveAspectRatio", value: unset'  } <$ _)
instance Attr everything PreserveAspectRatio (Event.Event  Unit ) where
  attr PreserveAspectRatio eventValue = unsafeAttribute \_ ->
    eventValue <#> \_ -> { key: "preserveAspectRatio", value: unset' }
