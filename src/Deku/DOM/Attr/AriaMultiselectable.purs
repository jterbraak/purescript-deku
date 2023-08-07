module Deku.DOM.Attr.AriaMultiselectable where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.View (View_)
import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Circle (Circle_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data AriaMultiselectable = AriaMultiselectable

instance Attr Circle_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Circle_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Ellipse_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Ellipse_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr ForeignObject_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr ForeignObject_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr G_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr G_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Line_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Line_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Marker_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Marker_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Path_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Path_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Polygon_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Polygon_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Polyline_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Polyline_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Rect_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Rect_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Svg_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Svg_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Symbol_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Symbol_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Text_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Text_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr TextPath_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr TextPath_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Tspan_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Tspan_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr Use_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr Use_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr View_ AriaMultiselectable  String  where
  attr AriaMultiselectable value = unsafeAttribute (  
    { key: "aria-multiselectable", value: prop' value  } <$ _)
instance Attr View_ AriaMultiselectable (Event.Event  String ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \value -> { key: "aria-multiselectable", value: prop' value }


instance Attr everything AriaMultiselectable  Unit  where
  attr AriaMultiselectable _ = unsafeAttribute (  
    { key: "aria-multiselectable", value: unset'  } <$ _)
instance Attr everything AriaMultiselectable (Event.Event  Unit ) where
  attr AriaMultiselectable eventValue = unsafeAttribute \_ ->
    eventValue <#> \_ -> { key: "aria-multiselectable", value: unset' }
