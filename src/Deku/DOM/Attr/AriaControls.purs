module Deku.DOM.Attr.AriaControls where


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

data AriaControls = AriaControls

instance Attr Circle_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Circle_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Ellipse_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Ellipse_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr ForeignObject_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr ForeignObject_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr G_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr G_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Line_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Line_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Marker_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Marker_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Path_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Path_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Polygon_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Polygon_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Polyline_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Polyline_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Rect_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Rect_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Svg_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Svg_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Symbol_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Symbol_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Text_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Text_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr TextPath_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr TextPath_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Tspan_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Tspan_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr Use_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr Use_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr View_ AriaControls  String  where
  attr AriaControls value = unsafeAttribute (  
    { key: "aria-controls", value: prop' value  } <$ _)
instance Attr View_ AriaControls (Event.Event  String ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "aria-controls", value: prop' value }


instance Attr everything AriaControls  Unit  where
  attr AriaControls _ = unsafeAttribute (  
    { key: "aria-controls", value: unset'  } <$ _)
instance Attr everything AriaControls (Event.Event  Unit ) where
  attr AriaControls eventValue = unsafeAttribute \_ -> eventValue <#>
    \_ -> { key: "aria-controls", value: unset' }
