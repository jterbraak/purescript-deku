module Deku.DOM.Attr.AriaKeyshortcuts where

import Prelude

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

data AriaKeyshortcuts = AriaKeyshortcuts

instance Attr Circle_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Ellipse_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr ForeignObject_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr G_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Line_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Marker_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Path_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Polygon_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Polyline_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Rect_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Svg_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Symbol_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Text_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr TextPath_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Tspan_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr Use_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr View_ AriaKeyshortcuts String where
  attr AriaKeyshortcuts value = unsafeAttribute
    { key: "aria-keyshortcuts", value: prop' value }

instance Attr everything AriaKeyshortcuts Unit where
  attr AriaKeyshortcuts _ = unsafeAttribute
    { key: "aria-keyshortcuts", value: unset' }
