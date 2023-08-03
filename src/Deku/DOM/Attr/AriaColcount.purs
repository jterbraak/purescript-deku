module Deku.DOM.Attr.AriaColcount where

import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

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

data AriaColcount = AriaColcount

instance Attr Circle_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Circle_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Circle_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Ellipse_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Ellipse_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Ellipse_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr ForeignObject_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr ForeignObject_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr ForeignObject_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr G_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr G_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr G_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Line_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Line_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Line_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Marker_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Marker_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Marker_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Path_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Path_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Path_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Polygon_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Polygon_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Polygon_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Polyline_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Polyline_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Polyline_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Rect_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Rect_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Rect_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Svg_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Svg_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Svg_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Symbol_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Symbol_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Symbol_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Text_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Text_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Text_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr TextPath_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr TextPath_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr TextPath_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Tspan_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Tspan_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Tspan_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr Use_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr Use_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr Use_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr View_ AriaColcount (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-colcount", value: prop' value })
instance Attr View_ AriaColcount  String  where
  attr AriaColcount value = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: prop' value }
instance Attr View_ AriaColcount (Event.Event  String ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-colcount", value: prop' value }

instance Attr everything AriaColcount (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr AriaColcount bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-colcount", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "aria-colcount", value: unset' })
instance Attr everything AriaColcount  Unit  where
  attr AriaColcount _ = unsafeAttribute $ This $ pure $
    { key: "aria-colcount", value: unset' }
instance Attr everything AriaColcount (Event.Event  Unit ) where
  attr AriaColcount eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "aria-colcount", value: unset' }
