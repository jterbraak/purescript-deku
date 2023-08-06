module Deku.DOM.Attr.AriaValuenow where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
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

data AriaValuenow = AriaValuenow

instance Attr Circle_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Circle_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Circle_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Circle_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Circle_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Ellipse_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Ellipse_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Ellipse_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Ellipse_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Ellipse_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr ForeignObject_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr ForeignObject_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr ForeignObject_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr ForeignObject_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr ForeignObject_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr G_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr G_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr G_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr G_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr G_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Line_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Line_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Line_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Line_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Line_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Marker_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Marker_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Marker_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Marker_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Marker_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Path_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Path_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Path_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Path_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Path_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Polygon_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Polygon_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Polygon_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Polygon_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Polygon_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Polyline_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Polyline_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Polyline_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Polyline_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Polyline_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Rect_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Rect_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Rect_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Rect_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Rect_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Svg_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Svg_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Svg_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Svg_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Svg_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Symbol_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Symbol_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Symbol_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Symbol_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Symbol_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Text_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Text_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Text_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Text_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Text_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr TextPath_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr TextPath_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr TextPath_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr TextPath_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr TextPath_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Tspan_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Tspan_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Tspan_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Tspan_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Tspan_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Use_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Use_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr Use_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr Use_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr Use_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr View_ AriaValuenow (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr View_ AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-valuenow", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-valuenow", value: prop' value })
instance Attr View_ AriaValuenow  String  where
  attr AriaValuenow value = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: prop' value }
instance Attr View_ AriaValuenow (Event.Event  String ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr View_ AriaValuenow (ST.ST Global.Global  String ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-valuenow", value: prop' value }

instance Attr everything AriaValuenow (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr AriaValuenow bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-valuenow", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "aria-valuenow", value: unset' })
instance Attr everything AriaValuenow (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr AriaValuenow (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "aria-valuenow", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "aria-valuenow", value: unset' })
instance Attr everything AriaValuenow  Unit  where
  attr AriaValuenow _ = unsafeAttribute $ This $ pure $
    { key: "aria-valuenow", value: unset' }
instance Attr everything AriaValuenow (Event.Event  Unit ) where
  attr AriaValuenow eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "aria-valuenow", value: unset' }

instance Attr everything AriaValuenow (ST.ST Global.Global  Unit ) where
  attr AriaValuenow iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "aria-valuenow", value: unset' }
