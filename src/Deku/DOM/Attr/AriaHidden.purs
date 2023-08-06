module Deku.DOM.Attr.AriaHidden where

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

data AriaHidden = AriaHidden

instance Attr Circle_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Circle_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Circle_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Circle_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Circle_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Ellipse_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Ellipse_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Ellipse_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Ellipse_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Ellipse_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr ForeignObject_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr ForeignObject_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr ForeignObject_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr ForeignObject_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr ForeignObject_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr G_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr G_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr G_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr G_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr G_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Line_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Line_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Line_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Line_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Line_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Marker_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Marker_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Marker_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Marker_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Marker_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Path_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Path_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Path_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Path_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Path_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Polygon_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Polygon_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Polygon_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Polygon_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Polygon_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Polyline_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Polyline_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Polyline_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Polyline_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Polyline_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Rect_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Rect_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Rect_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Rect_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Rect_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Svg_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Svg_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Svg_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Svg_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Svg_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Symbol_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Symbol_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Symbol_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Symbol_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Symbol_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Text_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Text_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Text_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Text_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Text_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr TextPath_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr TextPath_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr TextPath_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr TextPath_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr TextPath_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Tspan_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Tspan_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Tspan_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Tspan_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Tspan_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Use_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Use_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr Use_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr Use_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr Use_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr View_ AriaHidden (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr View_ AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-hidden", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-hidden", value: prop' value })
instance Attr View_ AriaHidden  String  where
  attr AriaHidden value = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: prop' value }
instance Attr View_ AriaHidden (Event.Event  String ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr View_ AriaHidden (ST.ST Global.Global  String ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-hidden", value: prop' value }

instance Attr everything AriaHidden (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr AriaHidden bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-hidden", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "aria-hidden", value: unset' })
instance Attr everything AriaHidden (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr AriaHidden (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "aria-hidden", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "aria-hidden", value: unset' })
instance Attr everything AriaHidden  Unit  where
  attr AriaHidden _ = unsafeAttribute $ This $ pure $
    { key: "aria-hidden", value: unset' }
instance Attr everything AriaHidden (Event.Event  Unit ) where
  attr AriaHidden eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "aria-hidden", value: unset' }

instance Attr everything AriaHidden (ST.ST Global.Global  Unit ) where
  attr AriaHidden iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "aria-hidden", value: unset' }
