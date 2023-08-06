module Deku.DOM.Attr.AriaHaspopup where

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

data AriaHaspopup = AriaHaspopup

instance Attr Circle_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Circle_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Circle_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Circle_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Circle_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Ellipse_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Ellipse_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Ellipse_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Ellipse_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Ellipse_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr ForeignObject_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr ForeignObject_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr ForeignObject_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr ForeignObject_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr ForeignObject_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr G_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr G_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr G_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr G_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr G_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Line_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Line_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Line_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Line_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Line_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Marker_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Marker_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Marker_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Marker_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Marker_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Path_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Path_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Path_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Path_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Path_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Polygon_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Polygon_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Polygon_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Polygon_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Polygon_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Polyline_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Polyline_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Polyline_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Polyline_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Polyline_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Rect_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Rect_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Rect_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Rect_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Rect_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Svg_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Svg_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Svg_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Svg_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Svg_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Symbol_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Symbol_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Symbol_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Symbol_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Symbol_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Text_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Text_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Text_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Text_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Text_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr TextPath_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr TextPath_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr TextPath_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr TextPath_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr TextPath_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Tspan_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Tspan_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Tspan_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Tspan_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Tspan_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Use_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Use_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr Use_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr Use_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr Use_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr View_ AriaHaspopup (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr View_ AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-haspopup", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-haspopup", value: prop' value })
instance Attr View_ AriaHaspopup  String  where
  attr AriaHaspopup value = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: prop' value }
instance Attr View_ AriaHaspopup (Event.Event  String ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr View_ AriaHaspopup (ST.ST Global.Global  String ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-haspopup", value: prop' value }

instance Attr everything AriaHaspopup (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr AriaHaspopup bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-haspopup", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "aria-haspopup", value: unset' })
instance Attr everything AriaHaspopup (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr AriaHaspopup (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "aria-haspopup", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "aria-haspopup", value: unset' })
instance Attr everything AriaHaspopup  Unit  where
  attr AriaHaspopup _ = unsafeAttribute $ This $ pure $
    { key: "aria-haspopup", value: unset' }
instance Attr everything AriaHaspopup (Event.Event  Unit ) where
  attr AriaHaspopup eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "aria-haspopup", value: unset' }

instance Attr everything AriaHaspopup (ST.ST Global.Global  Unit ) where
  attr AriaHaspopup iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "aria-haspopup", value: unset' }
