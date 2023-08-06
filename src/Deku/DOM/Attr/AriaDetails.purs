module Deku.DOM.Attr.AriaDetails where

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

data AriaDetails = AriaDetails

instance Attr Circle_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Circle_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Circle_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Circle_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Circle_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Ellipse_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Ellipse_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Ellipse_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Ellipse_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Ellipse_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr ForeignObject_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr ForeignObject_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr ForeignObject_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr ForeignObject_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr ForeignObject_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr G_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr G_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr G_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr G_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr G_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Line_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Line_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Line_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Line_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Line_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Marker_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Marker_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Marker_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Marker_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Marker_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Path_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Path_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Path_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Path_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Path_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Polygon_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Polygon_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Polygon_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Polygon_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Polygon_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Polyline_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Polyline_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Polyline_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Polyline_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Polyline_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Rect_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Rect_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Rect_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Rect_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Rect_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Svg_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Svg_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Svg_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Svg_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Svg_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Symbol_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Symbol_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Symbol_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Symbol_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Symbol_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Text_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Text_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Text_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Text_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Text_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr TextPath_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr TextPath_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr TextPath_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr TextPath_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr TextPath_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Tspan_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Tspan_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Tspan_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Tspan_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Tspan_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr Use_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Use_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr Use_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr Use_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr Use_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr View_ AriaDetails (NonEmpty.NonEmpty Event.Event  String ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr View_ AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "aria-details", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "aria-details", value: prop' value })
instance Attr View_ AriaDetails  String  where
  attr AriaDetails value = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: prop' value }
instance Attr View_ AriaDetails (Event.Event  String ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "aria-details", value: prop' value }

instance Attr View_ AriaDetails (ST.ST Global.Global  String ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "aria-details", value: prop' value }

instance Attr everything AriaDetails (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr AriaDetails bothValues = unsafeAttribute $ Both (pure 
    { key: "aria-details", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "aria-details", value: unset' })
instance Attr everything AriaDetails (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr AriaDetails (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "aria-details", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "aria-details", value: unset' })
instance Attr everything AriaDetails  Unit  where
  attr AriaDetails _ = unsafeAttribute $ This $ pure $
    { key: "aria-details", value: unset' }
instance Attr everything AriaDetails (Event.Event  Unit ) where
  attr AriaDetails eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "aria-details", value: unset' }

instance Attr everything AriaDetails (ST.ST Global.Global  Unit ) where
  attr AriaDetails iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "aria-details", value: unset' }
