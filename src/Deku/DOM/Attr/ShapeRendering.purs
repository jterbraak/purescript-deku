module Deku.DOM.Attr.ShapeRendering where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Switch (Switch_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.RadialGradient (RadialGradient_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Mask (Mask_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.LinearGradient (LinearGradient_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Filter (Filter_)
import Deku.DOM.Elt.FeTurbulence (FeTurbulence_)
import Deku.DOM.Elt.FeTile (FeTile_)
import Deku.DOM.Elt.FeSpecularLighting (FeSpecularLighting_)
import Deku.DOM.Elt.FeOffset (FeOffset_)
import Deku.DOM.Elt.FeMorphology (FeMorphology_)
import Deku.DOM.Elt.FeMerge (FeMerge_)
import Deku.DOM.Elt.FeImage (FeImage_)
import Deku.DOM.Elt.FeGaussianBlur (FeGaussianBlur_)
import Deku.DOM.Elt.FeFlood (FeFlood_)
import Deku.DOM.Elt.FeDisplacementMap (FeDisplacementMap_)
import Deku.DOM.Elt.FeDiffuseLighting (FeDiffuseLighting_)
import Deku.DOM.Elt.FeConvolveMatrix (FeConvolveMatrix_)
import Deku.DOM.Elt.FeComposite (FeComposite_)
import Deku.DOM.Elt.FeComponentTransfer (FeComponentTransfer_)
import Deku.DOM.Elt.FeColorMatrix (FeColorMatrix_)
import Deku.DOM.Elt.FeBlend (FeBlend_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Defs (Defs_)
import Deku.DOM.Elt.ClipPath (ClipPath_)
import Deku.DOM.Elt.Circle (Circle_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data ShapeRendering = ShapeRendering

instance Attr Circle_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Circle_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Circle_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Circle_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Circle_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr ClipPath_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr ClipPath_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr ClipPath_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr ClipPath_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr ClipPath_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Defs_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Defs_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Defs_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Defs_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Defs_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Ellipse_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Ellipse_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Ellipse_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Ellipse_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Ellipse_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeBlend_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeBlend_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeBlend_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeBlend_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeBlend_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeColorMatrix_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeColorMatrix_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeColorMatrix_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeColorMatrix_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeColorMatrix_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeComponentTransfer_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeComponentTransfer_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeComponentTransfer_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeComponentTransfer_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeComponentTransfer_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeComposite_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeComposite_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeComposite_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeComposite_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeComposite_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeConvolveMatrix_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeConvolveMatrix_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeConvolveMatrix_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeConvolveMatrix_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeConvolveMatrix_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeDiffuseLighting_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeDiffuseLighting_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeDiffuseLighting_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeDiffuseLighting_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeDiffuseLighting_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeDisplacementMap_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeDisplacementMap_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeDisplacementMap_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeDisplacementMap_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeDisplacementMap_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeFlood_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeFlood_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeFlood_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeFlood_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeFlood_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeGaussianBlur_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeGaussianBlur_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeGaussianBlur_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeGaussianBlur_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeGaussianBlur_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeImage_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeImage_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeImage_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeImage_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeImage_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeMerge_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeMerge_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeMerge_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeMerge_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeMerge_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeMorphology_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeMorphology_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeMorphology_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeMorphology_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeMorphology_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeOffset_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeOffset_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeOffset_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeOffset_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeOffset_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeSpecularLighting_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeSpecularLighting_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeSpecularLighting_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeSpecularLighting_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeSpecularLighting_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeTile_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeTile_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeTile_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeTile_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeTile_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeTurbulence_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeTurbulence_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr FeTurbulence_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr FeTurbulence_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr FeTurbulence_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Filter_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Filter_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Filter_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Filter_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Filter_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr ForeignObject_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr ForeignObject_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr ForeignObject_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr ForeignObject_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr ForeignObject_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr G_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr G_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr G_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr G_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr G_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Image_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Image_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Image_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Image_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Image_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Line_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Line_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Line_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Line_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Line_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr LinearGradient_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr LinearGradient_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr LinearGradient_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr LinearGradient_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr LinearGradient_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Marker_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Marker_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Marker_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Marker_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Marker_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Mask_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Mask_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Mask_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Mask_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Mask_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Path_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Path_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Path_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Path_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Path_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Pattern_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Pattern_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Pattern_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Pattern_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Pattern_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Polygon_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Polygon_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Polygon_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Polygon_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Polygon_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Polyline_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Polyline_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Polyline_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Polyline_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Polyline_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr RadialGradient_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr RadialGradient_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr RadialGradient_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr RadialGradient_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr RadialGradient_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Rect_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Rect_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Rect_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Rect_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Rect_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Svg_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Svg_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Svg_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Svg_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Svg_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Switch_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Switch_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Switch_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Switch_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Switch_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Symbol_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Symbol_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Symbol_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Symbol_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Symbol_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Text_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Text_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Text_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Text_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Text_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr TextPath_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr TextPath_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr TextPath_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr TextPath_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr TextPath_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Tspan_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Tspan_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Tspan_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Tspan_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Tspan_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Use_ ShapeRendering (NonEmpty.NonEmpty Event.Event  String ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Use_ ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "shape-rendering", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "shape-rendering", value: prop' value }
    )
instance Attr Use_ ShapeRendering  String  where
  attr ShapeRendering value = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: prop' value }
instance Attr Use_ ShapeRendering (Event.Event  String ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr Use_ ShapeRendering (ST.ST Global.Global  String ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "shape-rendering", value: prop' value }

instance Attr everything ShapeRendering (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr ShapeRendering bothValues = unsafeAttribute $ Both (pure 
    { key: "shape-rendering", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "shape-rendering", value: unset' })
instance Attr everything ShapeRendering (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr ShapeRendering (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "shape-rendering", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "shape-rendering", value: unset' })
instance Attr everything ShapeRendering  Unit  where
  attr ShapeRendering _ = unsafeAttribute $ This $ pure $
    { key: "shape-rendering", value: unset' }
instance Attr everything ShapeRendering (Event.Event  Unit ) where
  attr ShapeRendering eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "shape-rendering", value: unset' }

instance Attr everything ShapeRendering (ST.ST Global.Global  Unit ) where
  attr ShapeRendering iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "shape-rendering", value: unset' }
