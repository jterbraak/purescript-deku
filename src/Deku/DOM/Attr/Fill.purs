module Deku.DOM.Attr.Fill where

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
import Deku.DOM.Elt.Set (Set_)
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
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.DOM.Elt.AnimateMotion (AnimateMotion_)
import Deku.DOM.Elt.Animate (Animate_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Fill = Fill

instance Attr Animate_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Animate_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Animate_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Animate_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Animate_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr AnimateMotion_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr AnimateMotion_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr AnimateMotion_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr AnimateMotion_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr AnimateMotion_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr AnimateTransform_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr AnimateTransform_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr AnimateTransform_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr AnimateTransform_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr AnimateTransform_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Circle_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Circle_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Circle_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Circle_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Circle_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr ClipPath_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr ClipPath_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr ClipPath_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr ClipPath_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr ClipPath_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Defs_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Defs_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Defs_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Defs_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Defs_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Ellipse_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Ellipse_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Ellipse_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Ellipse_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Ellipse_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeBlend_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeBlend_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeBlend_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeBlend_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeBlend_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeColorMatrix_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeColorMatrix_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeColorMatrix_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeColorMatrix_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeColorMatrix_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeComponentTransfer_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeComponentTransfer_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeComponentTransfer_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeComponentTransfer_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeComponentTransfer_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeComposite_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeComposite_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeComposite_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeComposite_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeComposite_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeConvolveMatrix_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeConvolveMatrix_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeConvolveMatrix_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeConvolveMatrix_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeConvolveMatrix_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeDiffuseLighting_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeDiffuseLighting_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeDiffuseLighting_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeDiffuseLighting_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeDiffuseLighting_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeDisplacementMap_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeDisplacementMap_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeDisplacementMap_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeDisplacementMap_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeDisplacementMap_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeFlood_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeFlood_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeFlood_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeFlood_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeFlood_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeGaussianBlur_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeGaussianBlur_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeGaussianBlur_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeGaussianBlur_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeGaussianBlur_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeImage_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeImage_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeImage_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeImage_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeImage_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeMerge_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeMerge_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeMerge_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeMerge_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeMerge_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeMorphology_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeMorphology_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeMorphology_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeMorphology_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeMorphology_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeOffset_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeOffset_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeOffset_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeOffset_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeOffset_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeSpecularLighting_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeSpecularLighting_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeSpecularLighting_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeSpecularLighting_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeSpecularLighting_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeTile_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeTile_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeTile_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeTile_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeTile_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr FeTurbulence_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeTurbulence_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr FeTurbulence_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr FeTurbulence_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr FeTurbulence_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Filter_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Filter_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Filter_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Filter_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Filter_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr ForeignObject_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr ForeignObject_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr ForeignObject_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr ForeignObject_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr ForeignObject_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr G_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr G_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr G_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr G_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr G_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Image_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Image_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Image_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Image_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Image_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Line_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Line_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Line_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Line_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Line_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr LinearGradient_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr LinearGradient_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr LinearGradient_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr LinearGradient_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr LinearGradient_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Marker_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Marker_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Marker_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Marker_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Marker_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Mask_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Mask_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Mask_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Mask_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Mask_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Path_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Path_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Path_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Path_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Path_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Pattern_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Pattern_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Pattern_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Pattern_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Pattern_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Polygon_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Polygon_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Polygon_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Polygon_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Polygon_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Polyline_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Polyline_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Polyline_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Polyline_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Polyline_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr RadialGradient_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr RadialGradient_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr RadialGradient_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr RadialGradient_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr RadialGradient_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Rect_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Rect_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Rect_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Rect_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Rect_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Set_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Set_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Set_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Set_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Set_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Svg_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Svg_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Svg_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Svg_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Svg_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Switch_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Switch_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Switch_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Switch_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Switch_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Symbol_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Symbol_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Symbol_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Symbol_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Symbol_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Text_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Text_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Text_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Text_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Text_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr TextPath_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr TextPath_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr TextPath_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr TextPath_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr TextPath_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Tspan_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Tspan_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Tspan_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Tspan_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Tspan_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr Use_ Fill (NonEmpty.NonEmpty Event.Event  String ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure 
    { key: "fill", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Use_ Fill (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "fill", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "fill", value: prop' value })
instance Attr Use_ Fill  String  where
  attr Fill value = unsafeAttribute $ This $ pure $
    { key: "fill", value: prop' value }
instance Attr Use_ Fill (Event.Event  String ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "fill", value: prop' value }

instance Attr Use_ Fill (ST.ST Global.Global  String ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "fill", value: prop' value }

instance Attr everything Fill (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Fill bothValues = unsafeAttribute $ Both (pure  { key: "fill", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "fill", value: unset' })
instance Attr everything Fill (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Fill (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->   { key: "fill", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "fill", value: unset' })
instance Attr everything Fill  Unit  where
  attr Fill _ = unsafeAttribute $ This $ { key: "fill", value: unset' }
instance Attr everything Fill (Event.Event  Unit ) where
  attr Fill eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "fill", value: unset' }

instance Attr everything Fill (ST.ST Global.Global  Unit ) where
  attr Fill iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "fill", value: unset' }
