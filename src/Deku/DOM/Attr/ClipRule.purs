module Deku.DOM.Attr.ClipRule where

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

data ClipRule = ClipRule

instance Attr Circle_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Circle_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Circle_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr ClipPath_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr ClipPath_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr ClipPath_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Defs_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Defs_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Defs_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Ellipse_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Ellipse_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Ellipse_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeBlend_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeBlend_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeBlend_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeColorMatrix_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeColorMatrix_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeColorMatrix_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeComponentTransfer_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeComponentTransfer_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeComponentTransfer_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeComposite_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeComposite_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeComposite_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeConvolveMatrix_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeConvolveMatrix_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeConvolveMatrix_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeDiffuseLighting_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeDiffuseLighting_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeDiffuseLighting_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeDisplacementMap_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeDisplacementMap_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeDisplacementMap_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeFlood_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeFlood_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeFlood_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeGaussianBlur_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeGaussianBlur_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeGaussianBlur_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeImage_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeImage_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeImage_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeMerge_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeMerge_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeMerge_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeMorphology_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeMorphology_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeMorphology_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeOffset_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeOffset_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeOffset_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeSpecularLighting_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeSpecularLighting_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeSpecularLighting_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeTile_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeTile_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeTile_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr FeTurbulence_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr FeTurbulence_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr FeTurbulence_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Filter_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Filter_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Filter_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr ForeignObject_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr ForeignObject_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr ForeignObject_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr G_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr G_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr G_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Image_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Image_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Image_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Line_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Line_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Line_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr LinearGradient_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr LinearGradient_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr LinearGradient_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Marker_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Marker_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Marker_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Mask_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Mask_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Mask_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Path_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Path_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Path_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Pattern_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Pattern_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Pattern_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Polygon_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Polygon_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Polygon_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Polyline_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Polyline_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Polyline_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr RadialGradient_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr RadialGradient_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr RadialGradient_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Rect_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Rect_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Rect_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Svg_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Svg_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Svg_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Switch_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Switch_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Switch_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Symbol_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Symbol_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Symbol_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Text_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Text_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Text_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr TextPath_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr TextPath_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr TextPath_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Tspan_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Tspan_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Tspan_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr Use_ ClipRule (NonEmpty.NonEmpty Event.Event  String ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "clip-rule", value: prop' value })
instance Attr Use_ ClipRule  String  where
  attr ClipRule value = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: prop' value }
instance Attr Use_ ClipRule (Event.Event  String ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "clip-rule", value: prop' value }

instance Attr everything ClipRule (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr ClipRule bothValues = unsafeAttribute $ Both (pure 
    { key: "clip-rule", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "clip-rule", value: unset' })
instance Attr everything ClipRule  Unit  where
  attr ClipRule _ = unsafeAttribute $ This $ pure $
    { key: "clip-rule", value: unset' }
instance Attr everything ClipRule (Event.Event  Unit ) where
  attr ClipRule eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "clip-rule", value: unset' }
