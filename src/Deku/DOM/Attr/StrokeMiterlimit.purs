module Deku.DOM.Attr.StrokeMiterlimit where


import Prelude

import FRP.Event as Event
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

data StrokeMiterlimit = StrokeMiterlimit

instance Attr Circle_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Circle_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr ClipPath_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr ClipPath_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Defs_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Defs_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Ellipse_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Ellipse_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeBlend_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeBlend_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeColorMatrix_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeColorMatrix_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeComponentTransfer_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeComponentTransfer_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeComposite_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeComposite_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeConvolveMatrix_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeConvolveMatrix_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeDiffuseLighting_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeDiffuseLighting_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeDisplacementMap_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeDisplacementMap_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeFlood_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeFlood_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeGaussianBlur_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeGaussianBlur_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeImage_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeImage_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeMerge_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeMerge_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeMorphology_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeMorphology_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeOffset_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeOffset_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeSpecularLighting_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeSpecularLighting_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeTile_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeTile_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr FeTurbulence_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr FeTurbulence_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Filter_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Filter_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr ForeignObject_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr ForeignObject_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr G_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr G_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Image_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Image_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Line_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Line_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr LinearGradient_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr LinearGradient_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Marker_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Marker_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Mask_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Mask_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Path_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Path_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Pattern_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Pattern_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Polygon_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Polygon_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Polyline_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Polyline_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr RadialGradient_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr RadialGradient_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Rect_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Rect_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Svg_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Svg_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Switch_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Switch_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Symbol_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Symbol_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Text_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Text_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr TextPath_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr TextPath_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Tspan_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Tspan_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr Use_ StrokeMiterlimit  String  where
  attr StrokeMiterlimit value = unsafeAttribute (  
    { key: "stroke-miterlimit", value: prop' value  } <$ _)
instance Attr Use_ StrokeMiterlimit (Event.Event  String ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "stroke-miterlimit", value: prop' value }


instance Attr everything StrokeMiterlimit  Unit  where
  attr StrokeMiterlimit _ = unsafeAttribute (  
    { key: "stroke-miterlimit", value: unset'  } <$ _)
instance Attr everything StrokeMiterlimit (Event.Event  Unit ) where
  attr StrokeMiterlimit eventValue = unsafeAttribute \_ -> eventValue
    <#> \_ -> { key: "stroke-miterlimit", value: unset' }
