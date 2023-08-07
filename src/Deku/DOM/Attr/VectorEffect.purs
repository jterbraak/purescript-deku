module Deku.DOM.Attr.VectorEffect where


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

data VectorEffect = VectorEffect

instance Attr Circle_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Circle_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr ClipPath_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr ClipPath_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Defs_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Defs_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Ellipse_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Ellipse_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeBlend_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeBlend_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeColorMatrix_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeColorMatrix_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeComponentTransfer_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeComponentTransfer_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeComposite_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeComposite_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeConvolveMatrix_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeConvolveMatrix_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeDiffuseLighting_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeDiffuseLighting_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeDisplacementMap_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeDisplacementMap_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeFlood_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeFlood_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeGaussianBlur_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeGaussianBlur_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeImage_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeImage_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeMerge_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeMerge_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeMorphology_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeMorphology_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeOffset_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeOffset_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeSpecularLighting_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeSpecularLighting_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeTile_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeTile_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr FeTurbulence_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr FeTurbulence_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Filter_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Filter_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr ForeignObject_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr ForeignObject_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr G_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr G_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Image_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Image_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Line_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Line_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr LinearGradient_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr LinearGradient_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Marker_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Marker_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Mask_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Mask_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Path_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Path_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Pattern_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Pattern_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Polygon_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Polygon_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Polyline_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Polyline_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr RadialGradient_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr RadialGradient_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Rect_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Rect_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Svg_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Svg_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Switch_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Switch_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Symbol_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Symbol_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Text_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Text_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr TextPath_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr TextPath_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Tspan_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Tspan_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr Use_ VectorEffect  String  where
  attr VectorEffect value = unsafeAttribute (  
    { key: "vector-effect", value: prop' value  } <$ _)
instance Attr Use_ VectorEffect (Event.Event  String ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "vector-effect", value: prop' value }


instance Attr everything VectorEffect  Unit  where
  attr VectorEffect _ = unsafeAttribute (  
    { key: "vector-effect", value: unset'  } <$ _)
instance Attr everything VectorEffect (Event.Event  Unit ) where
  attr VectorEffect eventValue = unsafeAttribute \_ -> eventValue <#>
    \_ -> { key: "vector-effect", value: unset' }
