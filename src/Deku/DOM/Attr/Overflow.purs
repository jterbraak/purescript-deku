module Deku.DOM.Attr.Overflow where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.Switch (Switch_)
import Deku.DOM.Elt.Image (Image_)
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
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Overflow = Overflow

instance Attr FeBlend_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeBlend_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeColorMatrix_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeColorMatrix_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeComponentTransfer_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeComponentTransfer_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeComposite_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeComposite_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeConvolveMatrix_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeConvolveMatrix_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeDiffuseLighting_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeDiffuseLighting_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeDisplacementMap_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeDisplacementMap_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeFlood_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeFlood_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeGaussianBlur_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeGaussianBlur_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeImage_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeImage_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeMerge_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeMerge_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeMorphology_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeMorphology_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeOffset_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeOffset_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeSpecularLighting_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeSpecularLighting_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeTile_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeTile_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr FeTurbulence_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr FeTurbulence_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr Filter_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr Filter_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr Image_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr Image_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr Switch_ Overflow  String  where
  attr Overflow value = unsafeAttribute (  
    { key: "overflow", value: prop' value  } <$ _)
instance Attr Switch_ Overflow (Event.Event  String ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "overflow", value: prop' value }


instance Attr everything Overflow  Unit  where
  attr Overflow _ = unsafeAttribute (  
    { key: "overflow", value: unset'  } <$ _)
instance Attr everything Overflow (Event.Event  Unit ) where
  attr Overflow eventValue = unsafeAttribute \_ -> eventValue <#> \_ ->
    { key: "overflow", value: unset' }
