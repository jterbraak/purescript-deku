module Deku.DOM.Attr.EnableBackground where


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

data EnableBackground = EnableBackground

instance Attr FeBlend_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeBlend_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeColorMatrix_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeColorMatrix_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeComponentTransfer_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeComponentTransfer_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeComposite_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeComposite_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeConvolveMatrix_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeConvolveMatrix_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeDiffuseLighting_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeDiffuseLighting_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeDisplacementMap_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeDisplacementMap_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeFlood_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeFlood_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeGaussianBlur_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeGaussianBlur_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeImage_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeImage_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeMerge_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeMerge_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeMorphology_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeMorphology_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeOffset_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeOffset_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeSpecularLighting_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeSpecularLighting_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeTile_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeTile_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr FeTurbulence_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr FeTurbulence_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr Filter_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr Filter_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr Image_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr Image_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr Switch_ EnableBackground  String  where
  attr EnableBackground value = unsafeAttribute (  
    { key: "enable-background", value: prop' value  } <$ _)
instance Attr Switch_ EnableBackground (Event.Event  String ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \value -> { key: "enable-background", value: prop' value }


instance Attr everything EnableBackground  Unit  where
  attr EnableBackground _ = unsafeAttribute (  
    { key: "enable-background", value: unset'  } <$ _)
instance Attr everything EnableBackground (Event.Event  Unit ) where
  attr EnableBackground eventValue = unsafeAttribute \_ -> eventValue
    <#> \_ -> { key: "enable-background", value: unset' }
