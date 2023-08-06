module Deku.DOM.Attr.ColorProfile where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

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

data ColorProfile = ColorProfile

instance Attr FeBlend_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeBlend_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeColorMatrix_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeColorMatrix_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeComponentTransfer_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeComponentTransfer_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeComposite_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeComposite_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeConvolveMatrix_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeConvolveMatrix_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeDiffuseLighting_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeDiffuseLighting_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeDisplacementMap_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeDisplacementMap_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeFlood_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeFlood_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeGaussianBlur_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeGaussianBlur_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeImage_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeImage_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeMerge_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeMerge_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeMorphology_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeMorphology_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeOffset_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeOffset_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeSpecularLighting_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeSpecularLighting_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeTile_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeTile_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr FeTurbulence_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr FeTurbulence_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr Filter_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr Filter_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr Image_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr Image_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr Switch_ ColorProfile  String  where
  attr ColorProfile value = unsafeAttribute $ Left $  
    { key: "color-profile", value: prop' value }
instance Attr Switch_ ColorProfile (Event.Event  String ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "color-profile", value: prop' value }


instance Attr everything ColorProfile  Unit  where
  attr ColorProfile _ = unsafeAttribute $ Left $  
    { key: "color-profile", value: unset' }
instance Attr everything ColorProfile (Event.Event  Unit ) where
  attr ColorProfile eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "color-profile", value: unset' }
