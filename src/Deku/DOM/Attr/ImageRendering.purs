module Deku.DOM.Attr.ImageRendering where

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

data ImageRendering = ImageRendering

instance Attr FeBlend_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeBlend_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeColorMatrix_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeColorMatrix_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeComponentTransfer_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeComponentTransfer_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeComposite_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeComposite_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeConvolveMatrix_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeConvolveMatrix_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeDiffuseLighting_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeDiffuseLighting_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeDisplacementMap_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeDisplacementMap_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeFlood_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeFlood_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeGaussianBlur_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeGaussianBlur_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeImage_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeImage_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeMerge_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeMerge_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeMorphology_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeMorphology_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeOffset_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeOffset_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeSpecularLighting_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeSpecularLighting_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeTile_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeTile_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr FeTurbulence_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr FeTurbulence_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr Filter_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr Filter_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr Image_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr Image_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr Switch_ ImageRendering  String  where
  attr ImageRendering value = unsafeAttribute $ Left $  
    { key: "image-rendering", value: prop' value }
instance Attr Switch_ ImageRendering (Event.Event  String ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "image-rendering", value: prop' value }


instance Attr everything ImageRendering  Unit  where
  attr ImageRendering _ = unsafeAttribute $ Left $  
    { key: "image-rendering", value: unset' }
instance Attr everything ImageRendering (Event.Event  Unit ) where
  attr ImageRendering eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "image-rendering", value: unset' }
