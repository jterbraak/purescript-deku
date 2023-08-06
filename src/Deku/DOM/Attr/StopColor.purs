module Deku.DOM.Attr.StopColor where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Switch (Switch_)
import Deku.DOM.Elt.Stop (Stop_)
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

data StopColor = StopColor

instance Attr FeBlend_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeBlend_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeColorMatrix_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeColorMatrix_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeComponentTransfer_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeComponentTransfer_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeComposite_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeComposite_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeConvolveMatrix_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeConvolveMatrix_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeDiffuseLighting_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeDiffuseLighting_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeDisplacementMap_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeDisplacementMap_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeFlood_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeFlood_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeGaussianBlur_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeGaussianBlur_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeImage_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeImage_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeMerge_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeMerge_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeMorphology_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeMorphology_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeOffset_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeOffset_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeSpecularLighting_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeSpecularLighting_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeTile_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeTile_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr FeTurbulence_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr FeTurbulence_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr Filter_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr Filter_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr Image_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr Image_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr Stop_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr Stop_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr Switch_ StopColor  String  where
  attr StopColor value = unsafeAttribute $ Left $  
    { key: "stop-color", value: prop' value }
instance Attr Switch_ StopColor (Event.Event  String ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "stop-color", value: prop' value }


instance Attr everything StopColor  Unit  where
  attr StopColor _ = unsafeAttribute $ Left $  
    { key: "stop-color", value: unset' }
instance Attr everything StopColor (Event.Event  Unit ) where
  attr StopColor eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "stop-color", value: unset' }
