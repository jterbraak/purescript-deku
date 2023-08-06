module Deku.DOM.Attr.FontVariant where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.Text (Text_)
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

data FontVariant = FontVariant

instance Attr FeBlend_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeBlend_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeColorMatrix_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeColorMatrix_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeComponentTransfer_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeComponentTransfer_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeComposite_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeComposite_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeConvolveMatrix_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeConvolveMatrix_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeDiffuseLighting_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeDiffuseLighting_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeDisplacementMap_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeDisplacementMap_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeFlood_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeFlood_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeGaussianBlur_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeGaussianBlur_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeImage_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeImage_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeMerge_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeMerge_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeMorphology_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeMorphology_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeOffset_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeOffset_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeSpecularLighting_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeSpecularLighting_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeTile_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeTile_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr FeTurbulence_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr FeTurbulence_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr Filter_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr Filter_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr Image_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr Image_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr Switch_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr Switch_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr Text_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr Text_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr Tspan_ FontVariant  String  where
  attr FontVariant value = unsafeAttribute $ Left $  
    { key: "font-variant", value: prop' value }
instance Attr Tspan_ FontVariant (Event.Event  String ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "font-variant", value: prop' value }


instance Attr everything FontVariant  Unit  where
  attr FontVariant _ = unsafeAttribute $ Left $  
    { key: "font-variant", value: unset' }
instance Attr everything FontVariant (Event.Event  Unit ) where
  attr FontVariant eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "font-variant", value: unset' }
