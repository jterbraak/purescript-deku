module Deku.DOM.Attr.Width where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.Mask (Mask_)
import Deku.DOM.Elt.Image (Image_)
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
import Deku.DOM.Elt.FeDropShadow (FeDropShadow_)
import Deku.DOM.Elt.FeDisplacementMap (FeDisplacementMap_)
import Deku.DOM.Elt.FeDiffuseLighting (FeDiffuseLighting_)
import Deku.DOM.Elt.FeConvolveMatrix (FeConvolveMatrix_)
import Deku.DOM.Elt.FeComposite (FeComposite_)
import Deku.DOM.Elt.FeComponentTransfer (FeComponentTransfer_)
import Deku.DOM.Elt.FeColorMatrix (FeColorMatrix_)
import Deku.DOM.Elt.FeBlend (FeBlend_)
import Deku.DOM.Elt.Canvas (Canvas_)
import Deku.DOM.Elt.Embed (Embed_)
import Deku.DOM.Elt.Iframe (Iframe_)
import Deku.DOM.Elt.Img (Img_)
import Deku.DOM.Elt.Input (Input_)
import Deku.DOM.Elt.Object (Object_)
import Deku.DOM.Elt.Video (Video_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Width = Width

instance Attr Canvas_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Canvas_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Canvas_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Canvas_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Canvas_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Embed_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Embed_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Embed_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Embed_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Embed_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Iframe_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Iframe_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Iframe_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Iframe_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Iframe_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Img_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Img_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Img_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Img_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Img_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Input_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Input_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Input_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Input_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Input_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Object_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Object_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Object_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Object_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Object_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Video_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Video_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Video_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Video_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Video_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeBlend_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeBlend_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeBlend_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeBlend_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeBlend_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeColorMatrix_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeColorMatrix_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeColorMatrix_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeColorMatrix_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeColorMatrix_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeComponentTransfer_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeComponentTransfer_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeComponentTransfer_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeComponentTransfer_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeComponentTransfer_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeComposite_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeComposite_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeComposite_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeComposite_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeComposite_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeConvolveMatrix_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeConvolveMatrix_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeConvolveMatrix_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeConvolveMatrix_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeConvolveMatrix_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeDiffuseLighting_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDiffuseLighting_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDiffuseLighting_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeDiffuseLighting_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeDiffuseLighting_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeDisplacementMap_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDisplacementMap_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDisplacementMap_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeDisplacementMap_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeDisplacementMap_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeDropShadow_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDropShadow_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeDropShadow_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeDropShadow_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeDropShadow_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeFlood_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeFlood_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeFlood_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeFlood_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeFlood_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeGaussianBlur_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeGaussianBlur_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeGaussianBlur_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeGaussianBlur_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeGaussianBlur_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeImage_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeImage_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeImage_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeImage_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeImage_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeMerge_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeMerge_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeMerge_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeMerge_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeMerge_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeMorphology_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeMorphology_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeMorphology_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeMorphology_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeMorphology_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeOffset_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeOffset_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeOffset_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeOffset_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeOffset_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeSpecularLighting_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeSpecularLighting_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeSpecularLighting_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeSpecularLighting_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeSpecularLighting_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeTile_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeTile_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeTile_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeTile_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeTile_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr FeTurbulence_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeTurbulence_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr FeTurbulence_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr FeTurbulence_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr FeTurbulence_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Filter_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Filter_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Filter_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Filter_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Filter_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr ForeignObject_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr ForeignObject_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr ForeignObject_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr ForeignObject_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr ForeignObject_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Image_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Image_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Image_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Image_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Image_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Mask_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Mask_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Mask_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Mask_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Mask_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Pattern_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Pattern_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Pattern_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Pattern_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Pattern_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Rect_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Rect_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Rect_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Rect_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Rect_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Svg_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Svg_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Svg_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Svg_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Svg_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Symbol_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Symbol_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Symbol_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Symbol_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Symbol_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr Use_ Width (NonEmpty.NonEmpty Event.Event  String ) where
  attr Width bothValues = unsafeAttribute $ Both (pure 
    { key: "width", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Use_ Width (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "width", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "width", value: prop' value })
instance Attr Use_ Width  String  where
  attr Width value = unsafeAttribute $ This $ pure $
    { key: "width", value: prop' value }
instance Attr Use_ Width (Event.Event  String ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "width", value: prop' value }

instance Attr Use_ Width (ST.ST Global.Global  String ) where
  attr Width iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "width", value: prop' value }

instance Attr everything Width (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Width bothValues = unsafeAttribute $ Both (pure  { key: "width", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "width", value: unset' })
instance Attr everything Width (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Width (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->   { key: "width", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "width", value: unset' })
instance Attr everything Width  Unit  where
  attr Width _ = unsafeAttribute $ This $ { key: "width", value: unset' }
instance Attr everything Width (Event.Event  Unit ) where
  attr Width eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "width", value: unset' }

instance Attr everything Width (ST.ST Global.Global  Unit ) where
  attr Width iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "width", value: unset' }
