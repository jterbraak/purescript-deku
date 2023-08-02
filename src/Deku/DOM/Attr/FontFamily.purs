module Deku.DOM.Attr.FontFamily where

import Prelude
import Data.These (These(..))
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

data FontFamily = FontFamily

instance Attr FeBlend_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeBlend_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeBlend_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeColorMatrix_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeColorMatrix_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeColorMatrix_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeComponentTransfer_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeComponentTransfer_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeComponentTransfer_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeComposite_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeComposite_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeComposite_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeConvolveMatrix_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeConvolveMatrix_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeConvolveMatrix_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeDiffuseLighting_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeDiffuseLighting_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeDiffuseLighting_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeDisplacementMap_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeDisplacementMap_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeDisplacementMap_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeFlood_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeFlood_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeFlood_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeGaussianBlur_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeGaussianBlur_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeGaussianBlur_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeImage_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeImage_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeImage_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeMerge_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeMerge_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeMerge_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeMorphology_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeMorphology_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeMorphology_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeOffset_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeOffset_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeOffset_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeSpecularLighting_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeSpecularLighting_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeSpecularLighting_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeTile_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeTile_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeTile_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr FeTurbulence_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr FeTurbulence_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr FeTurbulence_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr Filter_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr Filter_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr Filter_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr Image_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr Image_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr Image_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr Switch_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr Switch_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr Switch_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr Text_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr Text_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr Text_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr Tspan_ FontFamily (NonEmpty.NonEmpty Event.Event  String ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "font-family", value: prop' value })
instance Attr Tspan_ FontFamily  String  where
  attr FontFamily value = unsafeAttribute $ This
    { key: "font-family", value: prop' value }
instance Attr Tspan_ FontFamily (Event.Event  String ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "font-family", value: prop' value }

instance Attr everything FontFamily (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr FontFamily bothValues = unsafeAttribute $ Both
    { key: "font-family", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "font-family", value: unset' })
instance Attr everything FontFamily  Unit  where
  attr FontFamily _ = unsafeAttribute $ This
    { key: "font-family", value: unset' }
instance Attr everything FontFamily (Event.Event  Unit ) where
  attr FontFamily eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "font-family", value: unset' }
