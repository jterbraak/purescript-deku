module Deku.DOM.Attr.TextRendering where

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
import Deku.Attribute (class Attr, prop', unsafeAttribute)

data TextRendering = TextRendering

instance Attr FeBlend_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeColorMatrix_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeComponentTransfer_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeComposite_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeConvolveMatrix_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeDiffuseLighting_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeDisplacementMap_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeFlood_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeGaussianBlur_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeImage_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeMerge_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeMorphology_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeOffset_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeSpecularLighting_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeTile_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr FeTurbulence_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr Filter_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr Image_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }

instance Attr Switch_ TextRendering String where
  attr TextRendering value = unsafeAttribute
    { key: "text-rendering", value: prop' value }