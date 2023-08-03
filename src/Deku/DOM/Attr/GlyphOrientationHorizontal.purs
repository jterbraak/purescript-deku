module Deku.DOM.Attr.GlyphOrientationHorizontal where

import Prelude
import Data.These (These(..))
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

data GlyphOrientationHorizontal = GlyphOrientationHorizontal

instance Attr FeBlend_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeBlend_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeBlend_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeColorMatrix_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeColorMatrix_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeColorMatrix_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeComponentTransfer_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeComponentTransfer_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeComponentTransfer_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeComposite_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeComposite_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeComposite_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeConvolveMatrix_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeConvolveMatrix_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeConvolveMatrix_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeDiffuseLighting_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeDiffuseLighting_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeDiffuseLighting_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeDisplacementMap_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeDisplacementMap_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeDisplacementMap_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeFlood_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeFlood_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeFlood_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeGaussianBlur_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeGaussianBlur_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeGaussianBlur_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeImage_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeImage_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeImage_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeMerge_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeMerge_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeMerge_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeMorphology_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeMorphology_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeMorphology_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeOffset_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeOffset_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeOffset_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeSpecularLighting_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeSpecularLighting_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeSpecularLighting_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeTile_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeTile_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeTile_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr FeTurbulence_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr FeTurbulence_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr FeTurbulence_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr Filter_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr Filter_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr Filter_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr Image_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr Image_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr Image_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr Switch_ GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  String ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "glyph-orientation-horizontal", value: prop' value }
    )
instance Attr Switch_ GlyphOrientationHorizontal  String  where
  attr GlyphOrientationHorizontal value = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: prop' value }
instance Attr Switch_ GlyphOrientationHorizontal (Event.Event  String ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \value ->
      { key: "glyph-orientation-horizontal", value: prop' value }

instance Attr everything GlyphOrientationHorizontal (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr GlyphOrientationHorizontal bothValues = unsafeAttribute $ Both (pure 
    { key: "glyph-orientation-horizontal", value: unset' })
    ( NonEmpty.tail bothValues <#> \_ ->
        { key: "glyph-orientation-horizontal", value: unset' }
    )
instance Attr everything GlyphOrientationHorizontal  Unit  where
  attr GlyphOrientationHorizontal _ = unsafeAttribute $ This $ pure $
    { key: "glyph-orientation-horizontal", value: unset' }
instance Attr everything GlyphOrientationHorizontal (Event.Event  Unit ) where
  attr GlyphOrientationHorizontal eventValue = unsafeAttribute $ That $
    eventValue <#> \_ -> { key: "glyph-orientation-horizontal", value: unset' }
