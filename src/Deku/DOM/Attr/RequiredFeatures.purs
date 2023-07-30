module Deku.DOM.Attr.RequiredFeatures where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Switch (Switch_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Mask (Mask_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Discard (Discard_)
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data RequiredFeatures = RequiredFeatures

instance Attr AnimateTransform_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Discard_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Ellipse_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr ForeignObject_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr G_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Image_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Line_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Marker_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Mask_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Path_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Pattern_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Polygon_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Polyline_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Rect_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Svg_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Switch_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Text_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr TextPath_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Tspan_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr Use_ RequiredFeatures String where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: prop' (fst bothValues) }
    ( snd bothValues <#> \value ->
        { key: "requiredFeatures", value: prop' value }
    )
  pureAttr RequiredFeatures value = unsafeAttribute $ This
    { key: "requiredFeatures", value: prop' value }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "requiredFeatures", value: prop' value }

instance Attr everything RequiredFeatures Unit where
  attr RequiredFeatures bothValues = unsafeAttribute $ Both
    { key: "requiredFeatures", value: unset' }
    (snd bothValues <#> \_ -> { key: "requiredFeatures", value: unset' })
  pureAttr RequiredFeatures _ = unsafeAttribute $ This
    { key: "requiredFeatures", value: unset' }
  unpureAttr RequiredFeatures eventValue = unsafeAttribute $ That $ eventValue
    <#> \_ -> { key: "requiredFeatures", value: unset' }
