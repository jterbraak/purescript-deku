module Deku.DOM.Attr.Type where

import Prelude
import Data.These (These(..))
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeTurbulence (FeTurbulence_)
import Deku.DOM.Elt.FeFuncR (FeFuncR_)
import Deku.DOM.Elt.FeFuncG (FeFuncG_)
import Deku.DOM.Elt.FeFuncB (FeFuncB_)
import Deku.DOM.Elt.FeFuncA (FeFuncA_)
import Deku.DOM.Elt.FeColorMatrix (FeColorMatrix_)
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Type = Type

instance Attr AnimateTransform_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeColorMatrix_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeFuncA_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeFuncB_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeFuncG_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeFuncR_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr FeTurbulence_ Type String where
  attr Type bothValues = unsafeAttribute $ Both
    { key: "type", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "type", value: prop' value })
  pureAttr Type value = unsafeAttribute $ This
    { key: "type", value: prop' value }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "type", value: prop' value }

instance Attr everything Type Unit where
  attr Type bothValues = unsafeAttribute $ Both { key: "type", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "type", value: unset' })
  pureAttr Type _ = unsafeAttribute $ This { key: "type", value: unset' }
  unpureAttr Type eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "type", value: unset' }
