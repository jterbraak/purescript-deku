module Deku.DOM.Attr.Elevation where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.FeDistantLight (FeDistantLight_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Elevation = Elevation

instance Attr FeDistantLight_ Elevation String where
  attr Elevation bothValues = unsafeAttribute $ Both
    { key: "elevation", value: prop' (fst bothValues) }
    (snd bothValues <#> \value -> { key: "elevation", value: prop' value })
  pureAttr Elevation value = unsafeAttribute $ This
    { key: "elevation", value: prop' value }
  unpureAttr Elevation eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "elevation", value: prop' value }

instance Attr everything Elevation Unit where
  attr Elevation bothValues = unsafeAttribute $ Both
    { key: "elevation", value: unset' }
    (snd bothValues <#> \_ -> { key: "elevation", value: unset' })
  pureAttr Elevation _ = unsafeAttribute $ This
    { key: "elevation", value: unset' }
  unpureAttr Elevation eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "elevation", value: unset' }
