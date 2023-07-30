module Deku.DOM.Attr.KeyPoints where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.AnimateMotion (AnimateMotion_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data KeyPoints = KeyPoints

instance Attr AnimateMotion_ KeyPoints String where
  attr KeyPoints bothValues = unsafeAttribute $ Both
    { key: "keyPoints", value: prop' (fst bothValues) }
    (snd bothValues <#> \value -> { key: "keyPoints", value: prop' value })
  pureAttr KeyPoints value = unsafeAttribute $ This
    { key: "keyPoints", value: prop' value }
  unpureAttr KeyPoints eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "keyPoints", value: prop' value }

instance Attr everything KeyPoints Unit where
  attr KeyPoints bothValues = unsafeAttribute $ Both
    { key: "keyPoints", value: unset' }
    (snd bothValues <#> \_ -> { key: "keyPoints", value: unset' })
  pureAttr KeyPoints _ = unsafeAttribute $ This
    { key: "keyPoints", value: unset' }
  unpureAttr KeyPoints eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "keyPoints", value: unset' }
