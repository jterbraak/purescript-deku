module Deku.DOM.Attr.Mode where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.FeBlend (FeBlend_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Mode = Mode

instance Attr FeBlend_ Mode String where
  attr Mode bothValues = unsafeAttribute $ Both
    { key: "mode", value: prop' (fst bothValues) }
    (snd bothValues <#> \value -> { key: "mode", value: prop' value })
  pureAttr Mode value = unsafeAttribute $ This
    { key: "mode", value: prop' value }
  unpureAttr Mode eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "mode", value: prop' value }

instance Attr everything Mode Unit where
  attr Mode bothValues = unsafeAttribute $ Both { key: "mode", value: unset' }
    (snd bothValues <#> \_ -> { key: "mode", value: unset' })
  pureAttr Mode _ = unsafeAttribute $ This { key: "mode", value: unset' }
  unpureAttr Mode eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "mode", value: unset' }
