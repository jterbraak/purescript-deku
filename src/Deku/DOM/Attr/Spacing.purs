module Deku.DOM.Attr.Spacing where

import Prelude
import Data.These (These(..))
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Spacing = Spacing

instance Attr TextPath_ Spacing String where
  attr Spacing bothValues = unsafeAttribute $ Both
    { key: "spacing", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spacing", value: prop' value })
  pureAttr Spacing value = unsafeAttribute $ This
    { key: "spacing", value: prop' value }
  unpureAttr Spacing eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spacing", value: prop' value }

instance Attr everything Spacing Unit where
  attr Spacing bothValues = unsafeAttribute $ Both
    { key: "spacing", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "spacing", value: unset' })
  pureAttr Spacing _ = unsafeAttribute $ This { key: "spacing", value: unset' }
  unpureAttr Spacing eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "spacing", value: unset' }
