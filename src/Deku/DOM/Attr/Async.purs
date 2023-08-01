module Deku.DOM.Attr.Async where

import Prelude
import Data.These (These(..))
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Script (Script_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Async = Async

instance Attr Script_ Async String where
  attr Async bothValues = unsafeAttribute $ Both
    { key: "async", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "async", value: prop' value })
  pureAttr Async value = unsafeAttribute $ This
    { key: "async", value: prop' value }
  unpureAttr Async eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "async", value: prop' value }

instance Attr everything Async Unit where
  attr Async bothValues = unsafeAttribute $ Both { key: "async", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "async", value: unset' })
  pureAttr Async _ = unsafeAttribute $ This { key: "async", value: unset' }
  unpureAttr Async eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "async", value: unset' }
