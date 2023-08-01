module Deku.DOM.Attr.Seed where

import Prelude
import Data.These (These(..))
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeTurbulence (FeTurbulence_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Seed = Seed

instance Attr FeTurbulence_ Seed String where
  attr Seed bothValues = unsafeAttribute $ Both
    { key: "seed", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "seed", value: prop' value })
  pureAttr Seed value = unsafeAttribute $ This
    { key: "seed", value: prop' value }
  unpureAttr Seed eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "seed", value: prop' value }

instance Attr everything Seed Unit where
  attr Seed bothValues = unsafeAttribute $ Both { key: "seed", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "seed", value: unset' })
  pureAttr Seed _ = unsafeAttribute $ This { key: "seed", value: unset' }
  unpureAttr Seed eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "seed", value: unset' }
