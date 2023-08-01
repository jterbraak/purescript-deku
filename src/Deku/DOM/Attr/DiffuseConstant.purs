module Deku.DOM.Attr.DiffuseConstant where

import Prelude
import Data.These (These(..))
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeDiffuseLighting (FeDiffuseLighting_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data DiffuseConstant = DiffuseConstant

instance Attr FeDiffuseLighting_ DiffuseConstant String where
  attr DiffuseConstant bothValues = unsafeAttribute $ Both
    { key: "diffuseConstant", value: prop' (NonEmpty.head bothValues) }
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "diffuseConstant", value: prop' value }
    )
  pureAttr DiffuseConstant value = unsafeAttribute $ This
    { key: "diffuseConstant", value: prop' value }
  unpureAttr DiffuseConstant eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "diffuseConstant", value: prop' value }

instance Attr everything DiffuseConstant Unit where
  attr DiffuseConstant bothValues = unsafeAttribute $ Both
    { key: "diffuseConstant", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "diffuseConstant", value: unset' })
  pureAttr DiffuseConstant _ = unsafeAttribute $ This
    { key: "diffuseConstant", value: unset' }
  unpureAttr DiffuseConstant eventValue = unsafeAttribute $ That $ eventValue
    <#> \_ -> { key: "diffuseConstant", value: unset' }
