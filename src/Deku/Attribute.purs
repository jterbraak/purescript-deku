-- | Low-level, fine-grained control of attributes. For more high-level functions, see
-- | [Deku.Attributes](https://pursuit.purescript.org/packages/purescript-deku/docs/Deku.Attributes).
-- | In this module, you'll find functions to set and unset attributes and listeners on elements.
-- | There's also the `xdata` function that allows you to construct an aribitrary data attribute.
module Deku.Attribute
  ( AttributeValue(..)
  , Attribute
  , Attribute'
  , class Attr
  , attr
  , (:=)
  , unsafeUnAttribute
  , unsafeAttribute
  , prop'
  , cb'
  , unset'
  , cb
  , Cb(..)
  , xdata
  , maybeAttr
  , (?:=)
  ) where

import Prelude

import Control.Plus (empty)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import FRP.Event as FRP
import Web.Event.Internal.Types (Event)

-- | A callback function that can be used as a value for a listener.
newtype Cb = Cb (Event -> Effect Boolean)

derive instance newtypeCb :: Newtype Cb _
instance eqCb :: Eq Cb where
  eq _ _ = false

instance ordCb :: Ord Cb where
  compare _ _ = LT

instance showCb :: Show Cb where
  show _ = "{callback}"

-- | Construct a `cb`. This is an alias for the newtype constructor.
cb :: (Event -> Effect Unit) -> Cb
cb = Cb <<< ((map <<< map) (const true))

-- | Construct a property attribute value.
-- | In general, this function is for internal use only. In practice, you'll use
-- | the `:=` family of operators and helpers like `style` and `klass` instead.
prop' :: String -> AttributeValue
prop' = Prop'

-- | Construct a callback for a listener.
-- | In general, this function is for internal use only. In practice, you'll use
-- | the `:=` family of operators and helpers like `click` and `keyUp` instead.
cb' :: Cb -> AttributeValue
cb' = Cb'

-- | Unset an attribute. You should not use this directly but rather
-- | you can set a value to `unit` to unset it, which calls
-- | `unset'` under the hood.
-- |
-- | For example, to set an ID and then unset it two seconds later,
-- | you could do the following:
-- |
-- | ```purescript
-- | id_ "foo" <|> delay 2000 (D.Id !:= unit)
-- | ```
unset' :: AttributeValue
unset' = Unset'

-- | Low-level constructor for attributes and listeners, including their unsetting.
-- | In general, these constructors are for internal use only. In practice, you'll use
-- | the `:=` family of operators and helpers like `style` and `klass` instead.
data AttributeValue = Prop' String | Cb' Cb | Unset'

type Attribute' =
  { key :: String
  , value :: AttributeValue
  }

-- | Low level representation of key-value pairs for attributes and listeners.
-- | In general, this type is for internal use only. In practice, you'll use
-- | the `:=` family of operators and helpers like `style` and `klass` instead.
newtype Attribute (e :: Type) = Attribute
  (FRP.Event Unit -> FRP.Event Attribute')

-- | For internal use only, exported to be used by other modules. Ignore this.
unsafeUnAttribute
  :: forall e
   . Attribute e
  -> (FRP.Event Unit -> FRP.Event Attribute')
unsafeUnAttribute (Attribute a) = a

-- | For internal use only, exported to be used by other modules. Ignore this.
unsafeAttribute
  :: forall e
   . (FRP.Event Unit -> FRP.Event Attribute')
  -> Attribute e
unsafeAttribute = Attribute

-- | Guarantees type-safe creation of attribute `a` with type `b` for element `e`.
-- | Guards against elements having incorrect attributes set, for example prohibiting
-- | the setting of `style` as a `Boolean`, etc.
class Attr e a b where
  -- | Construct a type-safe attribute or listener. More commonly used in its alias `:=`,
  -- | aka `D.Style := "color: red;"` is a valid attribute or listener for any element.
  attr :: a -> b -> Attribute e

-- | Construct a [data attribute](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes).
xdata :: forall e. String -> String -> Attribute e
xdata k v = unsafeAttribute (voidRight { key: "data-" <> k, value: Prop' v })

-- | A version of `attr` that sets an attribute or listener only if the value is `Just`.
-- | More commonly used in its alias `?:=`.
maybeAttr
  :: forall a b e
   . Attr e a b
  => a
  -> Maybe b
  -> Attribute e
maybeAttr a (Just b) = a := b
maybeAttr _ Nothing = Attribute (const empty)

infix 5 maybeAttr as ?:=
infix 5 attr as :=