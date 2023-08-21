-- | Low-level, fine-grained control of attributes. For more high-level functions, see
-- | [Deku.Attributes](https://pursuit.purescript.org/packages/purescript-deku/docs/Deku.Attributes).
-- | In this module, you'll find functions to set and unset attributes and listeners on elements.
-- | There's also the `xdata` function that allows you to construct an aribitrary data attribute.
module Deku.Attribute
  ( AttributeValue(..)
  , AnAttribute
  , AnAttribute'
  , Attribute(..)
  , unsafeUnAttribute
  , unsafeAttribute
  , prop'
  , cb'
  , unset'
  , cb
  , Cb(..)
  , xdata
  ) where

import Prelude

import Data.Functor.Contravariant (class Contravariant)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Safe.Coerce (coerce)
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

type AnAttribute' =
  { key :: String
  , value :: AttributeValue
  }

-- | Low level representation of key-value pairs for attributes and listeners.
-- | In general, this type is for internal use only. In practice, you'll use
-- | the `:=` family of operators and helpers like `style` and `klass` instead.
newtype AnAttribute :: forall k . k -> Type
newtype AnAttribute i = AnAttribute AnAttribute'

-- | For internal use only, exported to be used by other modules. Ignore this.
unsafeUnAttribute
  :: forall e. AnAttribute e -> AnAttribute'
unsafeUnAttribute = coerce

-- | For internal use only, exported to be used by other modules. Ignore this.
unsafeAttribute
  :: forall e. AnAttribute' -> AnAttribute e
unsafeAttribute = AnAttribute

newtype Attribute :: forall k . k -> Type -> Type
newtype Attribute e env = Attribute ( env -> AnAttribute e )
derive instance Newtype ( Attribute e env ) _
instance Contravariant ( Attribute e ) where
  cmap f ( Attribute af ) = Attribute ( f >>> af )

-- | Construct a [data attribute](https://developer.mozilla.org/en-US/docs/Learn/HTML/Howto/Use_data_attributes).
xdata :: forall e env . String -> String -> Attribute e env
xdata k v = Attribute \_ -> unsafeAttribute { key: "data-" <> k, value: Prop' v }
