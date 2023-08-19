-- | This module contains reexports of all the event listeners.
-- This module has been automatically generated by running `spago run -p codegen`.
-- Any changes may be overwritten.
module Deku.DOM.SVG.Listeners (module Combinators, repeat, repeat_, end, end_, begin, begin_) where

import Control.Applicative (pure) as Applicative
import Control.Category ((<<<))
import Data.Functor (map) as Functor
import FRP.Poll as FRP.Poll
import Deku.DOM.Combinators (unset, injectElement, injectElementT, runOn, runOn_, numberOn, numberOn_, checkedOn, checkedOn_, valueOn, valueOn_) as Combinators
import Deku.Attribute as Deku.Attribute
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Effect as Effect
import Data.Unit as Data.Unit
import Unsafe.Coerce as Unsafe.Coerce

repeat
  :: forall r
   . FRP.Poll.Poll (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (repeat :: Web.Event.Internal.Types.Event | r))
repeat = Functor.map
  ( Deku.Attribute.unsafeAttribute <<< { key: "repeat", value: _ } <<< Deku.Attribute.cb'
      <<< Deku.Attribute.cb
      <<< Unsafe.Coerce.unsafeCoerce
  )

repeat_
  :: forall r
   . (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (repeat :: Web.Event.Internal.Types.Event | r))
repeat_ = repeat <<< Applicative.pure

end
  :: forall r
   . FRP.Poll.Poll (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (end :: Web.Event.Internal.Types.Event | r))
end = Functor.map
  ( Deku.Attribute.unsafeAttribute <<< { key: "end", value: _ } <<< Deku.Attribute.cb'
      <<< Deku.Attribute.cb
      <<< Unsafe.Coerce.unsafeCoerce
  )

end_
  :: forall r
   . (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (end :: Web.Event.Internal.Types.Event | r))
end_ = end <<< Applicative.pure

begin
  :: forall r
   . FRP.Poll.Poll (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (begin :: Web.Event.Internal.Types.Event | r))
begin = Functor.map
  ( Deku.Attribute.unsafeAttribute <<< { key: "begin", value: _ } <<< Deku.Attribute.cb'
      <<< Deku.Attribute.cb
      <<< Unsafe.Coerce.unsafeCoerce
  )

begin_
  :: forall r
   . (Web.Event.Internal.Types.Event -> Effect.Effect Data.Unit.Unit)
  -> FRP.Poll.Poll (Deku.Attribute.Attribute (begin :: Web.Event.Internal.Types.Event | r))
begin_ = begin <<< Applicative.pure
