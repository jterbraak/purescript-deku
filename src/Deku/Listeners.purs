module Deku.Listeners
  ( slider
  , slider_
  , numeric
  , numeric_
  , checkbox
  , checkbox_
  , click
  , click_
  , click'
  , keyUp
  , keyUp_
  , keyDown
  , keyDown_
  , keyPress
  , keyPress_
  , textInput
  , textInput_
  , injectElement
  , injectElementT
  ) where

import Prelude

import Control.Alt (alt)
import Data.Foldable (for_)
import Deku.Attribute (class Attr, Attribute, Cb, attr, cb, (:=))
import Deku.DOM as D
import Effect (Effect)
import Effect.Aff (launchAff_, delay, Milliseconds(..))
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Web.DOM (Element)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (checked, fromEventTarget, value, valueAsNumber)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent)

click
  :: forall cb element
   . Attr element D.OnClick cb
  => Event cb
  -> Event (Attribute element)
click = map (attr D.OnClick)

click_
  :: forall cb element
   . Attr element D.OnClick cb
  => cb
  -> Event (Attribute element)
click_ = click <<< pure

click'
  :: forall m cb element
   . Monoid m
  => Attr element D.OnClick cb
  => Event (m -> cb)
  -> Event (Attribute element)
click' = map (attr D.OnClick <<< (_ $ mempty))

slider_
  :: (Number -> Effect Unit)
  -> Event (Attribute D.Input_)
slider_ = slider <<< pure

slider
  :: Event (Number -> Effect Unit)
  -> Event (Attribute D.Input_)
slider = alt (pure $ D.Xtype := "range") <<< map
  ( \push ->
      D.OnInput := cb \e -> for_
        (target e >>= fromEventTarget)
        (valueAsNumber >=> push)
  )

numeric_
  :: (Number -> Effect Unit)
  -> Event (Attribute D.Input_)
numeric_ = numeric <<< pure

numeric
  :: Event (Number -> Effect Unit)
  -> Event (Attribute D.Input_)
numeric = alt (pure $ D.Xtype := "number") <<< map
  ( \push ->
      D.OnInput := cb \e -> for_
        (target e >>= fromEventTarget)
        (valueAsNumber >=> push)
  )

checkbox_
  :: (Boolean -> Effect Unit)
  -> Event (Attribute D.Input_)
checkbox_ = checkbox <<< pure

checkbox
  :: Event (Boolean -> Effect Unit)
  -> Event (Attribute D.Input_)
checkbox = alt (pure $ D.Xtype := "checkbox") <<< map
  ( \push ->
      D.OnInput := cb \e -> for_
        (target e >>= fromEventTarget)
        (checked >=> push)
  )

textInput_
  :: forall e
   . (String -> Effect Unit)
  -> Event (Attribute e)
textInput_ = textInput <<< pure

textInput
  :: forall e
   . Event (String -> Effect Unit)
  -> Event (Attribute e)
textInput = map \push -> D.OnInput := cb \e -> for_
  (target e >>= fromEventTarget)
  (value >=> push)

keyEvent'
  :: forall f59 a62 e64 b66
   . Functor f59
  => Attr e64 a62 Cb
  => a62
  -> f59 (KeyboardEvent -> Effect b66)
  -> f59 (Attribute e64)
keyEvent' listener = map \f -> listener := cb \e -> for_ (fromEvent e) f

keyUp_
  :: forall eleemnt
   . (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyUp_ = keyUp <<< pure

keyUp
  :: forall eleemnt
   . Event (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyUp = keyEvent' D.OnKeyup

keyDown_
  :: forall eleemnt
   . (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyDown_ = keyDown <<< pure

keyDown
  :: forall eleemnt
   . Event (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyDown = keyEvent' D.OnKeydown

keyPress_
  :: forall eleemnt
   . (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyPress_ = keyPress <<< pure

keyPress
  :: forall eleemnt
   . Event (KeyboardEvent -> Effect Unit)
  -> Event (Attribute eleemnt)
keyPress = keyEvent' D.OnKeypress

injectElement
  :: forall e
   . Attr e D.Self (Element -> Effect Unit)
  => (Element -> Effect Unit)
  -> Event (Attribute e)
injectElement f = pure
  (D.Self := \s -> launchAff_ (delay (Milliseconds 0.0) *> liftEffect (f s)))

injectElementT
  :: forall e te
   . Attr e D.SelfT (te -> Effect Unit)
  => (te -> Effect Unit)
  -> Event (Attribute e)
injectElementT f = pure
  (D.SelfT := \s -> launchAff_ (delay (Milliseconds 0.0) *> liftEffect (f s)))