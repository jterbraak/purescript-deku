-- This module has been automatically generated by running `spago run -p codegen`.
-- Any changes may be overwritten.
module Deku.DOM.Self where

import Control.Applicative (pure, class Applicative) as Applicative
import Control.Category ((<<<))
import Data.Functor (map, class Functor) as Functor
import Type.Proxy (Proxy)
import Deku.Attribute as Deku.Attribute
import Web.DOM.Element as Web.DOM.Element
import Effect as Effect
import Data.Unit as Data.Unit
import Unsafe.Coerce as Unsafe.Coerce
import Web.HTML.HTMLAnchorElement as Web.HTML.HTMLAnchorElement
import Web.HTML.HTMLAreaElement as Web.HTML.HTMLAreaElement
import Web.HTML.HTMLAudioElement as Web.HTML.HTMLAudioElement
import Web.HTML.HTMLBRElement as Web.HTML.HTMLBRElement
import Web.HTML.HTMLBaseElement as Web.HTML.HTMLBaseElement
import Web.HTML.HTMLBodyElement as Web.HTML.HTMLBodyElement
import Web.HTML.HTMLButtonElement as Web.HTML.HTMLButtonElement
import Web.HTML.HTMLCanvasElement as Web.HTML.HTMLCanvasElement
import Web.HTML.HTMLDivElement as Web.HTML.HTMLDivElement
import Web.HTML.HTMLEmbedElement as Web.HTML.HTMLEmbedElement
import Web.HTML.HTMLFormElement as Web.HTML.HTMLFormElement
import Web.HTML.HTMLHRElement as Web.HTML.HTMLHRElement
import Web.HTML.HTMLHeadElement as Web.HTML.HTMLHeadElement
import Web.HTML.HTMLHtmlElement as Web.HTML.HTMLHtmlElement
import Web.HTML.HTMLInputElement as Web.HTML.HTMLInputElement
import Web.HTML.HTMLLabelElement as Web.HTML.HTMLLabelElement
import Web.HTML.HTMLLegendElement as Web.HTML.HTMLLegendElement
import Web.HTML.HTMLLinkElement as Web.HTML.HTMLLinkElement
import Web.HTML.HTMLMapElement as Web.HTML.HTMLMapElement
import Web.HTML.HTMLMetaElement as Web.HTML.HTMLMetaElement
import Web.HTML.HTMLMeterElement as Web.HTML.HTMLMeterElement
import Web.HTML.HTMLObjectElement as Web.HTML.HTMLObjectElement
import Web.HTML.HTMLOptionElement as Web.HTML.HTMLOptionElement
import Web.HTML.HTMLOutputElement as Web.HTML.HTMLOutputElement
import Web.HTML.HTMLParagraphElement as Web.HTML.HTMLParagraphElement
import Web.HTML.HTMLParamElement as Web.HTML.HTMLParamElement
import Web.HTML.HTMLPreElement as Web.HTML.HTMLPreElement
import Web.HTML.HTMLProgressElement as Web.HTML.HTMLProgressElement
import Web.HTML.HTMLScriptElement as Web.HTML.HTMLScriptElement
import Web.HTML.HTMLSelectElement as Web.HTML.HTMLSelectElement
import Web.HTML.HTMLSourceElement as Web.HTML.HTMLSourceElement
import Web.HTML.HTMLSpanElement as Web.HTML.HTMLSpanElement
import Web.HTML.HTMLStyleElement as Web.HTML.HTMLStyleElement
import Web.HTML.HTMLTableDataCellElement as Web.HTML.HTMLTableDataCellElement
import Web.HTML.HTMLTableElement as Web.HTML.HTMLTableElement
import Web.HTML.HTMLTemplateElement as Web.HTML.HTMLTemplateElement
import Web.HTML.HTMLTextAreaElement as Web.HTML.HTMLTextAreaElement
import Web.HTML.HTMLTimeElement as Web.HTML.HTMLTimeElement
import Web.HTML.HTMLTitleElement as Web.HTML.HTMLTitleElement
import Web.HTML.HTMLTrackElement as Web.HTML.HTMLTrackElement
import Web.HTML.HTMLVideoElement as Web.HTML.HTMLVideoElement

class IsSelf (element :: Type) (name :: Symbol) | element -> name

-- | Creates a special event where an Deku element can have its raw DOM element
-- | injected into a closure. All bets are off type-safety wise. This is useful
-- | when you need to manipulate the element itself, like for example attaching
-- | properties to it, etc.
self
  :: forall r f
   . Functor.Functor f
  => f (Web.DOM.Element.Element -> Effect.Effect Data.Unit.Unit)
  -> f (Deku.Attribute.Attribute r)
self = Functor.map
  ( Deku.Attribute.unsafeAttribute <<< Deku.Attribute.cb' "@self@" <<< Deku.Attribute.cb <<<
      Unsafe.Coerce.unsafeCoerce
  )

-- | Shorthand version of `self`
self_
  :: forall r f
   . Applicative.Applicative f
  => (Web.DOM.Element.Element -> Effect.Effect Data.Unit.Unit)
  -> f (Deku.Attribute.Attribute r)
self_ = self <<< Applicative.pure

-- | A slightly less permissive version of `Self` that associates Deku Elements to
-- | the primitive element definitions form `purescript-web`. For example, `A_` from `deku`
-- | gets translated to `HTMLAnchorElement` from `purescript-web`, etc.
selfT
  :: forall name e r f
   . Functor.Functor f
  => IsSelf e name
  => f (e -> Effect.Effect Data.Unit.Unit)
  -> f (Deku.Attribute.Attribute (__tag :: Proxy name | r))
selfT = Functor.map
  ( Deku.Attribute.unsafeAttribute <<< Deku.Attribute.cb' "@self@" <<< Deku.Attribute.cb <<<
      Unsafe.Coerce.unsafeCoerce
  )

-- | Shorthand version of `selfT`
selfT_
  :: forall name e r f
   . Applicative.Applicative f
  => IsSelf e name
  => (e -> Effect.Effect Data.Unit.Unit)
  -> f (Deku.Attribute.Attribute (__tag :: Proxy name | r))
selfT_ = selfT <<< Applicative.pure

instance IsSelf Web.HTML.HTMLAnchorElement.HTMLAnchorElement "HTMLAnchorElement"
instance IsSelf Web.HTML.HTMLAreaElement.HTMLAreaElement "HTMLAreaElement"
instance IsSelf Web.HTML.HTMLAudioElement.HTMLAudioElement "HTMLAudioElement"
instance IsSelf Web.HTML.HTMLBRElement.HTMLBRElement "HTMLBRElement"
instance IsSelf Web.HTML.HTMLBaseElement.HTMLBaseElement "HTMLBaseElement"
instance IsSelf Web.HTML.HTMLBodyElement.HTMLBodyElement "HTMLBodyElement"
instance IsSelf Web.HTML.HTMLButtonElement.HTMLButtonElement "HTMLButtonElement"
instance IsSelf Web.HTML.HTMLCanvasElement.HTMLCanvasElement "HTMLCanvasElement"
instance IsSelf Web.HTML.HTMLDivElement.HTMLDivElement "HTMLDivElement"
instance IsSelf Web.HTML.HTMLEmbedElement.HTMLEmbedElement "HTMLEmbedElement"
instance IsSelf Web.HTML.HTMLFormElement.HTMLFormElement "HTMLFormElement"
instance IsSelf Web.HTML.HTMLHRElement.HTMLHRElement "HTMLHRElement"
instance IsSelf Web.HTML.HTMLHeadElement.HTMLHeadElement "HTMLHeadElement"
instance IsSelf Web.HTML.HTMLHtmlElement.HTMLHtmlElement "HTMLHtmlElement"
instance IsSelf Web.HTML.HTMLInputElement.HTMLInputElement "HTMLInputElement"
instance IsSelf Web.HTML.HTMLLabelElement.HTMLLabelElement "HTMLLabelElement"
instance IsSelf Web.HTML.HTMLLegendElement.HTMLLegendElement "HTMLLegendElement"
instance IsSelf Web.HTML.HTMLLinkElement.HTMLLinkElement "HTMLLinkElement"
instance IsSelf Web.HTML.HTMLMapElement.HTMLMapElement "HTMLMapElement"
instance IsSelf Web.HTML.HTMLMetaElement.HTMLMetaElement "HTMLMetaElement"
instance IsSelf Web.HTML.HTMLMeterElement.HTMLMeterElement "HTMLMeterElement"
instance IsSelf Web.HTML.HTMLObjectElement.HTMLObjectElement "HTMLObjectElement"
instance IsSelf Web.HTML.HTMLOptionElement.HTMLOptionElement "HTMLOptionElement"
instance IsSelf Web.HTML.HTMLOutputElement.HTMLOutputElement "HTMLOutputElement"
instance IsSelf Web.HTML.HTMLParagraphElement.HTMLParagraphElement "HTMLParagraphElement"
instance IsSelf Web.HTML.HTMLParamElement.HTMLParamElement "HTMLParamElement"
instance IsSelf Web.HTML.HTMLPreElement.HTMLPreElement "HTMLPreElement"
instance IsSelf Web.HTML.HTMLProgressElement.HTMLProgressElement "HTMLProgressElement"
instance IsSelf Web.HTML.HTMLScriptElement.HTMLScriptElement "HTMLScriptElement"
instance IsSelf Web.HTML.HTMLSelectElement.HTMLSelectElement "HTMLSelectElement"
instance IsSelf Web.HTML.HTMLSourceElement.HTMLSourceElement "HTMLSourceElement"
instance IsSelf Web.HTML.HTMLSpanElement.HTMLSpanElement "HTMLSpanElement"
instance IsSelf Web.HTML.HTMLStyleElement.HTMLStyleElement "HTMLStyleElement"
instance
  IsSelf Web.HTML.HTMLTableDataCellElement.HTMLTableDataCellElement "HTMLTableDataCellElement"

instance IsSelf Web.HTML.HTMLTableElement.HTMLTableElement "HTMLTableElement"
instance IsSelf Web.HTML.HTMLTemplateElement.HTMLTemplateElement "HTMLTemplateElement"
instance IsSelf Web.HTML.HTMLTextAreaElement.HTMLTextAreaElement "HTMLTextAreaElement"
instance IsSelf Web.HTML.HTMLTimeElement.HTMLTimeElement "HTMLTimeElement"
instance IsSelf Web.HTML.HTMLTitleElement.HTMLTitleElement "HTMLTitleElement"
instance IsSelf Web.HTML.HTMLTrackElement.HTMLTrackElement "HTMLTrackElement"
instance IsSelf Web.HTML.HTMLVideoElement.HTMLVideoElement "HTMLVideoElement"
