module Deku.Ctrl2 where

import Prelude

import Control.Plus (empty)
import Data.Array.NonEmpty (NonEmptyArray, fromNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.NonEmpty ((:|))
import Data.Variant (Variant)
import FRP.Event (class IsEvent)
import Prim.Row (class Cons, class Nub, class Union)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

type AudioInterpret :: (Type -> Type) -> Type -> Type
type AudioInterpret event payload = { test :: payload }

newtype Node :: Row Type -> Row Type -> (Type -> Type) -> Type -> Type
newtype Node produced consumed event payload = Node
  (String -> AudioInterpret event payload -> event payload)

newtype GainInput :: Row Type -> Row Type -> (Type -> Type) -> Type -> Type
newtype GainInput produced consumed event payload = GainInput
  (NonEmptyArray (Node produced consumed event payload))

newtype Transition = Transition
  (Variant (linear :: Unit, exponential :: Unit, step :: Unit))

newtype AudioParam = AudioParam
  ( Variant
      ( number :: { n :: Number, o :: Number, t :: Transition }
      , env :: { p :: Array Number, o :: Number, d :: Number }
      , cancel :: { o :: Number }
      , sudden :: { n :: Number }
      )
  )

newtype SinOsc = SinOsc (Variant (frequency :: AudioParam))
newtype Gain = Gain (Variant (gain :: AudioParam))

ref
  :: forall proxy sym consumed event payload
   . IsEvent event
  => Cons sym Unit () consumed
  => proxy sym
  -> Node () consumed event payload
ref _ = Node $ \_ { test } -> pure test

sinOsc
  :: forall event payload
   . IsEvent event
  => event SinOsc
  -> Node () () event payload
sinOsc _ = Node $ \_ { test } -> pure test

sinOsc'
  :: forall proxy sym produced event payload
   . IsEvent event
  => Cons sym Unit () produced
  => proxy sym
  -> event SinOsc
  -> Node produced () event payload
sinOsc' _ _ = Node $ \_ { test } -> pure test

gain
  :: forall produced consumed event payload
   . IsEvent event
  => event Gain
  -> GainInput produced consumed event payload
  -> Node produced consumed event payload
gain _ _ = Node $ \_ { test } -> pure test

gain'
  :: forall proxy sym produced produced' consumed event payload
   . IsEvent event
  => Cons sym Unit produced' produced
  => proxy sym
  -> event Gain
  -> GainInput produced' consumed event payload
  -> Node produced consumed event payload
gain' _ _ _ = Node $ \_ { test } -> pure test

singleton
  :: forall produced consumed event payload
   . IsEvent event
  => Node produced consumed event payload
  -> GainInput produced consumed event payload
singleton a = GainInput (NEA.singleton a)

gainInputCons
  :: forall produced0 produced1 produced2 consumed0 consumed1 consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => Node produced0 consumed0 event payload
  -> Array (Node produced1 consumed1 event payload)
  -> GainInput produced2 consumed2 event payload
gainInputCons a b = GainInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 gainInputCons as :*

gainInputCons2
  :: forall produced0 produced1 produced2 consumed0 consumed1 consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => Node produced0 consumed0 event payload
  -> GainInput produced1 consumed1 event payload
  -> GainInput produced2 consumed2 event payload
gainInputCons2 a b = GainInput (NEA.cons (coerce a) (coerce b))

infixr 6 gainInputCons2 as ::*

gainInputAppend
  :: forall produced0 produced1 produced2 consumed0 consumed1 consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => GainInput produced0 consumed0 event payload
  -> GainInput produced1 consumed1 event payload
  -> GainInput produced2 consumed2 event payload
gainInputAppend a b = GainInput (coerce a <> coerce b)

infixr 6 gainInputAppend as <>*

speaker
  :: forall produced produced' consumed event payload
   . IsEvent event
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => GainInput produced consumed event payload
  -> Node produced consumed event payload
speaker _ = Node $ \_ { test } -> pure test

graph2 = speaker {-k-}
  $ singleton
  $ gain' (Proxy :: _ "j") empty {-j-}
  $ singleton
  $ gain' (Proxy :: _ "i") empty {-i-}
  $ singleton
  $ gain empty {-h-}
  $
    ( gain' (Proxy :: _ "d") empty {-d-}
        $ singleton
        $ gain empty {-b-}
        $ sinOsc empty {-a-} ::*
          ( ref (Proxy :: _ "j") :*
              [ gain empty {-c-}
                  $ singleton
                  $ gain empty {-e-}
                  $ singleton
                  $ ref (Proxy :: _ "d")
              ]
          )
    ) :*
      [ gain empty {-f-}
          $ singleton
          $ gain empty {-g-}
          $ sinOsc empty {-l-} :* [ref (Proxy :: _ "i")]
      ]
