module Deku.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.Foldable (oneOf)
import Data.NonEmpty ((:|))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, D1, pred, toInt)
import Data.Variant (match, unvariant)
import Data.Homogeneous.Variant (homogeneous)
import Deku.Core (AudioParameter(..))
import Deku.Core as C
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import Foreign.Object (fromHomogeneous)
import Prim.Row (class Cons, class Nub, class Union)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

-- gain input
singleton
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Node outputChannels produced consumed event payload
  -> C.GainInput outputChannels produced consumed event payload
singleton a = C.GainInput (NEA.singleton a)

gainInputCons
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event payload
  -> Array (C.Node outputChannels produced1 consumed1 event payload)
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputCons a b = C.GainInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 gainInputCons as :*

gainInputCons2
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event payload
  -> C.GainInput outputChannels produced1 consumed1 event payload
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputCons2 a b = C.GainInput (NEA.cons (coerce a) (coerce b))

infixr 6 gainInputCons2 as ::*

gainInputAppend
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.GainInput outputChannels produced0 consumed0 event payload
  -> C.GainInput outputChannels produced1 consumed1 event payload
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputAppend a b = C.GainInput (coerce a <> coerce b)

infixr 6 gainInputAppend as <>*

-- allpass

allpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeAllpass
  -> event C.Allpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
allpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeAllpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          ( pure
              ( makeAllpass
                  { id: me, parent: parent, frequency: i.frequency, q: i.q }
              )
          )
            <|> map
              ( \(C.Allpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )

allpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym Unit produced' produced
  => proxy sym
  -> C.InitializeAllpass
  -> event C.Allpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
allpass' _ i atts elts = let C.Node n = allpass i atts elts in C.Node n

-- analyser
analyser
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeAnalyser
  -> event C.Analyser
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
analyser i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeAnalyser, setAnalyserNodeCb }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure (makeAnalyser { id: me, parent: parent, cb: i.cb })
            <|> map
              ( \(C.Analyser e) -> match
                  { cb: \cb -> setAnalyserNodeCb { id: me, cb }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )

-- audio worklet

class
  ValidateOutputChannelCount
    (numberOfOutputs :: Type)
    (outputChannelCount :: Type) where
  toOutputChannelCount
    :: forall proxy
     . numberOfOutputs
    -> outputChannelCount
    -> Array Int

instance validateOutputChannelCountD1 ::
  Pos n =>
  ValidateOutputChannelCount D1 n where
  toOutputChannelCount _ n = [ toInt n ]
else instance validateOutputChannelCountN ::
  ( Pred x xMinus1
  , Pos n
  , ValidateOutputChannelCount xMinus1 r
  ) =>
  ValidateOutputChannelCount x (n /\ r) where
  toOutputChannelCount x (n /\ r) = [ toInt n ] <> toOutputChannelCount
    (pred x)
    r

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterOptions produced consumed event payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData AudioParameter
  => JSON.WriteForeign { | processorOptions }
  => IsEvent event
  => C.InitializeAudioWorkletNode numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs produced consumed event payload
  -> C.Node numberOfOutputs produced consumed event payload
audioWorklet i atts elt = C.Node go
  where
  go
    parent
    di@
      (C.AudioInterpret { ids, makeAudioWorkletNode, setAudioWorkletParameter }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeAudioWorkletNode
                { id: me
                , parent: parent
                , options:
                    { name: reflectSymbol (Proxy :: _ name)
                    , numberOfInputs: toInt i.numberOfInputs
                    , numberOfOutputs: toInt i.numberOfOutputs
                    , outputChannelCount: toOutputChannelCount i.numberOfOutputs
                        i.outputChannelCount
                    , parameterData: fromHomogeneous i.parameterData
                    , processorOptions: JSON.writeImpl i.processorOptions
                    }
                }
            )
            <|> map
              ( \(C.AudioWorkletNode e) -> setAudioWorkletParameter
                  { id: me
                  , paramName: unvariant e (\sym _ -> reflectSymbol sym)
                  , paramValue: extract (homogeneous e)
                  }
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

-- bandpass

bandpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeBandpass
  -> event C.Bandpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
bandpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeBandpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeBandpass
                { id: me, parent: parent, frequency: i.frequency, q: i.q }
            )
            <|> map
              ( \(C.Bandpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

bandpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym Unit produced' produced
  => proxy sym
  -> C.InitializeBandpass
  -> event C.Bandpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
bandpass' _ i atts elts = let C.Node n = bandpass i atts elts in C.Node n

-- gain

gain_
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeGain
  -> event C.Gain
  -> C.Node outputChannels produced consumed event payload
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
gain_ i atts h t = gain i atts (C.GainInput (NEA.fromNonEmpty (h :| t)))

gain
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeGain
  -> event C.Gain
  -> C.GainInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
gain i atts (C.GainInput elts) = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeGain, setGain }) = keepLatest
    ( (sample_ ids (pure unit)) <#> \me ->
        pure (makeGain { id: me, parent: parent, gain: i.gain })
          <|> map
            ( \(C.Gain e) -> match
                { gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) me di) elts)
            )
    )

gain'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym Unit produced' produced
  => proxy sym
  -> C.InitializeGain
  -> event C.Gain
  -> C.GainInput outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
gain' _ i atts elts = let C.Node n = gain i atts elts in C.Node n

ref
  :: forall proxy sym outputChannels consumed event payload
   . IsEvent event
  => IsSymbol sym
  => Cons sym Unit () consumed
  => proxy sym
  -> C.Node outputChannels () consumed event payload
ref px = C.Node go
  where
  go parent (C.AudioInterpret { connectXToY }) =
    pure (connectXToY { from: reflectSymbol px, to: parent })

sinOsc
  :: forall event payload
   . IsEvent event
  => C.InitializeSinOsc
  -> event C.SinOsc
  -> C.Node D1 () () event payload
sinOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeSinOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSinOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
                }
            )
            <|> map
              ( \(C.SinOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , onOff: \onOff -> setOnOff { id: me, onOff }
                  }
                  e
              )
              atts
      )

sinOsc'
  :: forall proxy sym produced event payload
   . IsEvent event
  => Cons sym Unit () produced
  => proxy sym
  -> C.InitializeSinOsc
  -> event C.SinOsc
  -> C.Node D1 produced () event payload
sinOsc' _ i atts = let C.Node n = sinOsc i atts in C.Node n

speaker
  :: forall outputChannels produced produced' consumed event payload
   . IsEvent event
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => C.GainInput outputChannels produced consumed event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker (C.GainInput elts) di@(C.AudioInterpret { ids, makeSpeaker }) =
  keepLatest
    ( (sample_ ids (pure unit)) <#> \me ->
        pure (makeSpeaker { id: me })
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) me di) elts)
    )
