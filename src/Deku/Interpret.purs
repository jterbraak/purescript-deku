module Deku.Interpret
  ( FFIAudioSnapshot
  , effectfulAudioInterpret
  , makeFFIAudioSnapshot
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Deku.Core as C
import Effect (Effect)
import Effect.Random as R
import FRP.Behavior (behavior)
import FRP.Event (Event, create, makeEvent, subscribe)
import Foreign.Object (Object)

-- foreign
data FFIAudioSnapshot

foreign import makeFFIAudioSnapshot :: Effect FFIAudioSnapshot

foreign import destroyUnit_ :: C.DestroyUnit -> FFIAudioSnapshot -> Effect Unit
foreign import disconnectXFromY_ :: C.DisconnectXFromY -> FFIAudioSnapshot -> Effect Unit
foreign import connectXToY_ :: C.ConnectXToY -> FFIAudioSnapshot -> Effect Unit
foreign import makeAllpass_ :: C.MakeAllpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeAnalyser_ :: C.MakeAnalyser -> FFIAudioSnapshot -> Effect Unit
foreign import makeAudioWorkletNode_ :: C.MakeAudioWorkletNode -> FFIAudioSnapshot -> Effect Unit
foreign import makeBandpass_ :: C.MakeBandpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeConstant_ :: C.MakeConstant -> FFIAudioSnapshot -> Effect Unit
foreign import makeConvolver_ :: C.MakeConvolver -> FFIAudioSnapshot -> Effect Unit
foreign import makeDelay_ :: C.MakeDelay -> FFIAudioSnapshot -> Effect Unit
foreign import makeDynamicsCompressor_ :: C.MakeDynamicsCompressor -> FFIAudioSnapshot -> Effect Unit
foreign import makeGain_ :: C.MakeGain -> FFIAudioSnapshot -> Effect Unit
foreign import makeHighpass_ :: C.MakeHighpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeHighshelf_ :: C.MakeHighshelf -> FFIAudioSnapshot -> Effect Unit
foreign import makeInput_ :: C.MakeInput -> FFIAudioSnapshot -> Effect Unit
foreign import makeLoopBuf_ :: C.MakeLoopBuf -> FFIAudioSnapshot -> Effect Unit
foreign import makeLowpass_ :: C.MakeLowpass -> FFIAudioSnapshot -> Effect Unit
foreign import makeLowshelf_ :: C.MakeLowshelf -> FFIAudioSnapshot -> Effect Unit
foreign import makeMediaElement_ :: C.MakeMediaElement -> FFIAudioSnapshot -> Effect Unit
foreign import makeMicrophone_ :: C.MakeMicrophone -> FFIAudioSnapshot -> Effect Unit
foreign import makeNotch_ :: C.MakeNotch -> FFIAudioSnapshot -> Effect Unit
foreign import makePeaking_ :: C.MakePeaking -> FFIAudioSnapshot -> Effect Unit
foreign import makePeriodicOsc_ :: C.MakePeriodicOsc -> FFIAudioSnapshot -> Effect Unit
foreign import makePlayBuf_ :: C.MakePlayBuf -> FFIAudioSnapshot -> Effect Unit
foreign import makeRecorder_ :: C.MakeRecorder -> FFIAudioSnapshot -> Effect Unit
foreign import makeSawtoothOsc_ :: C.MakeSawtoothOsc -> FFIAudioSnapshot -> Effect Unit
foreign import makeSinOsc_ :: C.MakeSinOsc -> FFIAudioSnapshot -> Effect Unit
foreign import makeSpeaker_ :: C.MakeSpeaker -> FFIAudioSnapshot -> Effect Unit
foreign import makeSquareOsc_ :: C.MakeSquareOsc -> FFIAudioSnapshot -> Effect Unit
foreign import makeStereoPanner_ :: C.MakeStereoPanner -> FFIAudioSnapshot -> Effect Unit

effectfulAudioInterpret
  :: C.AudioInterpret Event (FFIAudioSnapshot -> Effect Unit)
effectfulAudioInterpret =
  { ids: map show $ behavior \f -> makeEvent \k -> do
      r <- R.random
      subscribe f \x -> k (x r)
  , destroyUnit: destroyUnit_
  , disconnectXFromY: disconnectXFromY_
  , connectXToY: connectXToY_
  , makeAllpass: makeAllpass_
  , makeAnalyser: makeAnalyser_
  , makeAudioWorkletNode: makeAudioWorkletNode_
  , makeBandpass: makeBandpass_
  , makeConstant: makeConstant_
  , makeConvolver: makeConvolver_
  , makeDelay: makeDelay_
  , makeDynamicsCompressor: makeDynamicsCompressor_
  , makeGain: makeGain_
  , makeHighpass: makeHighpass_
  , makeHighshelf: makeHighshelf_
  , makeInput: makeInput_
  , makeLoopBuf: makeLoopBuf_
  , makeLowpass: makeLowpass_
  , makeLowshelf: makeLowshelf_
  , makeMediaElement: makeMediaElement_
  , makeMicrophone: makeMicrophone_
  , makeNotch: makeNotch_
  , makePeaking: makePeaking_
  , makePeriodicOsc: makePeriodicOsc_
  , makePlayBuf: makePlayBuf_
  , makeRecorder: makeRecorder_
  , makeSawtoothOsc: makeSawtoothOsc_
  , makeSinOsc: makeSinOsc_
  , makeSpeaker: makeSpeaker_
  , makeSquareOsc: makeSquareOsc_
  , makeStereoPanner: makeStereoPanner_
  , makeSubgraph: makeSubgraph_
  , makeTriangleOsc: makeTriangleOsc_
  , makeTumult: makeTumult_
  , makeWaveShaper: makeWaveShaper_
  , setAnalyserNodeCb: setAnalyserNodeCb_
  , setMediaRecorderCb: setMediaRecorderCb_
  , setWaveShaperCurve: setWaveShaperCurve_
  , setAudioWorkletParameter: setAudioWorkletParameter_
  , setBuffer: setBuffer_
  , setConvolverBuffer: setConvolverBuffer_
  , setPeriodicOsc: setPeriodicOsc_
  , setOnOff: setOnOff_
  , setBufferOffset: setBufferOffset_
  , setLoopStart: setLoopStart_
  , setLoopEnd: setLoopEnd_
  , setRatio: setRatio_
  , setOffset: setOffset_
  , setAttack: setAttack_
  , setGain: setGain_
  , setQ: setQ_
  , setPan: setPan_
  , setThreshold: setThreshold_
  , setRelease: setRelease_
  , setKnee: setKnee_
  , setDelay: setDelay_
  , setPlaybackRate: setPlaybackRate_
  , setFrequency: setFrequency_
  , setInput: setInput_
  , removeSubgraph: removeSubgraph_
  , insertOrUpdateSubgraph: insertOrUpdateSubgraph_
  , setTumult: setTumult_
  }
