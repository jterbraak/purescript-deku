var makeid = function (length) {
	var result = "";
	var characters =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	var charactersLength = characters.length;
	for (var i = 0; i < length; i++) {
		result += characters.charAt(Math.floor(Math.random() * charactersLength));
	}
	return result;
};
var NUMERIC = "numeric";
var CANCELLATION = "cancellation";
var IMMEDIATELY = "immediately";
var NO_RAMP = "noRamp";
var LINEAR_RAMP = "linearRamp";
var EXPONENTIAL_RAMP = "exponentialRamp";
var ENVELOPE = "envelope";
var isOn = function (param) {
	return param.type === "on" || param.type === "offOn";
};
var connectXToY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				state.units[x].outgoing.push({ unit: y, state: state });
				state.units[y].incoming.push({ unit: x, state: state });
				state.units[x].main.connect(state.units[y].main);
				if (state.units[y].se) {
					state.units[x].main.connect(state.units[y].se);
				}
			};
		};
	};
};
exports.connectXToY_ = connectXToY_;

var disconnectXFromY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				state.units[x].outgoing = state.units[x].outgoing.filter(function (i) {
					return !(i.unit === y);
				});
				state.units[y].incoming = state.units[y].incoming.filter(function (i) {
					return !(i.unit === x);
				});
				state.units[x].main.connect(state.units[y].main);
				if (state.units[y].se) {
					state.units[x].main.connect(state.units[y].se);
				}
			};
		};
	};
};
exports.disconnectXFromY_ = disconnectXFromY_;
exports.destroyUnit_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			// hack for recorder
			if (state.units[ptr].recorder) {
				state.units[ptr].recorder.stop();
			}
			// hack for analyser
			if (state.units[ptr].analyser) {
				// effectful unsubscribe
				state.units[ptr].analyser();
			}
			delete state.units[ptr];
		};
	};
};
// allpass
exports.makeAllpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "allpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// analyser
exports.makeAnalyser_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var analyserSideEffectFunction = a.cb;
			var dest = state.context.createAnalyser();
			// todo - unhardcode?
			dest.fftSize = 2048;
			// unsubscribe is effect unit
			var unsubscribe = analyserSideEffectFunction(dest)();
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				analyserOrig: analyserSideEffectFunction,
				analyser: unsubscribe,
				main: state.context.createGain(),
				se: dest,
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// audio worklet node
exports.makeAudioWorkletNode_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.options;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: new AudioWorkletNode(state.context, a.name, {
					numberOfInputs: a.numberOfInputs,
					numberOfOutputs: a.numberOfOutputs,
					outputChannelCount: a.outputChannelCount,
					parameterData: a.parameterData,
					processorOptions: a.processorOptions,
				}),
			};
			connectXToY_(ptr)(aa.parent)(state)();
		};
	};
};

// bandpass
exports.makeBandpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "bandpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// constant
exports.makeConstant_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new ConstantSourceNode(context, i);
			};
			var resume = { offset: a.offset };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

exports.makeConvolver_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: new ConvolverNode(state.context, { buffer: a.buffer }),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// delay
exports.makeDelay_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new DelayNode(state.context, {
					delayTime: a.delayTime,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// dynamicsCompressor
exports.makeDynamicsCompressor_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new DynamicsCompressorNode(state.context, {
					knee: a.knee,
					ratio: a.ratio,
					threshold: a.threshold,
					attack: a.attack,
					release: a.release,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// dynamicsCompressor
exports.makeGain_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new GainNode(state.context, {
					gain: a.gain,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};
// highpass
exports.makeHighpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "highpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// highshelf
exports.makeHighshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "highshelf",
					frequency: a.frequency,
					gain: a.gain,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// input
exports.makeInput_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var parent = a.parent;
			setImmediate(function () {
				connectXToY_(ptr)(parent)(state)();
			});
		};
	};
};

// loopBuf
exports.makeLoopBuf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new AudioBufferSourceNode(context, i);
			};
			var resume = {
				loop: true,
				buffer: a.buffer,
				loopStart: a.loopStart,
				loopEnd: a.loopEnd,
				playbackRate: a.playbackRate,
			};
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// lowpass
exports.makeLowpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "lowpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// lowshelf
exports.makeLowshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "lowshelf",
					frequency: a.frequency,
					gain: a.gain,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// media element

exports.makeMediaElement_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var elt = a.element;
			var createFunction = function () {
				var unit = state.context.createMediaElementSource(elt);
				return unit;
			};
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				createFunction: createFunction,
				resumeClosure: {},
				main: createFunction(),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// microphone
exports.makeMicrophone_ = function (a) {
	return function (state) {
		return function () {
			state.units[a.id] = {
				main: state.context.createMediaStreamSource(a.microphone),
				outgoing: [a.parent],
				incoming: [],
			};
		};
	};
};

// notch
exports.makeNotch_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "notch",
					frequency: a.frequency,
					Q: a.q,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// peaking
exports.makePeaking_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "peaking",
					frequency: a.frequency,
					Q: a.q,
					gain: a.gain,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// periodic osc
exports.makePeriodicOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				var o = new OscillatorNode(context, i);
				o.setPeriodicWave(
					i.spec.type === "wave"
						? i.spec.value
						: makePeriodicWaveImpl(state.context)(i.spec.value.real)(
								i.spec.value.img
						  )()
				);
				return o;
			};
			var resume = { frequency: a.frequency, type: "custom" };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// playBuf
exports.makePlayBuf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new AudioBufferSourceNode(context, i);
			};
			var resume = {
				loop: false,
				buffer: a.buffer,
				playbackRate: a.playbackRate,
			};
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				bufferOffset: a.bufferOffset,
				duration: a.duration,
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(
					state.writeHead + onOff.timeOffset,
					a.bufferOffset,
					a.duration.type === "just" ? a.duration.value : undefined
				);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// recorder
exports.makeRecorder_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var mediaRecorderSideEffectFn = aa.cb;
			var dest = state.context.createMediaStreamDestination();
			var mediaRecorder = new MediaRecorder(dest.stream);
			mediaRecorderSideEffectFn(mediaRecorder)();
			mediaRecorder.start();
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				recorderOrig: mediaRecorderSideEffectFn,
				recorder: mediaRecorder,
				main: state.context.createGain(),
				se: dest,
			};
			connectXToY_(ptr)(aa.parent)(state)();
		};
	};
};

// sawtooth osc
exports.makeSawtoothOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "sawtooth" };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// sine osc
exports.makeSinOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "sine" };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// make speaker
exports.makeSpeaker_ = function (a) {
	return function (state) {
		return function () {
			state.units[a.id] = {
				outgoing: [],
				incoming: [],
				main: state.context.createGain(),
				se: state.context.destination,
			};
		};
	};
};
// pan
exports.makeStereoPanner_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new StereoPannerNode(state.context, {
					pan: a.pan,
				}),
			};
			connectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// square osc
exports.makeSquareOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "square" };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// triangle osc
exports.makeTriangleOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "triangle" };
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			connectXToY_(ptr)(a.parent)(state)();
			var oo = isOn(onOff.onOff);
			if (oo) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = oo;
		};
	};
};

// various and sundry... mostly sundry... //
exports.makeFloatArray = function (fa) {
	return function () {
		var r = new Float32Array(fa.length);
		for (var i = 0; i < fa.length; i++) {
			r[i] = fa[i];
		}
		return r;
	};
};

exports.stopMediaRecorder = function (mediaRecorder) {
	return function () {
		mediaRecorder.stop();
	};
};

exports.isTypeSupported = function (mimeType) {
	return function () {
		return MediaRecorder.isTypeSupported(mimeType);
	};
};
// currently, there is no unsubscription logic to the media recorder
// in the case where a second subscriber is called, it will simply
// overwrite the first subscriber
// because of this, care needs to be taken in calling the "setMediaRecorderCb" function
// it will unset the previous one, which will result in the recording starting from the moment
// of being set
// if it is set in a loop, then there will effectively be no recording, as it will only capture the
// last couple milliseconds of the loop
exports.mediaRecorderToBlob = function (mimeType) {
	return function (handler) {
		return function (mediaRecorder) {
			return function () {
				var chunks = [];
				mediaRecorder.ondataavailable = function (evt) {
					chunks.push(evt.data);
				};

				mediaRecorder.onstop = function () {
					var blob = new Blob(chunks, { type: mimeType });
					handler(blob)();
					chunks = null;
				};
			};
		};
	};
};
// setting makes us stop the previous one if it exists
exports.setMediaRecorderCb_ = function (aa) {
	return function (state) {
		return function () {
			var a = aa.cb;
			var ptr = aa.id;
			if (state.units[ptr].recorderOrig === a) {
				return;
			}
			state.units[ptr].recorder && state.units[ptr].recorder.stop();
			var mediaRecorderSideEffectFn = a;
			state.units[ptr].recorderOrig = a;
			var mediaRecorder = new MediaRecorder(state.units[ptr].se);
			mediaRecorderSideEffectFn(mediaRecorder)();
			mediaRecorder.start();
		};
	};
};
exports.getBrowserMediaStreamImpl = function (audio) {
	return function (video) {
		return function () {
			return navigator.mediaDevices.getUserMedia({
				audio: audio,
				video: video,
			});
		};
	};
};

exports.getFFTSize = function (analyserNode) {
	return function () {
		return analyserNode.fftSize;
	};
};

exports.setFFTSize = function (analyserNode) {
	return function (fftSize) {
		return function () {
			analyserNode.fftSize = fftSize;
		};
	};
};

exports.getSmoothingTimeConstant = function (analyserNode) {
	return function () {
		return analyserNode.smoothingTimeConstant;
	};
};

exports.setSmoothingTimeConstant = function (analyserNode) {
	return function (smoothingTimeConstant) {
		return function () {
			analyserNode.smoothingTimeConstant = smoothingTimeConstant;
		};
	};
};

exports.getMinDecibels = function (analyserNode) {
	return function () {
		return analyserNode.minDecibels;
	};
};

exports.setMinDecibels = function (analyserNode) {
	return function (minDecibels) {
		return function () {
			analyserNode.minDecibels = minDecibels;
		};
	};
};

exports.getMaxDecibels = function (analyserNode) {
	return function () {
		return analyserNode.maxDecibels;
	};
};

exports.setMaxDecibels = function (analyserNode) {
	return function (maxDecibels) {
		return function () {
			analyserNode.maxDecibels = maxDecibels;
		};
	};
};

exports.getFrequencyBinCount = function (analyserNode) {
	return function () {
		return analyserNode.frequencyBinCount;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatTimeDomainData
exports.getFloatTimeDomainData = function (analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.fftSize);
		analyserNode.getFloatTimeDomainData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatFrequencyData
exports.getFloatFrequencyData = function (analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.frequencyBinCount);
		analyserNode.getFloatFrequencyData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteTimeDomainData
exports.getByteTimeDomainData = function (analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.fftSize);
		analyserNode.getByteTimeDomainData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteFrequencyData
exports.getByteFrequencyData = function (analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.frequencyBinCount);
		analyserNode.getByteFrequencyData(dataArray);
		return dataArray;
	};
};
exports.bufferSampleRate = function (buffer) {
	return buffer.sampleRate;
};
exports.bufferLength = function (buffer) {
	return buffer.length;
};
exports.bufferDuration = function (buffer) {
	return buffer.duration;
};
exports.bufferNumberOfChannels = function (buffer) {
	return buffer.numberOfChannels;
};
exports.constant0Hack = function (context) {
	return function () {
		var constant = context.createConstantSource();
		constant.offset.value = 0.0;
		constant.connect(context.destination);
		constant.start();
		return function () {
			constant.stop();
			constant.disconnect(context.destination);
		};
	};
};

var makePeriodicWaveImpl = function (ctx) {
	return function (real_) {
		return function (imag_) {
			return function () {
				var real = new Float32Array(real_.length);
				var imag = new Float32Array(imag_.length);
				for (var i = 0; i < real_.length; i++) {
					real[i] = real_[i];
				}
				for (var i = 0; i < imag_.length; i++) {
					imag[i] = imag_[i];
				}
				return ctx.createPeriodicWave(real, imag, {
					disableNormalization: true,
				});
			};
		};
	};
};
exports.makePeriodicWaveImpl = makePeriodicWaveImpl;
