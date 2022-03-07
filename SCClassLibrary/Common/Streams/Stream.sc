Stream : AbstractFunction {
	// 'reset' is defined in class Object to do nothing.
	// reading

	parent { ^nil }
	next { ^this.subclassResponsibility(thisMethod) }
	iter { ^this }
	streamArg { ^this }

	value { arg inval; ^this.next(inval) }
	valueArray { ^this.next }

	all { arg inval;
		// don't do this on infinite streams.
		var array;
		this.do({|item| array = array.add(item) }, inval);
		^array
	}

	// writing
	put { arg item;
		^this.subclassResponsibility(thisMethod)
	}
	putN { arg n, item;
		n.do({ this.put(item); });
	}
	putAll { arg aCollection;
		aCollection.do {|item| this.put(item); };
	}

	do { arg function, inval;
		var item, i=0;
		while {
			item = this.next(inval);
			item.notNil
		}{
			function.value(item, i);
			i = i + 1;
		};
	}

	subSample {| offset= 0, skipSize = 0|
		^Routine {
			offset.do{ this.next };
			loop {
				this.next.yield;
				skipSize.do { this.next }
			}
		}
	}

	generate { arg function, item;
		var i=0;
		while {
			item = this.next(item);
			item.notNil
		}{
			function.value(item, i);
			i = i + 1;
		};
	}

	// combination
	collect { arg argCollectFunc;
		// modify a stream
		var nextFunc = { arg inval;
			var	nextval = this.next(inval);
			if ( nextval.notNil, {
				argCollectFunc.value(nextval, inval)
			})
		};
		var resetFunc = { this.reset };
		^FuncStream.new(nextFunc, resetFunc);
	}
	reject { arg function;
		// reject elements from a stream
		var nextFunc = { arg inval;
			var	nextval = this.next(inval);
			while {
				nextval.notNil and: { function.value(nextval, inval) }
			}{
				nextval = this.next(inval);
			};
			nextval
		};
		var resetFunc = { this.reset };
		^FuncStream.new(nextFunc, resetFunc);
	}
	select { arg function;
		// select elements from a stream
		var nextFunc = { arg inval;
			var	nextval = this.next(inval);
			while {
				nextval.notNil and: { function.value(nextval, inval).not }
			}{
				nextval = this.next(inval);
			};
			nextval
		};
		var resetFunc = { this.reset };
		^FuncStream.new(nextFunc, resetFunc);
	}

	dot { arg function, stream;
		// combine item by item with another stream
		^FuncStream.new(
			{ arg inval;
				var x = this.next(inval);
				var y = stream.next(inval);
				if ( x.notNil and: { y.notNil }, {
					function.value(x, y, inval)
				});
			},
			{ this.reset; stream.reset; }
		);
	}

	interlace { arg function, stream;
		// interlace with another stream
		var nextx = this.next;
		var nexty = stream.next;
		^FuncStream.new({ |inval|
			var val;
			if ( nextx.isNil ) {
				if ( nexty.isNil) {nil}{ val = nexty; nexty = stream.next(inval); val };
			}{
				if ( nexty.isNil or: { function.value(nextx, nexty, inval) },
					{ val = nextx; nextx = this.next(inval); val },
					{ val = nexty; nexty = stream.next(inval); val }
				);
			};
		},
		{
			this.reset; stream.reset;
			nextx = this.next;
			nexty = stream.next;
		});
	}

	++ { arg stream; ^this.appendStream(stream) }

	appendStream { arg stream;
		var reset = false;
		^Routine({ arg inval;
			if (reset) {
				this.reset;
				stream.reset;
			};
			reset = true;
			inval = this.embedInStream(inval);
			stream.embedInStream(inval);
		});
	}

	collate { arg stream;
		// ascending order merge of two streams
		^this.interlace({|x y| x < y }, stream);
	}

	embedInStream { arg inval;
		var outval;
		while {
			outval = this.value(inval);
			outval.notNil
		}{
			inval = outval.yield;
		};
		^inval
	}

	repeat { arg repeats = inf;
		^r { arg inval;
			repeats.value(inval).do {
				inval = this.reset.embedInStream(inval)
			}
		}
	}


}

OneShotStream : Stream {
	var value, once = true;
	*new { arg value;
		^super.newCopyArgs(value)
	}
	next { ^if (once) {once = false; value} }
	reset { once = true }
}

EmbedOnce : Stream  {
	var <stream, <cleanup;
	*new { arg stream, cleanup;
		^super.newCopyArgs(stream.asStream, cleanup)
	}
	next { arg inval;
		var val = stream.next(inval);
		if(val.isNil) { // embed once, then release memory
			cleanup !? { cleanup.exit(inval) };
			stream = nil;
			cleanup = nil;
		} {
			cleanup.update(val)
		};
		^val
	}
	reset {
		stream.reset
	}
}


FuncStream : Stream {
	var <>nextFunc; // Func is evaluated for each next state
	var <>resetFunc; // Func is evaluated on reset
	var	<>envir;
	*new { |nextFunc, resetFunc|
		^super.new.nextFunc_(nextFunc).resetFunc_(resetFunc).envir_(currentEnvironment)
	}
	next { arg inval;
		^envir.use({ nextFunc.value(inval) })
	}
	reset {
		^envir.use({ resetFunc.value })
	}
}

StreamClutch : Stream {
	var <>stream, <>connected, value, >reset=true;

	*new { arg pattern, connected = true;
		^super.newCopyArgs(pattern.asStream, connected)
	}

	next { arg inval;
		if(reset) {
			reset = false;
			value = stream.next(inval)
		};
		if(connected.value(inval)) {
			value = stream.next(inval);
		};
		^value
	}
	lastValue { ^value }

	reset {
		stream.reset;
		reset = true
	}

}

CleanupStream : Stream {
	var <stream, <>cleanup;

	*new { arg stream, cleanup;
		^super.newCopyArgs(stream, cleanup)
	}
	next { arg inval;
		var outval = stream.next(inval);
		if (outval.isNil) {
			cleanup.value(this, inval);
			cleanup = nil;
		}
		^outval
	}
	reset {
		stream.reset
	}
}

// PauseStream is a stream wrapper that can be started and stopped.

PauseStream : Stream {
	var <stream, <originalStream, <clock, <nextBeat, <>streamHasEnded=false;
	var isWaiting = false, era=0;

	isPlaying { ^stream.notNil }

	reset { originalStream.reset }
	stop {
		var saveStream = this.stream;
		this.prStop;
 		this.changed(\userStopped);
		if(saveStream === thisThread) {
			nil.alwaysYield
		}
	}
	prStop {
		stream = nil;
		isWaiting = false;
	}
	removedFromScheduler {
		nextBeat = nil;
		this.prStop;
		this.changed(\stopped);
	}
	streamError { this.removedFromScheduler; streamHasEnded = true;  }

	canPause { ^this.streamHasEnded.not }

	pause {
		this.stop;
	}
	resume { arg argClock, quant;
		^this.play(clock ? argClock, false, quant)
	}

	refresh {
		stream = originalStream.threadPlayer_(this)
	}

	start { arg argClock, quant;
		^this.play(argClock, true, quant)
	}

	stream_ { arg argStream;
		originalStream.threadPlayer_(nil);  // not owned any more
		originalStream = argStream.threadPlayer_(this);
		if (stream.notNil, { stream = argStream; streamHasEnded = argStream.isNil; });
	}

	next { arg inval;
		var nextTime = stream.next(inval);
		if (nextTime.isNil) {
			streamHasEnded = stream.notNil;
			this.removedFromScheduler;
		} {
			nextBeat = inval + nextTime
		};	// inval is current logical beat
		^nextTime
	}
	awake { arg beats, seconds, inClock;
		clock = inClock;
		^this.next(beats)
	}
	threadPlayer { ^this }
}

// Task is a PauseStream for wrapping a Routine

Task : PauseStream {
	*new { arg func, clock;
		^super.new(Routine(func), clock)
	}
}
