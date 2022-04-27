Event : Environment {

	classvar defaultParentEvent;
	classvar <parentEvents;
	classvar <partialEvents;

	*new { arg n=8, proto, parent, know=true;
		^super.new(n, proto, parent, know)
	}

	*default {
		^Event.new(8, nil, defaultParentEvent, true);
	}

	*silent { |dur(1.0), inEvent|
		var delta;
		if(inEvent.isNil) { inEvent = Event.new }
		{ inEvent = inEvent.copy };
		delta = dur * (inEvent[\stretch] ? 1);
		if(dur.isRest.not) { dur = Rest(dur) };
		inEvent.put(\dur, dur).put(\parent, defaultParentEvent)
		.put(\delta, delta);
		^inEvent
	}

	// event types

	*addEventType { |type, func, parentEvent|
		partialEvents.playerEvent.eventTypes.put(type, func);
		this.addParentType(type, parentEvent)
	}

	*addParentType { |type, parentEvent|
		if(parentEvent.notNil and: { parentEvent.parent.isNil }) { parentEvent.parent = defaultParentEvent };
		partialEvents.playerEvent.parentTypes.put(type, parentEvent)
	}

	*removeEventType { |type|
		Event.removeParentType(type);
		partialEvents.playerEvent.eventTypes.removeAt(type);
	}

	*removeParentType { |type|
		partialEvents.playerEvent.parentTypes.removeAt(type)
	}

	*parentTypes {
		^this.partialEvents.playerEvent.parentTypes
	}

	*eventTypes {
		^this.partialEvents.playerEvent.eventTypes
	}

	// instance methods

	next { arg inval; ^composeEvents(inval, this) }

	delta {
		_Event_Delta
		^this.primitiveFailed;
		/*
		// implemented by primitive for speed
		var delta;
		delta = this.at('delta');
		if (delta.notNil, { ^delta },{ ^this.at('dur') * this.at('stretch') });
		*/
	}
	play {
		if (parent.isNil) {
			parent = defaultParentEvent;
		};
		this.use {
			this[\play].value;
		};
		//		^this.delta
	}

	isRest {
		_Event_IsRest
		// In theory, we should never get here unless the Event object is corrupted.
		^this.primitiveFailed;
	}

	// node watcher interface

	isPlaying_ { arg val;
		this.put(\isPlaying, val);
	}

	isRunning_ { arg val;
		this.put(\isRunning, val);
	}

	// this method is called by EventStreamPlayer so it can schedule Routines as well
	playAndDelta { | cleanup, mute |
		if (mute) { this.put(\type, \rest) };
		cleanup.update(this);
		this.play;
		^this.delta;
	}

	// A Quant specifies the quant and phase at which a TempoClock starts an EventStreamPlayer
	// Its offset specifies how far ahead of time events are actually computed by the EventStream.
	// offset allows ~strum to be negative, so strummed chords complete on the beat
	// it also makes it possible for one pattern to run a little ahead of another to set values
	// This method keeps ~timingOffset and Quant.offset the same.

	synchWithQuant { | quant |
		if(quant.timingOffset.notNil) {
			^this.copy.put(\timingOffset, quant.timingOffset)
		} {
			quant.timingOffset = this[\timingOffset];
			^this
		};
	}

	// This enables events to represent the server resources they created in an Event
	// So, ~bufnum = (type: \sine1, amps: 1/(1..10)) is possible
	asControlInput {
	}
	asUGenInput { ^this.asControlInput }

	printOn { arg stream, itemsPerLine = 5;
		var max, itemsPerLinem1, i=0;
		itemsPerLinem1 = itemsPerLine - 1;
		max = this.size;
		stream << "( ";
		this.keysValuesDo({ arg key, val;
			stream <<< key << ": " << val;
			if ((i=i+1) < max, { stream.comma.space;
				if (i % itemsPerLine == itemsPerLinem1, { stream.nl.space.space });
			});
		});
		stream << " )";
	}

	storeOn { arg stream, itemsPerLine = 5;
		var max, itemsPerLinem1, i=0;
		itemsPerLinem1 = itemsPerLine - 1;
		max = this.size;
		stream << "( ";
		this.keysValuesDo({ arg key, val;
			stream <<< key << ": " <<< val;
			if ((i=i+1) < max, { stream.comma.space;
				if (i % itemsPerLine == itemsPerLinem1, { stream.nl.space.space });
			});
		});
		stream << " )";
		if(proto.notNil) { stream << "\n.proto_(" <<< proto << ")" };
		if(parent.notNil) { stream << "\n.parent_(" <<< parent << ")" };
	}

	*initClass {
		this.makeParentEvents;
	}

	*makeParentEvents {}
}
