Nil {
	*new { ^this.shouldNotImplement(thisMethod) }

	isNil { ^true }
	notNil { ^false }
	? { arg obj; ^obj }
	?? { arg obj; ^obj.value }
	!? {}
	asBoolean    { ^false }
	booleanValue { ^false } // TODO in the long-run, deprecate for asBoolean

	// support a nil Environment
	push { arg function; ^function.value }
	appendStream { arg stream; ^stream }
	pop {}

	// support a nil Plug
	source {}
	source_ {}

	// rate access support
	rate { ^nil }
	numChannels { ^nil }
	isPlaying { ^false }

	do {}
	reverseDo {}
	pairsDo {}
	collect {}
	select {}
	reject {}
	detect {}
	collectAs {}
	selectAs {}
	rejectAs {}
	collectInPlace {}
	collectCopy {}

	// dependancy operators are no-ops
	dependants {
		^IdentitySet.new
	}
	changed {}
	addDependant {}
	removeDependant {}
	release {}
	update {}

	// nil Event support
	transformEvent { arg event;
		^event
	}
	awake { arg inBeats, inSeconds, inClock;
		var temp;
		temp = inBeats; // prevent optimization
		^nil
	}
	play {}

	nextTimeOnGrid { arg clock; ^clock !? { clock.nextTimeOnGrid } }

	swapThisGroup {}

	performMsg {}

	printOn { arg stream;
		stream.putAll("nil");
	}
	storeOn { arg stream;
		stream.putAll("nil");
	}

	matchItem { ^true } // nil matches anything

	addAll { arg array; ^array.asArray }
	++ { arg array; ^array }
	remove {}

	// ControlView support
	set {}
	get { arg prevVal; ^prevVal }

	// FunctionList support
	removeFunc { ^this }

	replaceFunc { }

	// if Main-startup fails then AppClock scheduler may be nil. If that happens an
	// endless cascade of doesNotUnderstand messages gets printed in response to each clock tick
	// unless we do this.
	seconds_ {}

	// throwing Nil
	throw {}

	handleError { arg error;
		Error.handling = true;
		if (Error.debug) {
			{ error.inspect }.defer;
		} {
			error.reportError
		};
		Error.handling = false;
		this.halt;
	}

	archiveAsCompileString { ^true }

	superclassesDo {}
}
