Function : AbstractFunction {
	var <def, context;
	// a Function is what you get when you write a FunctionDef in your code.
	// it consists of the function's code and the variables in its defining context

	*new { ^this.shouldNotImplement(thisMethod) }

	isFunction { ^true }
	isClosed { ^def.sourceCode.notNil }


	archiveAsCompileString { ^true }
	archiveAsObject { ^true }
	checkCanArchive { if (def.sourceCode.isNil) { "cannot archive open Functions".warn } }
	storeOn { arg stream;
		var args;
		stream << (def.sourceCode ?? {
			args = def.argumentString;
			"{ %\"open Function\" }".format(if(args.notNil) { "| % | ".format(args) } { "" })
		})
	}

	shallowCopy { ^this }

	choose { ^this.value }

	// evaluation
	value { arg ... args;
		_FunctionValue
		// evaluate a function with args
		^this.primitiveFailed
	}
	valueArray { arg ... args;
		_FunctionValueArray
		// evaluate a function, if the last argument is an array it will be
		// expanded into separate args.
		^this.primitiveFailed
	}

	valueEnvir { arg ... args;
		_FunctionValueEnvir
		// evaluate a function with args.
		// unsupplied argument names are looked up in the currentEnvironment
		^this.primitiveFailed
	}
	valueArrayEnvir { arg ... args;
		_FunctionValueArrayEnvir
		// evaluate a function, if the last argument is an array it will be
		// expanded into separate args.
		// unsupplied argument names are looked up in the currentEnvironment
		^this.primitiveFailed
	}
	functionPerformList { arg selector, arglist;
		_ObjectPerformList;
		^this.primitiveFailed
	}

	valueWithEnvir { arg envir;
		var prototypeFrame;
		if(envir.isNil) { ^this.value };
		prototypeFrame = def.prototypeFrame.copy;

		def.argNames.do { |name,i|
			var val = envir[name];
			val !? { prototypeFrame[i] = val };
		};
		//	postf("argNames: % prototypeFrame: %\n", def.argNames, prototypeFrame);

		// evaluate a function, using arguments from the supplied environment
		// slightly faster than valueEnvir and does not replace the currentEnvironment
		^this.valueArray(prototypeFrame)
	}

	performWithEnvir { |selector, envir|
		if(selector === \value) { ^this.valueWithEnvir(envir) };
		^super.performWithEnvir(selector, envir)
	}

	performKeyValuePairs { |selector, pairs|
		var envir;
		if(selector !== \value) {
			^this.superPerform(\performKeyValuePairs, pairs)
		};

		envir = this.def.makeEnvirFromArgs;
		envir.putPairs(pairs);

		^this.valueWithEnvir(envir)
	}

	numArgs { ^def.numArgs }		// return number of arguments to the function
	numVars { ^def.numVars }		// return number of variables in the function
	varArgs { ^def.varArgs }		// return boolean whether function has ellipsis argument

	loop {
		// loop is supported magically by the compiler,
		// thus it can be implemented in terms of itself
		loop { this.value };
	}

	block {
		^this.value {|val| ^val };
	}
	//	block {
	//		var result;
	//		try {
	//			result = this.value #{|val| Break(val).throw };
	//		}{|error|
	//			if (error.class == Break) {
	//				^error.value
	//			}{
	//				error.throw
	//			}
	//		}
	//		^result
	//	}

	asRoutine {
		^Routine.new(this)
	}

	dup { arg n = 2;
		^Array.fill(n, this)
	}
	sum { arg n = 2;
		var sum = 0;
		n.do {|i| sum = sum + this.value(i) };
		^sum
	}

	thunk { ^Thunk(this) }

	// Pattern support
	transformEvent { arg event;
		^this.value(event)
	}

	// ControlView support
	set { arg ... args; ^this.valueArray(args) }
	get { arg prevVal; ^prevVal }

	fork { arg clock, quant, stackSize;
		^Routine(this, stackSize).play(clock, quant);
	}

	forkIfNeeded { arg clock, quant, stackSize;
		if(thisThread.isKindOf(Routine), this, { ^this.fork(clock, quant, stackSize) });
		^thisThread;
	}


	awake { arg beats, seconds, clock;
		var time = seconds; // prevent optimization
		^this.value(beats, seconds, clock)
	}

	cmdPeriod { this.value }

	protect { arg handler;
		var result;
		result = this.prTry;
		if (result.isException) {
			handler.value(result);
			result.throw;
		}{
			handler.value; // argument should be nil if there was no exception.
			^result
		};
	}

	try { arg handler;
		var result = this.prTry;
		if (result.isException) { ^handler.value(result); }
		{ ^result }
	}
	prTry {
		var result, thread = thisThread;
		var next = thread.exceptionHandler,
		wasInProtectedFunc = Exception.inProtectedFunction;
		thread.exceptionHandler = {|error|
			thread.exceptionHandler = next; // pop
			^error
		};
		Exception.inProtectedFunction = true;
		result = this.value;
		Exception.inProtectedFunction = wasInProtectedFunc;
		thread.exceptionHandler = next; // pop
		^result
	}

	handleError { arg error; ^this.value(error) }

	r { ^Routine(this) }

	matchItem { arg item;
		^this.value(item)
	}

	// scale suppoert

	performDegreeToKey { arg scaleDegree, stepsPerOctave = 12, accidental = 0;
		^this.value(scaleDegree, stepsPerOctave, accidental)
	}

	// attach the function to a specific environment
	inEnvir { |envir|
		envir ?? { envir = currentEnvironment };
		^{ |... args| envir.use({ this.valueArray(args) }) }
	}

	loadToFloatArray { |duration = 0.01, target, action|
		this.asBuffer(duration, target, { |buffer|
			buffer.loadToFloatArray(action: { |array|
				action.value(array, buffer);
				buffer.free
			})
		})
	}

	getToFloatArray { |duration = 0.01, target, action, wait = 0.01, timeout = 3|
		this.asBuffer(duration, target, { |buffer|
			buffer.getToFloatArray(0, wait: wait, action: { |array|
				action.value(array, buffer);
				buffer.free
			})
		})
	}

}

Thunk : AbstractFunction {
	// a thunk is an unevaluated value.
	// it gets evaluated once and then always returns that value.
	// also known as a "promise" in Scheme.
	// thunks have no arguments.
	var function, value;

	*new { arg function;
		^super.newCopyArgs(function)
	}
	value {
		^value ?? { value = function.value; function = nil; value }
	}
	valueArray { ^this.value }
	valueEnvir { ^this.value }
	valueArrayEnvir { ^this.value }
}

