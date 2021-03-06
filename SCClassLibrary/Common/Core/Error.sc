Exception {
	classvar <>handling = false;
	classvar <>debug = false;
	classvar <>inProtectedFunction = false;

	var <>what, <>protectedBacktrace, <>path;

	*new { arg what;
		var protectedBacktrace, instance;
		if (debug || inProtectedFunction, {
			protectedBacktrace = this.getBackTrace.caller;
			inProtectedFunction = false;
		});
		^super.newCopyArgs(what ? this.name, protectedBacktrace, thisProcess.nowExecutingPath);
	}
	errorString {
		^"EXCEPTION: " ++ what
	}
	reportError {
		this.errorString.postln;
		if(protectedBacktrace.notNil, { this.postProtectedBacktrace });
		this.dumpBackTrace;
		// this.adviceLink.postln;
		"^^ The preceding error dump is for %\n".postf(this.errorString);
	}
	adviceLink {
		^("For advice: [http://supercollider.sf.net/wiki/index.php/%]"
			.format(this.adviceLinkPage));
	}
	adviceLinkPage {
		^this.errorString.tr($ , $_).tr($\n, $_);
	}
	isException { ^true }
}

Error : Exception {
	errorString {
		^"ERROR: " ++ what
	}
	errorPathString {
		^if(path.isNil) { "" } { "PATH:" + path ++ "\n" }
	}
}

MethodError : Error {
	var <>receiver;

	*new { arg what, receiver;
		^super.new(what).receiver_(receiver)
	}
	reportError {
		this.errorString.postln;
		"RECEIVER:\n".post;
		receiver.dump;
		this.errorPathString.post;
		if(protectedBacktrace.notNil, { this.postProtectedBacktrace });
		this.dumpBackTrace;
		// this.adviceLink.postln;
		"^^ The preceding error dump is for %\nRECEIVER: %\n\n\n".postf(this.errorString, receiver);
	}
	adviceLinkPage {
		^this.class.name
	}

}

PrimitiveFailedError : MethodError {
	var <>failedPrimitiveName;

	*new { arg receiver;
		^super.new(Thread.primitiveErrorString, receiver)
			.failedPrimitiveName_(thisThread.failedPrimitiveName)
	}
	errorString {
		^"ERROR: Primitive '%' failed.\n%".format(failedPrimitiveName, what ? "")
	}
}

SubclassResponsibilityError : MethodError {
	var <>method, <>class;
	*new { arg receiver, method, class;
		^super.new(nil, receiver).method_(method).class_(class)
	}
	errorString {
		^"ERROR: '" ++ method.name ++ "' should have been implemented by "
			++ class.name ++ "."
	}
}

ShouldNotImplementError : MethodError {
	var <>method, <>class;
	*new { arg receiver, method, class;
		^super.new(nil, receiver).method_(method).class_(class)
	}
	errorString {
		^"ERROR: '" ++ method.ownerClass.name ++ "-" ++ method.name
			++ "' Message not valid for this subclass: "
			++ class.name ++ "."
	}
}

DoesNotUnderstandError : MethodError {
	var <>selector, <>args, <suggestedCorrection, suggestion = "";
	*new { arg receiver, selector, args;
		^super.new(nil, receiver).selector_(selector).args_(args).init
	}

	errorString {
		^"ERROR: Message '" ++ selector ++ "' not understood." ++ suggestion
	}
	reportError {
		this.errorString.postln;
		"RECEIVER:\n".post;
		receiver.dump;
		"ARGS:\n".post;
		args.dumpAll;
		this.errorPathString.post;
		if(protectedBacktrace.notNil, { this.postProtectedBacktrace });
		this.dumpBackTrace;
		// this.adviceLink.postln;
		"^^ %\nRECEIVER: %\n\n\n".postf(this.errorString, receiver);
	}
	adviceLinkPage {
		^"%#%".format(this.class.name, selector)
	}
}


MustBeBooleanError : MethodError {
	errorString {
		^"ERROR: Non Boolean in test."
	}
}

NotYetImplementedError : MethodError {
}

OutOfContextReturnError : MethodError {
	var <>method, <>result;
	*new { arg receiver, method, result;
		^super.new(nil, receiver).method_(method).result_(result)
	}
	errorString {
		^"ERROR: '" ++ method.ownerClass.name ++ "-" ++ method.name
			++ "' Out of context return of value: " ++ result
	}
}

ImmutableError : MethodError {
	var <>value;
	*new { arg receiver, value;
		^super.new(nil, receiver).value_(value)
	}
	errorString {
		^"ERROR: Object is immutable: " ++ receiver
	}
}

BinaryOpFailureError : DoesNotUnderstandError {
	errorString {
		^"ERROR: binary operator '" ++ selector ++ "' failed."
	}
}

DeprecatedError : MethodError {
	var <>method, <>class, <>alternateMethod;

	*new { arg receiver, method, alternateMethod, class;
		^super.new(nil).receiver_(receiver).method_(method).class_(class).alternateMethod_(alternateMethod)
	}
	errorString {
		var methodSignature = { arg m;
			m.ownerClass.name.asString  ++ ":" ++ m.name;
		};
		var searchForCaller = { arg backtrace, m;
			while {
				backtrace.notNil and: {
					backtrace.functionDef !== m
				}
			} {
				backtrace = backtrace.caller;
			};
			// backtrace.caller may now be a FunctionDef,
			// useless for troubleshooting
			// so roll back to the last real method
			while {
				backtrace.notNil and: {
					backtrace = backtrace.caller;
					backtrace.functionDef.isKindOf(Method).not
				}
			};
			if(backtrace.notNil) { backtrace.tryPerform(\functionDef) };
		};
		var caller, string;
		if(protectedBacktrace.notNil) {
			caller = searchForCaller.value(protectedBacktrace, method);
		};
		if(caller.isNil) {
			caller = searchForCaller.value(this.getBackTrace, method);
		};
		if(caller.isNil) {
			caller = "{unknown}"
		} {
			if(caller.isKindOf(Method)) {
				caller = methodSignature.value(caller);
			} {
				caller = caller.asString;
			};
		};
		string = "WARNING: Called from %, method % is deprecated and will be removed.".format(
			caller,
			methodSignature.value(method)
		);
		if(alternateMethod.notNil) {
			string = string + "Use" + methodSignature.value(alternateMethod) + "instead.";
		};

		string = string ++ "\nThe definition of '%' is to be found here: '%'".format(method, method.filenameSymbol);

		^string
	}

	reportError {
		this.errorString.postln;
		this.errorPathString.post;
		// this.adviceLink.postln;
		"\n\n".post;
	}

	throw {
		Error.handling = true;
		this.reportError;
		if (Error.debug) {
			if(protectedBacktrace.notNil, { this.postProtectedBacktrace });
			this.dumpBackTrace;
			Error.handling = false;
			this.halt;
		} {
			Error.handling = false;
		};

	}
	adviceLinkPage {
		^"DeprecatedError"
	}
}
