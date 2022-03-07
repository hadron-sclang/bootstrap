// you must not make any change at all to the order or number of
// instance variables in these classes!
// You should also not muck with the contents of the instance
// variables unless you are sure you know what you are doing.
// You may add methods.

Class {
	var <name, <nextclass, superclass, <subclasses;
	var <methods, <instVarNames, <classVarNames;
	var <iprototype, <cprototype;
	var <constNames, <constValues;
	var instanceFormat, instanceFlags;
	var <classIndex, classFlags, <maxSubclassIndex;
	var <filenameSymbol, <charPos, <classVarIndex;

	classvar <>classesInited;

	// Every class has a metaclass which has 'Meta_' prepended to the name.
	// Though there is a class Meta_Class which is the class of Class, the
	// class of Meta_Class is Class. It is a loop, but a different one
	// than the structure in Smalltalk.

	superclass {
		// superclass is stored as a symbol to allow forward reference during compilation
		^superclass.asClass
	}
	asClass { ^this }
	isMetaClass { ^this.class === Class }

	initClass {   }


	// call Class.initClassTree(SomeClass) to force a class to init if you depend on its resources
	*initClassTree { arg aClass;
		var implementsInitClass;
		// sometimes you need a class to be inited before another class
		// start the process: Class.initClassTree(Object)
		if(classesInited.isNil, { classesInited = IdentitySet.new });
		if(classesInited.includes(aClass).not, {
			classesInited.add(aClass);

			if(aClass.isMetaClass.not and: { aClass.class.findMethod(\initClass).notNil }, {
					aClass.initClass;
			});

			if(aClass.subclasses.notNil,{
				aClass.subclasses.do({ arg class; this.initClassTree(class); });
			});
		});
	}

	*allClasses {
		_AllClasses
		^this.primitiveFailed
	}
	findMethod { arg methodName;
		if ( methods.notNil, {
			^methods.detect({ arg method; method.name == methodName });
		},{ ^nil });
	}
	findRespondingMethodFor { arg methodName;
		this.superclassesDo { arg class;
			var method = class.findMethod(methodName);
			method !? { ^method };
		};
		^nil
	}
	findOverriddenMethod { arg methodName;
		if(this.findMethod(methodName).isNil) { ^nil };
		this.superclass.superclassesDo { arg class;
			var method = class.findMethod(methodName);
			if(method.notNil) { ^method }
		};
		^nil
	}
	superclassesDo { arg function;
		var class = this;
		while { class.notNil } {
			function.value(class);
			class = class.superclass;
		}
	}

	dumpClassSubtree {
		 _DumpClassSubtree
		^this.primitiveFailed
	}
	dumpInterface {
		// show all methods and their arguments defined for this class
		// does not include methods defined in superclasses
		this.methods.do({ arg meth;
			var numargs;
			numargs = meth.argNames.size - 1;
			"   ".post;
			meth.name.post;
			" ( ".post;
			meth.argNames.do({ arg name, i;
				if (i > 0, { // skip 'this'
					name.post;
					if (i < numargs, {
						", ".post;
					});
				});
			});
			" )\n".post;
		});
	}

	asString {
		^name.asString
	}
	printOn { arg stream;
		stream << "class " << name;
	}
	storeOn { arg stream;
		stream << name;
	}
	archiveAsCompileString { ^true }

	hasHelpFile {
		//should cache this in Library or classvar
		//can't add instance variables to Class
		^this.name.asString.findHelpFile.notNil
	}
	helpFilePath {
		^this.name.asString.findHelpFile
	}
	help {
		this.openHelpFile
	}
	openHelpFile {
		// NOTE: because wslib provided the shortcut "Object:*help --> Object:*openHelpFile", we do the same
		// rather than moving the implementation to the future-compatible :help method.
		// This prevents infinite recursions for people with wslib installed.
		// In future (3.7) this method content should be moved to :help, but no sooner.
		this.name.asString.help
	}

	shallowCopy { ^this }
	//listInstances { _ListInstancesOf }
	//traceAnyPathToAllInstancesOf { _TraceAnyPathToAllInstancesOf }

	openCodeFile {
		this.filenameSymbol.asString.openDocument(this.charPos, -1);
	}
	findReferences { arg aSymbol, references;
		methods.do({ arg meth;
			references = meth.findReferences(aSymbol, references)
		});
		^references
	}
	*findAllReferences { arg aSymbol;
		// this will not find method calls that are compiled with special byte codes such as 'value'.
		var references;
		Class.allClasses.do({ arg class;
			references = class.findReferences(aSymbol, references);
		});
		^references;
	}
	allSubclasses {
		var list;
		list = subclasses.copy;
		subclasses.do({ arg class; list = list ++ class.allSubclasses; });
		^list
	}
	superclasses {
		var list;
		this.superclass.superclassesDo { arg class; list = list.add(class) }
		^list
	}

}

Process {
	// A Process is a runtime environment.
	var classVars, <interpreter;
	var curThread, <mainThread;
	var schedulerQueue;
	var <>nowExecutingPath;

	startup {
	}
	run {
		// This method is called when 'Run Main' is chosen from the menu.
		// Override in class 'Main' to do whatever you want.
	}
	stop {
		// This method is called when 'Stop Main' is chosen from the menu.
		// Override in class 'Main' to do whatever you want.
	}
	shutdown {
	}

	*tailCallOptimize {
		_GetTailCallOptimize
		^this.primitiveFailed
	}
	*tailCallOptimize_ { arg bool;
		_SetTailCallOptimize
		^this.primitiveFailed
	}

	getCurrentSelection {
		var qt = \QtGUI.asClass;
		^if(qt.notNil and: {qt.focusView.notNil}) {
			qt.selectedText;
		} {
			interpreter.cmdLine;
		}
	}

	interpretCmdLine {
		// interpret some text from the command line
		interpreter.interpretCmdLine;
	}

	interpretPrintCmdLine {
		// interpret some text from the command line and print result
		interpreter.interpretPrintCmdLine;
	}

	interpretPrintSelectedText {
		interpreter.cmdLine = this.getCurrentSelection;
		interpreter.interpretPrintCmdLine;
	}

	showHelp {
		this.getCurrentSelection.help
	}

	shallowCopy { ^this }

	*elapsedTime {
		_ElapsedTime
		^this.primitiveFailed
	}

	*monotonicClockTime {
		_monotonicClockTime
		^this.primitiveFailed
	}

	storeOn { arg stream;
		stream << "thisProcess";
	}
	archiveAsCompileString { ^true }

	prSchedulerQueue { ^schedulerQueue }
}


FunctionDef {
	var raw1, raw2, <code, <selectors, <constants, <prototypeFrame, <context, <argNames, <varNames;
	var <sourceCode;

	// a FunctionDef is defined by a code within curly braces {}
	// When you use a FunctionDef in your code it gets pushed on the stack
	// as an instance of Function

	dumpByteCodes {
		_DumpByteCodes
		^this.primitiveFailed
	}

	numArgs {
		// return number of arguments to the function
		_FunDef_NumArgs
		^this.primitiveFailed
	}
	numVars {
		// return number of variables in the function
		_FunDef_NumVars
		^this.primitiveFailed
	}
	varArgs {
		// return boolean whether function has ellipsis argument
		_FunDef_VarArgs
		^this.primitiveFailed
	}
	shallowCopy { ^this }

	asFunction {
		// this is only legal for closed functions.
		_FunctionDefAsFunction
		^this.primitiveFailed
	}

	dumpContexts {
		_FunctionDefDumpContexts
		^this.primitiveFailed
	}

	findReferences { arg aSymbol, references;
		var lits;
		lits = selectors.asArray;
		if (lits.includes(aSymbol), {
			references = references.add(this);
		});
		lits.do({ arg item;
			if (item.isKindOf(FunctionDef), {
				references = item.findReferences(aSymbol, references)
			})
		});
		^references
	}
	storeOn { arg stream;
		stream << "nil"
	}
	checkCanArchive { "cannot archive FunctionDefs".warn }
	archiveAsCompileString { ^true }

	argumentString { arg withDefaultValues=true;
		var res = "", pairs = this.keyValuePairsFromArgs;
		var last, noVarArgs;
		if(pairs.isEmpty) { ^nil };
		last = pairs.lastIndex;
		noVarArgs = this.varArgs.not;
		pairs.pairsDo { |name, defaultValue, i|
			var value, notLast;
			notLast = i + 1 != last;
			if(notLast or: noVarArgs) {
				res = res ++ name;
				if(withDefaultValues and: { defaultValue.notNil }) {
					res = res ++ " = " ++ defaultValue.asCompileString;
				};
			} {
				res = res ++ "... " ++ name;
			};
			if(notLast) { res = res ++ ", " };
		}
		^res
	}
}

Method : FunctionDef {
	var <ownerClass, <name, <primitiveName;
	var <filenameSymbol, <charPos;

	openCodeFile {
		this.filenameSymbol.asString.openDocument(this.charPos, -1);
	}
	hasHelpFile {
		//should cache this in Library or classvar
		//can't add instance variables to Class
		^this.name.asString.findHelpFile.notNil
	}
	storeOn { arg stream;
		stream << ownerClass.name << ".findMethod(" << name.asCompileString << ")"
	}
	archiveAsObject { ^true }
	checkCanArchive {}
	findReferences { arg aSymbol, references;
		var lits, functionRefs;
		lits = selectors.asArray;
		if (lits.includes(aSymbol), {
			references = references.add(this);
			^references // we only need to be listed once
		});
		lits.do({ arg item;
			if (item.isKindOf(FunctionDef), {
				functionRefs = item.findReferences(aSymbol, functionRefs);
			})
		});
		functionRefs.notNil.if({references = references.add(this)});
		^references
	}
}

Frame {
	// frames contain the local variables, context and continuation of a function or method invocation.
	// since some Frames are deleted instead of garbage collected, it is too
	// dangerous to allow access to them. Dangling pointers could result.
	shallowCopy { ^this }

	storeOn { arg stream; stream << "nil"; }
	archiveAsCompileString { ^true }
	checkCanArchive { "cannot archive Frames".warn }
}

DebugFrame {
	var <functionDef, <args, <vars, <caller, <context, <address;
	// Object.getBackTrace returns one of these.
	// 'functionDef' is the FunctionDef for this function or method.
	// 'args' the values of the arguments to the function call.
	// 'vars' the values of the local variables.
	// 'caller' points to another DebugFrame for the caller to this function.
	// 'context' points to another DebugFrame for the frame lexically enclosing this one.
	// 'address' memory address of the actual frame object.
	asString { ^"DebugFrame of " ++ functionDef.asString }
}

RawPointer {
	// class used to hold raw pointers from the
	// host environment.
}

Interpreter {
	// The interpreter defines a context in which interactive commands
	// are compiled.

	var <>cmdLine; // place holder for text executed from a worksheet
	var context; // faked interpreter context frame. Don't mess with it.

	// a-z are predefined variables for use by the interactive context.
	// They are read+write so that programmatic methods can
	// get and alter the values that the interpreter has access to.
	var <>a, <>b, <>c, <>d, <>e, <>f, <>g, <>h, <>i, <>j;
	var <>k, <>l, <>m, <>n, <>o, <>p, <>q, <>r, <>s, <>t;
	var <>u, <>v, <>w, <>x, <>y, <>z;

	var <>codeDump, <>preProcessor;

	*new { ^this.shouldNotImplement(thisMethod) }

	interpretCmdLine {
		^this.compile(cmdLine).value;
	}

	interpret { arg string ... args;
		// compile, evaluate
		cmdLine = string;
		^this.compile(string).valueArray(args);
	}
	interpretPrint { arg string ... args;
		// compile, evaluate, print
		cmdLine = string;
		^this.compile(string).valueArray(args).postln;
	}
	compile { arg string;
		_CompileExpression
		// compiles string and returns a Function.
		// the string must be a valid expression.
		// You cannot compile a class definition this way.
		// This method is not implemented in SCPlay.
		^nil
	}

	clearAll {
		a = b = c = d = e = f = g = h = i = j = k = l = m =
		n = o = p = q = r = s = t = u = v = w = x = y = z = nil;
	}

	// PRIVATE
	functionCompileContext {
		// compiler uses this method as a fake context in which to compile
		// the user's function.
		// Do not edit this method!

		{}	// this forces the compiler to generate a heap allocated frame rather than
			// a frame on the stack
	}
	shallowCopy { ^this }
}
