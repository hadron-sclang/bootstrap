ArrayedCollection : SequenceableCollection {
	*newClear { arg indexedSize = 0;
		_BasicNewClear
		^this.primitiveFailed
		// creates a new instance with indexedSize indexable slots.
		// the slots are filled with nil, zero or something else
		// appropriate to the type of indexable slots in the
		// object.
	}

	// ArrayedCollections are vectors which have a
	// fixed maximum capacity.

	indexedSize {
		_BasicSize
		^this.primitiveFailed
	}
	size {
		_BasicSize
		^this.primitiveFailed
	}
	maxSize {
		_BasicMaxSize
		^this.primitiveFailed
	}

	swap { arg i, j; var temp;
		_BasicSwap;
		^this.primitiveFailed;
	}

	at { arg index;
		_BasicAt;
		^this.primitiveFailed;
	}
	clipAt { arg index;
		_BasicClipAt;
		^this.primitiveFailed;
	}
	wrapAt { arg index;
		_BasicWrapAt;
		^this.primitiveFailed;
	}
	foldAt { arg index;
		_BasicFoldAt;
		^this.primitiveFailed;
	}
	put { arg index, item;
		_BasicPut;
		^this.primitiveFailed;
	}
	clipPut { arg index, item;
		_BasicClipPut;
		^this.primitiveFailed;
	}
	wrapPut { arg index, item;
		_BasicWrapPut;
		^this.primitiveFailed;
	}
	foldPut { arg index, item;
		_BasicFoldPut;
		^this.primitiveFailed;
	}
	removeAt { arg index;
		_BasicRemoveAt;
		^this.primitiveFailed;
	}
	takeAt { arg index;
		_BasicTakeAt;
		^this.primitiveFailed;
	}

	indexOf { arg item;
		_ArrayIndexOf
		^this.primitiveFailed;
	}
	indexOfGreaterThan { arg val;
		_ArrayIndexOfGreaterThan
		^super.indexOfGreaterThan(val)
	}

	takeThese { arg func;
		var i = 0;
		while { i < this.size }
		{
			if (func.value(this[i], i)) {
				this.takeAt(i)
			}{
				i = i + 1
			}
		}
	}

	// see counterparts to these in Object
	slotSize {
		^this.size;
	}
	slotAt { arg index;
		^this.at(index);
	}
	slotPut { arg index, value;
		^this.put(index, value);
	}
	slotKey { arg index;
		^index
	}
	slotIndex { arg key;
		^nil
	}
	getSlots {
		^this.copy
	}
	setSlots { arg array;
		this.overWrite(array)
	}

	atModify { arg index, function; this.put(index, function.value(this.at(index), index)) }
	atInc { arg index, inc=1; this.put(index, this.at(index)+inc); }
	atDec { arg index, dec=1; this.put(index, this.at(index)-dec); }

	isArray { ^true }
	asArray { ^this }

	copyRange { arg start, end;
		// copies the fixed part of an object and the indexed slots
		// from start to end.
		_ObjectCopyRange;
		^this.primitiveFailed
	}
	copySeries { arg first, second, last;
		// copies elements from first to last, stepping by (second-first)
		// copySeries(3,5,11) copies elements 3,5,7,9,11
		// if second is nil then the step is 1.
		// copySeries(3,nil,11) copies elements 3,4,5,6,7,8,9,10,11
		_ObjectCopySeries;
		^this.primitiveFailed
	}
	putSeries { arg first, second, last, value;
		// puts value into array at indices defined by the series. see copySeries.
		_ArrayPutSeries;
		^this.primitiveFailed
	}

	add { arg item;
		// add item to end of array.
		// if the capacity is exceeded, this returns a new
		// ArrayedCollection.
		_ArrayAdd
		^this.primitiveFailed;
	}
	addAll { arg aCollection;
		var array;
		_ArrayAddAll
		array = this;
		aCollection.asCollection.do({ arg item; array = array.add(item) }) ;
		^array
	}
	putEach { arg keys, values;
		_ArrayPutEach
		^super.putEach(keys, values)
	}
	extend { arg size, item;
		_ArrayExtend
		^this.primitiveFailed
	}
	insert { arg index, item;
		// add item at specified index.
		// if the capacity is exceeded, this returns a new
		// ArrayedCollection.
		_ArrayInsert
		^this.primitiveFailed;
	}
	move { arg fromIndex, toIndex;
		^this.insert(toIndex, this.removeAt(fromIndex))
	}
	addFirst { arg item; ^this.insert(0, item) }
	addIfNotNil { arg item; if(item.notNil,{ ^this.add(item) }) }
	pop {
		// remove and return last item in array
		_ArrayPop
		^nil;
	}
	++ { arg anArray;
		// concatenate two arrays of the same type
		// this primitive will handle all array element types
		_ArrayCat;
		// primitive fails if arrays are different types
		^super ++ anArray
	}
	overWrite { arg aCollection, pos=0;
		var array, grow;
		_ArrayOverwrite
		^this.primitiveFailed
		//array = this.growClear(pos + aCollection.size - this.size);
		//grow = pos + aCollection.size - this.size;
		//if (grow > 0, { array = this ++ this.class.newClear(grow); },{ array = this });
		//aCollection.do({ arg item, i; array.put(pos + i, item); });
		//^array
	}
	grow { arg sizeIncrease;
		// returns an array of sufficient capacity.
		// may return same object if it still has enough space or if sizeIncrease <= 0.
		_ArrayGrow
		^this.primitiveFailed
	}
	growClear { arg sizeIncrease;
		// returns an array of sufficient capacity.
		// may return same object if it still has enough space or if sizeIncrease <= 0.
		// clears new space
		_ArrayGrowClear
		^this.primitiveFailed
	}
	seriesFill { arg start, step;
		this.size.do({ arg i;
			this.put(i, start);
			start = start + step;
		});
	}

	fill { arg value;
		_ArrayFill
		^this.primitiveFailed
		/* replaced by primitive
		var i = 0, size;
		size = this.size;
		while ({ i < size }, {
			this.put(i, val);
			i = i + 1;
		});
		*/
	}
	do { arg function;
		// special byte codes inserted by compiler for this method
		var i=0;
		while ({ i < this.size }, {
			function.value(this.at(i), i);
			i = i + 1;
		})
	}
	reverseDo { arg function;
		// special byte codes inserted by compiler for this method
		var i=0, j=0;
		i = this.size - 1;
		while ({ i >= 0 },{
			function.value(this.at(i), j);
			i = i - 1;
			j = j + 1;
		})
	}
	reverse {
		var i = 0;
		var res = this.copy;
		var size1 = res.size - 1;
		var halfsize = res.size div: 2;
		halfsize.do({ arg i;
			res.swap(i, size1 - i);
		});
		^res
	}

	windex {
		_ArrayWIndex
		^this.primitiveFailed
		//		var r, sum = 0.0, index;
		//		r = 1.0.rand;
		//		this.detect({ arg weight, i;
		//			sum = sum + weight;
		//			if (sum >= r, {
		//				index = i;
		//				true;
		//			},{ false });
		//		});
		//		^index;
	}
	normalizeSum {
		_ArrayNormalizeSum
		^(this * this.sum.reciprocal)
	}
	normalize { arg min=0.0, max=1.0;
		var minItem = this.minItem;
		var maxItem = this.maxItem;
		^this.collect { |el| el.linlin(minItem, maxItem, min, max) };
	}

	asciiPlot {
		// draw the waveform down the page as asterisks
		var lo = this.minItem;
		var hi = this.maxItem;
		var scale = 80 / (hi - lo);
		this.size.do { |i|
			var pt = ((this[i] - lo) * scale).asInteger;
			pt.do({ " ".post; });
			"*\n".post;
		};
	}

	performInPlace { arg selector, from, to, argList;
		^this.overWrite(this.copyRange(from, to).performList(selector, argList), from)
	}

	clipExtend { arg length;
		^this.extend(length, this.last)
	}

	// concepts borrowed from J programming language
	rank {
		// rank is the number of dimensions in a multidimensional array.
		// see also Object-rank
		// this assumes every element has the same rank
		^1 + this.first.rank
	}
	reshapeLike { arg another, indexing=\wrapAt;
		var index = 0;
		var flat = this.flat;
		^another.deepCollect(0x7FFFFFFF) {
			var item = flat.perform(indexing, index);
			index = index + 1;
			item;
		};
	}
	deepCollect { arg depth = 1, function, index = 0, rank = 0;
		if(depth.isNil) {
			rank = rank + 1;
			^this.collect { |item, i| item.deepCollect(depth, function, i, rank) }
		};
		if (depth <= 0) {
			^function.value(this, index, rank)
		};
		depth = depth - 1;
		rank = rank + 1;
		^this.collect { |item, i| item.deepCollect(depth, function, i, rank) }
	}
	deepDo { arg depth = 1, function, index = 0, rank = 0;
		if(depth.isNil) {
			rank = rank + 1;
			^this.do { |item, i| item.deepDo(depth, function, i, rank) }
		};
		if (depth <= 0) {
			function.value(this, index, rank);
			^this
		};
		depth = depth - 1;
		rank = rank + 1;
		^this.do { |item, i| item.deepDo(depth, function, i, rank) }
	}
	unbubble { arg depth=0, levels=1;
		if (depth <= 0) {
			// converts a size 1 array to the item.
			if (this.size > 1) { ^this };
			if (levels <= 1) { ^this[0] }
			^this[0].unbubble(depth, levels-1)
		};
		^this.collect {|item| item.unbubble(depth-1) }
	}

	// random distribution table

	asRandomTable { arg size;
		var a=this, b;
		if(size.isNil) { size = this.size } { a = a.resamp1(size) };
		a = a.integrate; // incrementally integrate
		a = a.normalize(0, size-1); // normalize and scale by max index
		b = Array.fill(size, { arg i; a.indexInBetween(i) });  // flip array
		b = b / size // rescale to 0..1
		^b
	}

	tableRand {
		^this.blendAt((this.size - 1).asFloat.rand)
	}

	// osc bundle support

	msgSize {
		_NetAddr_MsgSize;
		^this.primitiveFailed
	}

	prBundleSize {
		_NetAddr_BundleSize;
		^this.primitiveFailed
	}
	includes { |item| ^this.indexOf(item).notNil }
}

RawArray : ArrayedCollection {
	// species { ^this.class }
	archiveAsCompileString { ^true }
	archiveAsObject { ^true }

	rate { ^\scalar }

	readFromStream { |stream, method|
		if(method.notNil) {
			this.size.do({ |i|
				this.put(i, stream.perform(method));
			})
		} {
			this.shouldNotImplement(thisMethod);
		}
	}

	powerset {
		^this.as(Array).powerset
	}
}

Int8Array[int8] : RawArray {
	unarchive {
		_Unarchive
		^this.primitiveFailed
	}

	readFromStream { |stream|
		super.readFromStream(stream, \getInt8);
	}
}

Int16Array[int16] : RawArray {
	readFromStream { |stream|
		super.readFromStream(stream, \getInt16);
	}
}

Int32Array[int32] : RawArray {
	readFromStream { |stream|
		super.readFromStream(stream, \getInt32);
	}
}

FloatArray[float] : RawArray {
	readFromStream { |stream|
		super.readFromStream(stream, \getFloat);
	}
}

DoubleArray[double] : RawArray {
	readFromStream { |stream|
		super.readFromStream(stream, \getDouble);
	}
}

	// readFromStream not implemented yet
SymbolArray[symbol] : RawArray {
}
