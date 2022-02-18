Float : SimpleNumber {
	isFloat { ^true }
	asFloat { ^this }

	+ { arg aNumber, adverb; _AddFloat; ^aNumber.performBinaryOpOnSimpleNumber('+', this, adverb) }
	- { arg aNumber, adverb; _SubFloat; ^aNumber.performBinaryOpOnSimpleNumber('-', this, adverb) }
	* { arg aNumber, adverb; _MulFloat; ^aNumber.performBinaryOpOnSimpleNumber('*', this, adverb) }

	clip { arg lo, hi; _ClipFloat; ^this.primitiveFailed }
	wrap { arg lo, hi; _WrapFloat; ^this.primitiveFailed }
	fold { arg lo, hi; _FoldFloat; ^this.primitiveFailed }

	coin { ^1.0.rand < this }
	xrand2 { ^this.rand2 }

	// returns an Integer which is the bit pattern of this as a
	// 32bit single precision float
	as32Bits {
		_As32Bits
		^this.primitiveFailed
	}

	// returns an Integer which is the bit pattern of high
	// 32 bits of the 64 bit double precision floating point value
	high32Bits {
		_High32Bits
		^this.primitiveFailed
	}
	low32Bits {
		_Low32Bits
		^this.primitiveFailed
	}

	*from32Bits { arg word;
		_From32Bits
		^this.primitiveFailed
	}
	*from64Bits { arg hiWord, loWord;
		_From64Bits
		^this.primitiveFailed
	}
}
