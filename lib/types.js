//////////////////////////////////////////////////////////////////////
// helper functions

//var jsnums = require('./js-numbers');


var types = {};


(function () {

//////////////////////////////////////////////////////////////////////


var appendChild = function(parent, child) {
    parent.appendChild(child);
};


// Inheritance from pg 168: Javascript, the Definitive Guide.
var heir = function(p) {
    var f = function() {}
    f.prototype = p;
    return new f();
};



//////////////////////////////////////////////////////////////////////



var _eqHashCodeCounter = 0;
makeEqHashCode = function() {
    _eqHashCodeCounter++;
    return _eqHashCodeCounter;
};

    
// getHashCode: any -> (or fixnum string)
// Produces a hashcode appropriate for eq.
getEqHashCode = function(x) {
    if (x && !x._eqHashCode) {
	x._eqHashCode = makeEqHashCode();
    }
    if (x && x._eqHashCode) {
	return x._eqHashCode;
    }
    if (typeof(x) == 'string') {
	return x;
    }
    return 0;
};


// Union/find for circular equality testing.

var UnionFind = function() {
	// this.parenMap holds the arrows from an arbitrary pointer
	// to its parent.
	this.parentMap = makeLowLevelEqHash();
}

// find: ptr -> UnionFindNode
// Returns the representative for this ptr.
UnionFind.prototype.find = function(ptr) {
	var parent = (this.parentMap.containsKey(ptr) ? 
		      this.parentMap.get(ptr) : ptr);
	if (parent === ptr) {
	    return parent;
	} else {
	    var rep = this.find(parent);
	    // Path compression:
	    this.parentMap.put(ptr, rep);
	    return rep;
	}
};

// merge: ptr ptr -> void
// Merge the representative nodes for ptr1 and ptr2.
UnionFind.prototype.merge = function(ptr1, ptr2) {
	this.parentMap.put(this.find(ptr1), this.find(ptr2));
};



//////////////////////////////////////////////////////////////////////
 


//////////////////////////////////////////////////////////////////////


var makeStructureType = function(name, fields) {   
    var aStruct = function(args) {
	Struct.call(this, name, args);
    };

    aStruct.prototype = heir(Struct.prototype);

    var type, constructor, predicate, accessor, mutator;

    // DO SOMETHING HERE
    return { 
	constructor: function(args) { return new aStruct(args); },
	predicate: function(x) { return x instanceof aStruct; }, 
	accessor: function(x, i) { return x._fields[i]; },
	mutator: function(x, i, v) {x._fields[i] = v} };
};


// Structures.
var Struct = function (constructorName, fields) {
    this._constructorName = constructorName; 
    this._fields = fields;
};

Struct = Struct;

Struct.prototype.toWrittenString = function(cache) { 
    //    cache.put(this, true);
    var buffer = [];
    buffer.push("(");
    buffer.push(this._constructorName);
    for(var i = 0; i < this._fields.length; i++) {
	buffer.push(" ");
	buffer.push(toWrittenString(this._fields[i], cache));
    }
    buffer.push(")");
    return String.makeInstance(buffer.join(""));
};


Struct.prototype.toDisplayedString = Struct.prototype.toWrittenString;

Struct.prototype.toDomNode = function(cache) {
    //    cache.put(this, true);
    var node = document.createElement("div");
    node.appendChild(document.createTextNode("("));
    node.appendChild(document.createTextNode(this._constructorName));
    for(var i = 0; i < this._fields.length; i++) {
	node.appendChild(document.createTextNode(" "));
	appendChild(node, toDomNode(this._fields[i], cache));
    }
    node.appendChild(document.createTextNode(")"));
    return node;
}


    Struct.prototype.isEqual = function(other, aUnionFind) {
	if (typeof(other) != 'object') {
	    return false;
	}
	if (! other._constructorName) {
	    return false;
	}
	if (other._constructorName != this._constructorName) {
	    return false;
	}
	if (typeof(other._fields) === 'undefined') {
	    return false;
	}
	if (this._fields.length != other._fields.length) {
	    return false;
	}
	for (var i = 0; i < this._fields.length; i++) {
	    if (! isEqual(this._fields[i],
			  other._fields[i],
			  aUnionFind)) {
		return false;
	    }
	}
	return true;
    };



//////////////////////////////////////////////////////////////////////

// Regular expressions.

var RegularExpression = function(pattern) {
    this.pattern = pattern;
};


var ByteRegularExpression = function(pattern) {
    this.pattern = pattern;
};




//////////////////////////////////////////////////////////////////////

// Paths

var Path = function(p) {
    this.path = p;
};


//////////////////////////////////////////////////////////////////////

// Bytes

var Bytes = function(bts) {
    this.bytes = bts;
};


//////////////////////////////////////////////////////////////////////
// Boxes
    
var Box = function(x) { 
    Struct.call(this, "box", [x]);
};

Box.prototype = heir(Struct.prototype);

Box.prototype.unbox = function() {
    return this._fields[0];
};

Box.prototype.set = function(newVal) {
    this._fields[0] = newVal;
};

Box.prototype.toString = function() {
    return "#&" + this._fields[0].toString();
}
//////////////////////////////////////////////////////////////////////








// We are reusing the built-in Javascript boolean class here.
Logic = {
    TRUE : true,
    FALSE : false
};

// WARNING
// WARNING: we are extending the built-in Javascript boolean class here!
// WARNING
Boolean.prototype.toWrittenString = function(cache) {
    if (this.valueOf()) { return "true"; }
    return "false";
};
Boolean.prototype.toDisplayedString = Boolean.prototype.toWrittenString;

Boolean.prototype.toString = function() { return this.valueOf() ? "true" : "false"; };

Boolean.prototype.isEqual = function(other, aUnionFind){
    return this == other;
};




// Chars
// Char: string -> Char
Char = function(val){
    this.val = val;
};
    
Char.makeInstance = function(val){
    return new Char(val);
};

Char.prototype.toWrittenString = function(cache) {
    return "#\\" + this.val;
};

Char.prototype.toDisplayedString = function (cache) {
    return this.val;
};

Char.prototype.toString = function() { return '#\\'+this.val; }

Char.prototype.getValue = function() {
    return this.val;
};

Char.prototype.isEqual = function(other, aUnionFind){
    return other instanceof Char && this.val == other.val;
};

//////////////////////////////////////////////////////////////////////
    
// Symbols

//////////////////////////////////////////////////////////////////////
var Symbol = function(val) {
    this.val = val;
};

var symbolCache = {};
    
// makeInstance: string -> Symbol.
Symbol.makeInstance = function(val) {
    // To ensure that we can eq? symbols with equal values.
    if (!(val in symbolCache)) {
	symbolCache[val] = new Symbol(val);
    } else {
    }
    return symbolCache[val];
};
    
Symbol.prototype.isEqual = function(other, aUnionFind) {
    return other instanceof Symbol &&
    this.val == other.val;
};
    

Symbol.prototype.toString = function() {
    return this.val;
};

Symbol.prototype.toWrittenString = function(cache) {
    return this.val;
};

Symbol.prototype.toDisplayedString = function(cache) {
    return this.val;
};

//////////////////////////////////////////////////////////////////////

// Keywords

var Keyword = function(val) {
    this.val = val;
};

var keywordCache = {};
    
// makeInstance: string -> Keyword.
Keyword.makeInstance = function(val) {
    // To ensure that we can eq? symbols with equal values.
    if (!(val in keywordCache)) {
	keywordCache[val] = new Keyword(val);
    } else {
    }
    return keywordCache[val];
};
    
Keyword.prototype.isEqual = function(other, aUnionFind) {
    return other instanceof Keyword &&
    this.val == other.val;
};
    

Keyword.prototype.toString = function() {
    return this.val;
};

Keyword.prototype.toWrittenString = function(cache) {
    return this.val;
};

Keyword.prototype.toDisplayedString = function(cache) {
    return this.val;
};


//////////////////////////////////////////////////////////////////////


    
    
    
Empty = function() {
};
Empty.EMPTY = new Empty();


Empty.prototype.isEqual = function(other, aUnionFind) {
    return other instanceof Empty;
};

Empty.prototype.reverse = function() {
    return this;
};

Empty.prototype.first = function() {
    throwRuntimeError("first can't be applied on empty.");
};
Empty.prototype.rest = function() {
    throwRuntimeError("rest can't be applied on empty.");
};
Empty.prototype.isEmpty = function() {
    return true;
};
Empty.prototype.toWrittenString = function(cache) { return "empty"; };
Empty.prototype.toDisplayedString = function(cache) { return "empty"; };
Empty.prototype.toString = function(cache) { return "()"; };


    
// Empty.append: (listof X) -> (listof X)
Empty.prototype.append = function(b){
    return b;
};
    
Cons = function(f, r) {
    this.f = f;
    this.r = r;
};

Cons.prototype.reverse = function() {
    var lst = this;
    var ret = Empty.EMPTY;
    while (!lst.isEmpty()){
	ret = Cons.makeInstance(lst.first(), ret);
	lst = lst.rest();
    }
    return ret;
};
    
Cons.makeInstance = function(f, r) {
    return new Cons(f, r);
};


// FIXME: can we reduce the recursion on this?
Cons.prototype.isEqual = function(other, aUnionFind) {
    if (! (other instanceof Cons)) {
	return Logic.FALSE;
    }
    return (isEqual(this.first(), other.first(), aUnionFind) &&
	    isEqual(this.rest(), other.rest(), aUnionFind));
};
    
Cons.prototype.first = function() {
    return this.f;
};
    
Cons.prototype.rest = function() {
    return this.r;
};
    
Cons.prototype.isEmpty = function() {
    return false;
};
    
// Cons.append: (listof X) -> (listof X)
Cons.prototype.append = function(b){
    if (b.isEmpty())
	return this;
    var ret = b;
    var lst = this.reverse();
    while (true) {
	if (lst.isEmpty()) 
	    break;
	ret = Cons.makeInstance(lst.first(), ret);
	lst = lst.rest();
    }
	
    return ret;
};
    

Cons.prototype.toWrittenString = function(cache) {
    //    cache.put(this, true);
    var texts = [];
    var p = this;
    while (true) {
	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
	    texts.push(".");
	    texts.push(toWrittenString(p, cache));
	    break;
	}
	if (p.isEmpty())
	    break;
	texts.push(toWrittenString(p.first(), cache));
	p = p.rest();
    }
    return "(" + texts.join(" ") + ")";
};

Cons.prototype.toString = Cons.prototype.toWrittenString;

Cons.prototype.toDisplayedString = function(cache) {
    //    cache.put(this, true);
    var texts = [];
    var p = this;
    while (true) {
	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
	    texts.push(".");
	    texts.push(toWrittenString(p, cache));
	    break;
	}
	if (p.isEmpty()) 
	    break;
	texts.push(toDisplayedString(p.first(), cache));
	p = p.rest();
    }
    return "(" + texts.join(" ") + ")";
};



Cons.prototype.toDomNode = function(cache) {
    //    cache.put(this, true);
    var node = document.createElement("div");
    node.appendChild(document.createTextNode("("));
    var p = this;
    while (true) {
	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
	    appendChild(node, document.createTextNode(" "));
	    appendChild(node, document.createTextNode("."));
	    appendChild(node, document.createTextNode(" "));
	    appendChild(node, toDomNode(p, cache));
	    break;
	}
	if (p.isEmpty())
	    break;
	appendChild(node, toDomNode(p.first(), cache));
	p = p.rest();
	if (! p.isEmpty()) {
	    appendChild(node, document.createTextNode(" "));
	}
    }
    node.appendChild(document.createTextNode(")"));
    return node;
};



//////////////////////////////////////////////////////////////////////

Vector = function(n, initialElements) {
    this.elts = new Array(n);
    if (initialElements) {
	for (var i = 0; i < n; i++) {
	    this.elts[i] = initialElements[i];
	}
    } else {
	for (var i = 0; i < n; i++) {
	    this.elts[i] = undefined;
	}
    }
};
Vector.makeInstance = function(n, elts) {
    return new Vector(n, elts);
}
    Vector.prototype.length = function() {
	return this.elts.length;
    };
Vector.prototype.ref = function(k) {
    return this.elts[k];
};
Vector.prototype.set = function(k, v) {
    this.elts[k] = v;
};

Vector.prototype.isEqual = function(other, aUnionFind) {
    if (other != null && other != undefined && other instanceof Vector) {
	if (other.length() != this.length()) {
	    return false
	}
	for (var i = 0; i <  this.length(); i++) {
	    if (! isEqual(this.elts[i], other.elts[i], aUnionFind)) {
		return false;
	    }
	}
	return true;
    } else {
	return false;
    }
};

Vector.prototype.toList = function() {
    var ret = Empty.EMPTY;
    for (var i = this.length() - 1; i >= 0; i--) {
	ret = Cons.makeInstance(this.elts[i], ret);	    
    }	
    return ret;
};

Vector.prototype.toWrittenString = function(cache) {
    //    cache.put(this, true);
    var texts = [];
    for (var i = 0; i < this.length(); i++) {
	texts.push(toWrittenString(this.ref(i), cache));
    }
    return "#(" + texts.join(" ") + ")";
};

Vector.prototype.toDisplayedString = function(cache) {
    //    cache.put(this, true);
    var texts = [];
    for (var i = 0; i < this.length(); i++) {
	texts.push(toDisplayedString(this.ref(i), cache));
    }
    return "#(" + texts.join(" ") + ")";
};

Vector.prototype.toDomNode = function(cache) {
    //    cache.put(this, true);
    var node = document.createElement("div");
    node.appendChild(document.createTextNode("#("));
    for (var i = 0; i < this.length(); i++) {
	appendChild(node,
		    toDomNode(this.ref(i), cache));
    }
    node.appendChild(document.createTextNode(")"));
    return node;
};


//////////////////////////////////////////////////////////////////////
/*

// _gcd: fixnum fixnum -> fixnum
var _gcd = function(a, b) {
    while (b != 0) {
	var t = a;
	a = b;
	b = t % b;
    }
    return Math.abs(a);
};

// _lcm: fixnum fixnum -> integer
var _lcm = function(a, b) {
    return Math.abs(a * b / _gcd(a, b));
};

// FIXME: there are two definitions of gcd floating around: which one is right?


//////////////////////////////////////////////////////////////////////



    
// Rationals
    
var gcd = function(a, b) {
    var t;
    if (isNaN(a) || !isFinite(a)) {
	throwRuntimeError("not a number: " + a);
    }
    if (isNaN(b) || !isFinite(b)) {
	throwRuntimeError("not a number: " + b);
    }
    while (b != 0) {
	t = a;
	a = b;
	b = t % b;
    }
    return a;
};
    
var Rational = function(n, d) {
    if (d == undefined) { d = 1; }
    if (d == 0) {
	throwRuntimeError("cannot have zero denominator.");
    }
    var divisor = gcd(Math.abs(n), Math.abs(d));
    this.n = n / divisor;
    this.d = d / divisor;
};

    
Rational.prototype.toWrittenString = function(cache) {
    if (this.d == 1) {
	return this.n + "";
    } else {
	return this.n + "/" + this.d;
    }
};

Rational.prototype.toString = Rational.prototype.toWrittenString;

Rational.prototype.toDisplayedString = Rational.prototype.toWrittenString;

    
Rational.prototype.level = function() {
    return 0;
};
    
    
Rational.prototype.lift = function(target) {
    if (target.level() == 1)
	return FloatPoint.makeInstance(this.n / this.d);
    if (target.level() == 2)	
	return Complex.makeInstance(this, 
				    Rational.ZERO);
    throwRuntimeError("invalid level of Number");
};
    
Rational.prototype.isFinite = function() {
    return true;
};

Rational.prototype.isEqual = function(other, aUnionFind) {
    return this.equals(other);
};

Rational.prototype.equals = function(other) {
    return other instanceof Rational &&
    this.n == other.n &&
    this.d == other.d;
};


Rational.prototype.isInteger = function() { 
    return this.d == 1;
}
    
    Rational.prototype.isRational = function() {
        return true;
    };
    
Rational.prototype.isReal = function() {
    return true;
};

    
Rational.prototype.add = function(other) {
    return Rational.makeInstance(this.n * other.d + 
				 this.d * other.n,
				 this.d * other.d);
};
    
Rational.prototype.subtract = function(other) {
    return Rational.makeInstance((this.n * other.d) - 
				 (this.d * other.n),
				 (this.d * other.d));
};
    
Rational.prototype.multiply = function(other) {
    return Rational.makeInstance(this.n * other.n,
				 this.d * other.d);
};
    
Rational.prototype.divide = function(other) {
    if (this.d * other.n == 0) {
	throwRuntimeError("division by zero");
    }
    return Rational.makeInstance(this.n * other.d,
				 this.d * other.n);
};
    

Rational.prototype.toExact = function() { 
    return this;
};

Rational.prototype.isExact = function() {
    return true;
};
    
Rational.prototype.toFixnum = function() {
    return Math.floor(this.n / this.d);  
};

Rational.prototype.numerator = function() {
    return Rational.makeInstance(this.n);
};

Rational.prototype.denominator = function() {
    return Rational.makeInstance(this.d);
};
    
Rational.prototype.toFloat = function() {
    return this.n / this.d;
};
    
Rational.prototype.toComplex = function(){
    return Complex.makeInstance(this, Rational.ZERO);
};
    
Rational.prototype.greaterThan = function(other) {
    return this.n*other.d > this.d*other.n;
};

Rational.prototype.greaterThanOrEqual = function(other) {
    return this.n*other.d >= this.d*other.n;
};
    
Rational.prototype.lessThan = function(other) {
    return this.n*other.d < this.d*other.n;
};

Rational.prototype.lessThanOrEqual = function(other) {
    return this.n*other.d <= this.d*other.n;
};

    
Rational.prototype.sqrt = function() {
    if (this.n >= 0) {
	var newN = Math.sqrt(this.n);
	var newD = Math.sqrt(this.d);
	if (Math.floor(newN) == newN &&
	    Math.floor(newD) == newD) {
	    return Rational.makeInstance(newN, newD);
	} else {
	    return FloatPoint.makeInstance(newN / newD);
	}
    } else {
	var newN = Math.sqrt(- this.n);
	var newD = Math.sqrt(this.d);
	if (Math.floor(newN) == newN &&
	    Math.floor(newD) == newD) {
	    return Complex.makeInstance(
					Rational.ZERO,
					Rational.makeInstance(newN, newD));
	} else {
	    return Complex.makeInstance(
					Rational.ZERO,
					FloatPoint.makeInstance(newN / newD));
	}
    }
};
    
Rational.prototype.abs = function() {
    return Rational.makeInstance(Math.abs(this.n),
				 this.d);
};
    
Rational.prototype.floor = function() {
    return Rational.makeInstance(Math.floor(this.n / this.d), 1);
};
    
    
Rational.prototype.ceiling = function() {
    return Rational.makeInstance(Math.ceil(this.n / this.d), 1);
};
    
Rational.prototype.conjugate = Rational.prototype.abs;
    
Rational.prototype.magnitude = Rational.prototype.abs;
    
Rational.prototype.log = function(){
    return FloatPoint.makeInstance(Math.log(this.n / this.d));
};
    
Rational.prototype.angle = function(){
    if (0 == this.n)
	return Rational.ZERO;
    if (this.n > 0)
	return Rational.ZERO;
    else
	return FloatPoint.pi;
};
    
Rational.prototype.atan = function(){
    return FloatPoint.makeInstance(Math.atan(this.n / this.d));
};
    
Rational.prototype.cos = function(){
    return FloatPoint.makeInstance(Math.cos(this.n / this.d));
};
    
Rational.prototype.sin = function(){
    return FloatPoint.makeInstance(Math.sin(this.n / this.d));
};
    
Rational.prototype.expt = function(a){
    return FloatPoint.makeInstance(Math.pow(this.n / this.d, a.n / a.d));
};
    
Rational.prototype.exp = function(){
    return FloatPoint.makeInstance(Math.exp(this.n / this.d));
};
    
Rational.prototype.acos = function(){
    return FloatPoint.makeInstance(Math.acos(this.n / this.d));
};
    
Rational.prototype.asin = function(){
    return FloatPoint.makeInstance(Math.asin(this.n / this.d));
};
    
Rational.prototype.imag_dash_part = function(){
    return Rational.ZERO;
};
    
Rational.prototype.real_dash_part = function(){
    return this;
};


Rational.prototype.timesI = function() {
    return Complex.makeInstance(Rational.ZERO, this);
};
    
Rational.prototype.round = function() {
    if (this.d == 2) {
	// Round to even if it's a n/2
	var v = this.n / this.d;
	var fl = Math.floor(v);
	var ce = Math.ceil(v);
	if (fl % 2 == 0) { 
	    return Rational.makeInstance(fl); 
	}
	else { 
	    return Rational.makeInstance(ce); 
	}
    } else {
	return Rational.makeInstance(Math.round(this.n / this.d));
    }
};
    
    
Rational.prototype.half = function(){
    return Rational.makeInstance(this.n, this.d * 2);
};
    
Rational.prototype.minus = function(){
    return Rational.makeInstance(0 - this.n, this.d);
};
    
    
var _rationalCache = {};
Rational.makeInstance = function(n, d) {
    if (n == undefined)
	throwRuntimeError("n undefined");

    if (d == undefined) { d = 1; }
	
    if (d < 0) {
	n = -n;
	d = -d;
    }

    // Defensive edge cases.  We should never hit these
    // cases, but since we don't yet have bignum arithmetic,
    // it's possible that we may pass bad arguments to
    // Integer.makeInstance.
    if (isNaN (n) || isNaN(d)) {
	return FloatPoint.nan;
    }
    if (! isFinite(d)) {
	return Rational.ZERO;
    }
    if (! isFinite(n)) {
	return FloatPoint.makeInstance(n);
    }

    return new Rational(n, d);
//     if (d == 1 && n in _rationalCache) {
// 	return _rationalCache[n];
//     }
//     else {
// 	return new Rational(n, d);
//     }
};
    
_rationalCache = {};
(function() {
    var i;
    for(i = -500; i < 500; i++) {
	_rationalCache[i] = new Rational(i, 1);
    }
})();
Rational.NEGATIVE_ONE = _rationalCache[-1];
Rational.ZERO = _rationalCache[0];
Rational.ONE = _rationalCache[1];
Rational.TWO = _rationalCache[2];
    
    
    
    
    
    
var FloatPoint = function(n) {
    this.n = n;
};
FloatPoint = FloatPoint;


var NaN = new FloatPoint(Number.NaN);
var inf = new FloatPoint(Number.POSITIVE_INFINITY);
var neginf = new FloatPoint(Number.NEGATIVE_INFINITY);

FloatPoint.pi = new FloatPoint(Math.PI);
FloatPoint.e = new FloatPoint(Math.E);
FloatPoint.nan = NaN;
FloatPoint.inf = inf;
FloatPoint.neginf = neginf;

FloatPoint.makeInstance = function(n) {
    if (isNaN(n)) {
	return FloatPoint.nan;
    } else if (n === Number.POSITIVE_INFINITY) {
	return FloatPoint.inf;
    } else if (n === Number.NEGATIVE_INFINITY) {
	return FloatPoint.neginf;
    }
    return new FloatPoint(n);
};



FloatPoint.prototype.isFinite = function() {
    return isFinite(this.n);
};


FloatPoint.prototype.toExact = function() {
    return Rational.makeInstance(Math.floor(this.n), 1);
};

FloatPoint.prototype.isExact = function() {
    return false;
};


FloatPoint.prototype.level = function() {
    return 1;
};
    
FloatPoint.prototype.lift = function(target) {
    return Complex.makeInstance(this, Rational.ZERO);
};
    
FloatPoint.prototype.toWrittenString = function(cache) {
    if (this.n == Number.POSITIVE_INFINITY) {
	return "+inf.0";
    } else if (this.n == Number.NEGATIVE_INFINITY) {
	return "-inf.0";
    } else if (this.n == Number.NaN) {
	return "+nan.0";
    } else {
	return this.n.toString();
    }
};
FloatPoint.prototype.toString = FloatPoint.prototype.toWrittenString;
    
FloatPoint.prototype.toDisplayedString = FloatPoint.prototype.toWrittenString;


FloatPoint.prototype.isEqual = function(other, aUnionFind) {
    return ((other instanceof FloatPoint) &&
	    ((this.n == other.n) ||
	     (isNaN(this.n) && isNaN(other.n))));
};

FloatPoint.prototype.equals = function(other) {
    return ((other instanceof FloatPoint) &&
	    ((this.n == other.n)));
};


FloatPoint.prototype.isRational = function() {
    return this.isFinite() && this.n == Math.floor(this.n);
};

FloatPoint.prototype.isInteger = function() {
    return this.isFinite() && this.n == Math.floor(this.n);
};

FloatPoint.prototype.isReal = function() {
    return true;
};
    

// sign: Number -> {-1, 0, 1}
var sign = function(n) {
    if (NumberTower.lessThan(n, Rational.ZERO)) {
	return -1;
    } else if (NumberTower.greaterThan(n, Rational.ZERO)) {
	return 1;
    } else {
	return 0;
    }
};


FloatPoint.prototype.add = function(other) {
    if (this.isFinite() && other.isFinite()) {
	return FloatPoint.makeInstance(this.n + other.n);
    } else {
	if (isNaN(this.n) || isNaN(other.n)) {
	    return NaN;
	} else if (this.isFinite() && ! other.isFinite()) {
	    return other;
	} else if (!this.isFinite() && other.isFinite()) {
	    return this;
	} else {
	    return ((sign(this) * sign(other) == 1) ?
		    this : NaN);
	};
    }
};
    
FloatPoint.prototype.subtract = function(other) {
    if (this.isFinite() && other.isFinite()) {
	return FloatPoint.makeInstance(this.n - other.n);
    } else if (isNaN(this.n) || isNaN(other.n)) {
	return NaN;
    } else if (! this.isFinite() && ! other.isFinite()) {
	if (sign(this) == sign(other)) {
	    return NaN;
	} else {
	    return this;
	}
    } else if (this.isFinite()) {
	return NumberTower.multiply(other, Rational.NEGATIVE_ONE);
    } else {  // other.isFinite()
	return this;
    }

};
    
FloatPoint.prototype.multiply = function(other) {
    if (this.n == 0 || other.n == 0) { return Rational.ZERO; }

    if (this.isFinite() && other.isFinite()) {
	return FloatPoint.makeInstance(this.n * other.n);
    } else if (isNaN(this.n) || isNaN(other.n)) {
	return NaN;
    } else {
	return ((sign(this) * sign(other) == 1) ? inf : neginf);
    }
};
    
FloatPoint.prototype.divide = function(other) {
    if (this.isFinite() && other.isFinite()) {
	if (other.n == 0) {
	    throwRuntimeError("division by zero");
	}
	return FloatPoint.makeInstance(this.n / other.n);
    } else if (isNaN(this.n) || isNaN(other.n)) {
	return NaN;
    } else if (! this.isFinite() && !other.isFinite()) {
	return NaN;
    } else if (this.isFinite() && !other.isFinite()) {
	return FloatPoint.makeInstance(0.0);
    } else if (! this.isFinite() && other.isFinite()) {
	return ((sign(this) * sign(other) == 1) ? inf : neginf);
    }

};
    
    
FloatPoint.prototype.toFixnum = function() {
    return Math.floor(this.n);  
};
    
FloatPoint.prototype.numerator = function() {
    var stringRep = this.n.toString();
    var match = stringRep.match(/^(.*)\.(.*)$/);
    if (match) {
	return FloatPoint.makeInstance(parseFloat(match[1] + match[2]));
    } else {
	return this;
    }
};

FloatPoint.prototype.denominator = function() {
    var stringRep = this.n.toString();
    var match = stringRep.match(/^(.*)\.(.*)$/);
    if (match) {
	return FloatPoint.makeInstance(Math.pow(10, match[2].length));
    } else {
	return FloatPoint.makeInstance(1.0);
    }
};


FloatPoint.prototype.toFloat = function() {
    return this.n;
};
    
FloatPoint.prototype.toComplex = function(){
    return Complex.makeInstance(this, Rational.ZERO);
};
    
FloatPoint.prototype.floor = function() {
    if (! isFinite(this.n)) {
	return this;
    }
    return Rational.makeInstance(Math.floor(this.n), 1);
};
    
FloatPoint.prototype.ceiling = function() {
    if (! isFinite(this.n)) {
	return this;
    }
    return Rational.makeInstance(Math.ceil(this.n), 1);
};
    


FloatPoint.prototype.greaterThan = function(other) {
    return this.n > other.n;
};
    
FloatPoint.prototype.greaterThanOrEqual = function(other) {
    return this.n >= other.n;
};
    
FloatPoint.prototype.lessThan = function(other) {
    return this.n < other.n;
};
    
FloatPoint.prototype.lessThanOrEqual = function(other) {
    return this.n <= other.n;
};

    
FloatPoint.prototype.sqrt = function() {
    if (this.n < 0) {
	var result = Complex.makeInstance(
					  Rational.ZERO,
					  FloatPoint.makeInstance(Math.sqrt(-this.n)));
	return result;
    } else {
	return FloatPoint.makeInstance(Math.sqrt(this.n));
    }
};
    
FloatPoint.prototype.abs = function() {
    return FloatPoint.makeInstance(Math.abs(this.n));
};
    

    
FloatPoint.prototype.log = function(){
    if (this.n < 0)
	return this.toComplex().log();
    else
	return FloatPoint.makeInstance(Math.log(this.n));
};
    
FloatPoint.prototype.angle = function(){
    if (0 == this.n)
	return Rational.ZERO;
    if (this.n > 0)
	return Rational.ZERO;
    else
	return FloatPoint.pi;
};
    
FloatPoint.prototype.atan = function(){
    return FloatPoint.makeInstance(Math.atan(this.n));
};
    
FloatPoint.prototype.cos = function(){
    return FloatPoint.makeInstance(Math.cos(this.n));
};
    
FloatPoint.prototype.sin = function(){
    return FloatPoint.makeInstance(Math.sin(this.n));
};
    
FloatPoint.prototype.expt = function(a){
    if (this.n == 1) {
	if (a.isFinite()) {
	    return this;
	} else if (isNaN(a.n)){
	    return this;
	} else {
	    return this;
	}
    } else {
	return FloatPoint.makeInstance(Math.pow(this.n, a.n));
    }
};
    
FloatPoint.prototype.exp = function(){
    return FloatPoint.makeInstance(Math.exp(this.n));
};
    
FloatPoint.prototype.acos = function(){
    return FloatPoint.makeInstance(Math.acos(this.n));
};
    
FloatPoint.prototype.asin = function(){
    return FloatPoint.makeInstance(Math.asin(this.n));
};
    
FloatPoint.prototype.imag_dash_part = function(){
    return Rational.ZERO;
};
    
FloatPoint.prototype.real_dash_part = function(){
    return this;
};
    
    
FloatPoint.prototype.round = function(){
    if (isFinite(this.n)) {
	if (Math.abs(Math.floor(this.n) - this.n) == 0.5) {
	    if (Math.floor(this.n) % 2 == 0)
		return Rational.makeInstance(Math.floor(this.n));
	    return Rational.makeInstance(Math.ceil(this.n));
	} else {
	    return Rational.makeInstance(Math.round(this.n));
	}
    } else {
	return this;
    }	
};
    
    
FloatPoint.prototype.conjugate = FloatPoint.prototype.abs;
    
FloatPoint.prototype.magnitude = FloatPoint.prototype.abs;
    
FloatPoint.prototype.minus = function(){
    return FloatPoint.makeInstance(0 - this.n);
};
    
FloatPoint.prototype.half = function(){
    return FloatPoint.makeInstance(this.n / 2);
};	
    
FloatPoint.prototype.timesI = function(){
    return Complex.makeInstance(Rational.ZERO, this);
};
    

Complex = function(r, i){
    this.r = r;
    this.i = i;
};
    
// Constructs a complex number from two basic number r and i.  r and i can
// either be plt.type.Rational or plt.type.FloatPoint.
Complex.makeInstance = function(r, i){
    if (typeof(r) == 'number') {
	r = (r == Math.floor(r) ? Rational.makeInstance(r) :
	     FloatPoint.makeInstance(r));
    }
    if (typeof(i) == 'number') {
	i = (i == Math.floor(i) ? Rational.makeInstance(i) :
	     FloatPoint.makeInstance(i));
    }

    var result = new Complex(r, i);
    return result;
};
    
Complex.prototype.toWrittenString = function(cache) {
    if (NumberTower.greaterThanOrEqual(
				       this.i,
				       Rational.ZERO)) {
        return toWrittenString(this.r) + "+" + toWrittenString(this.i)+"i";
    } else {
	return toWrittenString(this.r) + toWrittenString(this.i)+"i";
    }
};

Complex.prototype.toString = Complex.prototype.toWrittenString;

Complex.prototype.toDisplayedString = Complex.prototype.toWrittenString;



Complex.prototype.isFinite = function() {
    return this.r.isFinite() && this.i.isFinite();
}


    Complex.prototype.isRational = function() {
	return this.r.isRational() && NumberTower.equal(this.i, Rational.ZERO);
    };

Complex.prototype.isInteger = function() {
    return this.r.isInteger() && NumberTower.equal(this.i, Rational.ZERO);
};

Complex.prototype.toExact = function() { 
    if (! this.isReal()) {
	throwRuntimeError("inexact->exact: expects argument of type real number");
    }
    return this.r.toExact();
};

Complex.prototype.isExact = function() { 
    return this.r.isExact() && this.i.isExact();
};



Complex.prototype.level = function(){
    return 2;
};
    
Complex.prototype.lift = function(target){
    throwRuntimeError("Don't know how to lift Complex number");
};
    
Complex.prototype.isEqual = function(other, aUnionFind){
    return this.equals(other);
};

Complex.prototype.equals = function(other) {
    var result = ((other instanceof Complex) && 
		  (NumberTower.equal(this.r, other.r)) &&
		  (NumberTower.equal(this.i, other.i)));
    return result;
};


Complex.prototype.greaterThan = function(other) {
    if (! this.isReal() || ! other.isReal()) {
	throwRuntimeError(">: expects argument of type real number");
    }
    return NumberTower.greaterThan(this.r, other.r);
};

Complex.prototype.greaterThanOrEqual = function(other) {
    if (! this.isReal() || ! other.isReal()) {
	throwRuntimeError(">: expects argument of type real number");
    }
    return NumberTower.greaterThanOrEqual(this.r, other.r);
};

Complex.prototype.lessThan = function(other) {
    if (! this.isReal() || ! other.isReal()) {
	throwRuntimeError(">: expects argument of type real number");
    }
    return NumberTower.lessThan(this.r, other.r);
};

Complex.prototype.lessThanOrEqual = function(other) {
    if (! this.isReal() || ! other.isReal()) {
	throwRuntimeError(">: expects argument of type real number");
    }
    return NumberTower.lessThanOrEqual(this.r, other.r);
};


Complex.prototype.abs = function(){
    if (!NumberTower.equal(this.i, Rational.ZERO).valueOf())
	throwRuntimeError("abs: expects argument of type real number");
    return this.r.abs();
};
    
Complex.prototype.toFixnum = function(){
    if (!NumberTower.equal(this.i, Rational.ZERO).valueOf())
	throwRuntimeError("toFixnum: expects argument of type real number");
    return this.r.toFixnum();
};

Complex.prototype.numerator = function() {
    if (!this.isReal())
	throwRuntimeError("numerator: can only be applied to real number");
    return this.n.numerator();
};
    

Complex.prototype.denominator = function() {
    if (!this.isReal())
	throwRuntimeError("floor: can only be applied to real number");
    return this.n.denominator();
};

    
Complex.prototype.toFloat = function(){
    if (!NumberTower.equal(this.i, Rational.ZERO).valueOf())
	throwRuntimeError("toFloat: expects argument of type real number");
    return this.r.toFloat();
};
    
Complex.prototype.toComplex = function(){
    return this;
};
    
Complex.prototype.add = function(other){
    return Complex.makeInstance(
				NumberTower.add(this.r, other.r),
				NumberTower.add(this.i, other.i));
};
    
Complex.prototype.subtract = function(other){
    return Complex.makeInstance(
				NumberTower.subtract(this.r, other.r),
				NumberTower.subtract(this.i, other.i));
};
    
Complex.prototype.multiply = function(other){

    // If the other value is real, just do primitive division
    if (other.isReal()) {
	return Complex.makeInstance(
				    NumberTower.multiply(this.r, other.r),
				    NumberTower.multiply(this.i, other.r));
    }

    var r = NumberTower.subtract(
				 NumberTower.multiply(this.r, other.r),
				 NumberTower.multiply(this.i, other.i));
    var i = NumberTower.add(
			    NumberTower.multiply(this.r, other.i),
			    NumberTower.multiply(this.i, other.r));
    if (NumberTower.equal(i, Rational.ZERO)) {
	return r;
    }
    return Complex.makeInstance(r, i);
};
    
Complex.prototype.divide = function(other){
    // If the other value is real, just do primitive division
    if (other.isReal()) {
	return Complex.makeInstance(
				    NumberTower.divide(this.r, other.r),
				    NumberTower.divide(this.i, other.r));
    }


    var con = other.conjugate();
    var up =  NumberTower.multiply(this, con).toComplex();

    // Down is guaranteed to be real by this point.
    var down = NumberTower.multiply(other, con);

    var result = Complex.makeInstance(
				      NumberTower.divide(up.r, down),
				      NumberTower.divide(up.i, down));
    return result;
};
    
Complex.prototype.conjugate = function(){
    var result = Complex.makeInstance(
				      this.r, 
				      NumberTower.subtract(Rational.ZERO, 
							   this.i));

    return result;
};
    
Complex.prototype.magnitude = function(){
    var sum = NumberTower.add(
			      NumberTower.multiply(this.r, this.r),
			      NumberTower.multiply(this.i, this.i));
    return sum.sqrt();
};
    
Complex.prototype.isReal = function(){
    return NumberTower.equal(this.i, Rational.ZERO);
};
    
Complex.prototype.sqrt = function(){
    if (this.isReal())
	return this.r.sqrt();
    // http://en.wikipedia.org/wiki/Square_root#Square_roots_of_negative_and_complex_numbers	
    var r_plus_x = NumberTower.add(this.magnitude(), this.r);

    var r = r_plus_x.half().sqrt();

    var i = NumberTower.divide(this.i, NumberTower.multiply(r_plus_x, FloatPoint.makeInstance(2)).sqrt());
	

    return Complex.makeInstance(r, i);
};
    
Complex.prototype.log = function(){
    var m = this.magnitude();
    var theta = this.angle();
    var result = NumberTower.add(
				 m.log(),
				 theta.timesI());
    return result;
};
    
Complex.prototype.angle = function(){
    if (this.isReal()) {
	return this.r.angle();
    }
    if (NumberTower.equal(Rational.ZERO, this.r)) {
	var tmp = FloatPoint.pi.half();
	return NumberTower.greaterThan(this.i, Rational.ZERO) ? tmp : tmp.minus();
    } else {
	var tmp = NumberTower.divide(this.i.abs(), this.r.abs()).atan();
	if (NumberTower.greaterThan(this.r, Rational.ZERO)) {
	    return NumberTower.greaterThan(this.i, Rational.ZERO) ? tmp : tmp.minus();
	} else {
	    return NumberTower.greaterThan(this.i, Rational.ZERO) ? FloatPoint.pi.subtract(tmp) : tmp.subtract(FloatPoint.pi);
	}
    }
};
    
var plusI = Complex.makeInstance(Rational.ZERO,
				 Rational.ONE);
var minusI = Complex.makeInstance(Rational.ZERO,
				  Rational.NEGATIVE_ONE);
    
Complex.prototype.atan = function(){
    if (NumberTower.equal(this, plusI) ||
	NumberTower.equal(this, minusI)) {
	return FloatPoint.makeInstance(Number.NEGATIVE_INFINITY);
    }
    return NumberTower.multiply(
				plusI,
				NumberTower.multiply(
						     FloatPoint.makeInstance(0.5),
						     (NumberTower.divide(
									 NumberTower.add(plusI, this),
									 NumberTower.add(
											 plusI,
											 NumberTower.subtract(Rational.ZERO, this)))).log()));
};
    
Complex.prototype.cos = function(){
    if (this.isReal())
	return this.r.cos();
    var iz = this.timesI();
    var iz_minus = iz.minus();
	
    return NumberTower.add(iz.exp(), iz_minus.exp()).half();
};
    
Complex.prototype.sin = function(){
    if (this.isReal())
	return this.r.sin();
    var iz = this.timesI();
    var iz_minus = iz.minus();
    var z2 = Complex.makeInstance(Rational.ZERO,
				  Rational.TWO);
    var exp_minus = NumberTower.subtract(iz.exp(), iz_minus.exp());
    var result = NumberTower.divide(exp_minus, z2);
    return result;
};
    
Complex.prototype.expt= function(y){
    var expo = NumberTower.multiply(y, this.log());
    return expo.exp();
};
    
Complex.prototype.exp = function(){
    var r = this.r.exp();
    var cos_a = this.i.cos();
    var sin_a = this.i.sin();

    return NumberTower.multiply(
				r,
				NumberTower.add(cos_a, sin_a.timesI()));
};
    
Complex.prototype.acos = function(){
    if (this.isReal())
	return this.r.acos();
    var pi_half = FloatPoint.pi.half();
    var iz = this.timesI();
    var root = NumberTower.subtract(Rational.ONE, this.multiply(this)).sqrt();
    var l = NumberTower.add(iz, root).log().timesI();
    return NumberTower.add(pi_half, l);
};
    
Complex.prototype.asin = function(){
    if (this.isReal())
	return this.r.asin();

    var oneMinusThisSq = 
    NumberTower.subtract(
			 Rational.ONE, 
			 this.multiply(this));
    var sqrtOneMinusThisSq = oneMinusThisSq.sqrt();
    return NumberTower.multiply(
				Rational.TWO,
				(NumberTower.divide(
						    this, 
						    NumberTower.add(
								    Rational.ONE,
								    sqrtOneMinusThisSq))).atan());
};
    
Complex.prototype.ceiling = function(){
    if (!this.isReal())
	throwRuntimeError("ceiling: can only be applied to real number");
    return this.r.ceiling();
};
    
Complex.prototype.floor = function(){
    if (!this.isReal())
	throwRuntimeError("floor: can only be applied to real number");
    return this.r.floor();
};
    
Complex.prototype.imag_dash_part = function(){
    return this.i;
};
    
Complex.prototype.real_dash_part = function(){
    return this.r;
};
    
Complex.prototype.round = function(){
    return this.r.round();
};
    
    
Complex.prototype.timesI = function(){
    return this.multiply(Complex.makeInstance(Rational.ZERO, Rational.ONE));
};
    
Complex.prototype.minus = function(){
    return Complex.makeInstance(NumberTower.subtract(Rational.ZERO, this.r),
				NumberTower.subtract(Rational.ZERO, this.i));
};
    
Complex.prototype.half = function(){
    return Complex.makeInstance(this.r.half(), 
				this.i.half());
};
    
//////////////////////////////////////////////////////////////////////
// NumberTower.
// 
var NumberTower = {};
NumberTower = NumberTower;

    
NumberTower.toFixnum = function(num) {
    return num.toFixnum();
};
    
NumberTower.toFloat = function(num) {
    return num.toFloat();
};
    
NumberTower.abs = function(n) {
    return n.abs();
};
    
NumberTower.isFinite = function(n) {
    return n.isFinite();
}

    NumberTower.toExact = function(x) {
	return x.toExact();
    };

NumberTower.add = function(x, y) {
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    return x.add(y);
};
    
NumberTower.subtract = function(x, y) {
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    return x.subtract(y);
};
    
NumberTower.multiply = function(x, y) {
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    return x.multiply(y);
};
    
NumberTower.divide = function(x, y) {
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    return x.divide(y);
};
    
NumberTower.equal = function(x, y) {
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
	
    return x.equals(y);
};

NumberTower.eqv = function(x, y) {
    return ((x === y) ||
	    (x.level() === y.level() && x.equals(y)));
};
    
NumberTower.approxEqual = function(x, y, delta) {
    return NumberTower.lessThan(NumberTower.abs(NumberTower.subtract(x, y)),
				delta);
};
    
NumberTower.greaterThanOrEqual = function(x, y){
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);

    if (!(x.isReal() && y.isReal()))
	throwRuntimeError("greaterThanOrEqual: couldn't be applied to complex number");
    return x.greaterThanOrEqual(y);
};
    
NumberTower.lessThanOrEqual = function(x, y){
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    if (!(x.isReal() && y.isReal()))
	throwRuntimeError("lessThanOrEqual: couldn't be applied to complex number");
    return x.lessThanOrEqual(y);    	
};
    
NumberTower.greaterThan = function(x, y){
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
	
    if (!(x.isReal() && y.isReal()))
	throwRuntimeError("greaterThan: couldn't be applied to complex number");
    return x.greaterThan(y);
	
};
    
NumberTower.lessThan = function(x, y){
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);

    if (!(x.isReal() && y.isReal()))
	throwRuntimeError("lessThan: couldn't be applied to complex number");
    return x.lessThan(y);
};
    
NumberTower.modulo = function(m, n) {
    var result = 
    Rational.makeInstance(m.toFixnum() % n.toFixnum(),
			  1);

    // The sign of the result should match the sign of n.
    if (NumberTower.lessThan(n, Rational.ZERO)) {
	if (NumberTower.lessThanOrEqual(result, Rational.ZERO)) {
	    return result;
	}
	return NumberTower.add(result, n);

    } else {
	if (NumberTower.lessThan(result, Rational.ZERO)) {
	    return NumberTower.add(result, n);
	}
	return result;
    }
};
    
NumberTower.sqr = function(x) {
    return NumberTower.multiply(x, x);
};


// FIXME: rename to negate
NumberTower.minus = function(x) {
    return x.minus();
};

NumberTower.half = function(x) {
    return x.half();
};


NumberTower.exp = function(x) {
    return x.exp();
};
    
NumberTower.expt = function(x, y){
    if (x.level() < y.level()) x = x.lift(y);
    if (y.level() < x.level()) y = y.lift(x);
    return x.expt(y);
};
    

// gcd: number [number ...] -> number
NumberTower.gcd = function(first, rest) {
    var result = Math.abs(first.toFixnum());
    for (var i = 0; i < rest.length; i++) {
	result = _gcd(result, rest[i].toFixnum());
    }
    return Rational.makeInstance(result);	
};

// lcm: number [number ...] -> number
NumberTower.lcm = function(first, rest) {
    var result = Math.abs(first.toFixnum());
    if (result == 0) { return Rational.ZERO; }
    for (var i = 0; i < rest.length; i++) {
	if (rest[i].toFixnum() == 0) {
	    return Rational.ZERO;
	}
	result = _lcm(result, rest[i].toFixnum());
    }
    return Rational.makeInstance(result);
};

*/






// Now using mutable strings
Str = function(chars) {
	this.chars = chars;
	this.length = chars.length;
}

Str.makeInstance = function(chars) {
	return new Str(chars);
}

Str.fromString = function(s) {
	return Str.makeInstance(s.split(""));
}

Str.prototype.toString = function() {
	return this.chars.join("");
}

Str.prototype.toWrittenString = Str.prototype.toString;
Str.prototype.toDisplayedString = Str.prototype.toString;

Str.prototype.copy = function() {
	return Str.makeInstance(this.chars.slice(0));
}

Str.prototype.substring = function(start, end) {
	if (end == null || end == undefined) {
		end = this.length;
	}
	
	return Str.makeInstance( this.chars.slice(start, end) );
}

Str.prototype.charAt = function(index) {
	return this.chars[index];
}

Str.prototype.charCodeAt = function(index) {
	return this.chars[index].charCodeAt(0);
}

Str.prototype.replace = function(expr, newStr) {
	return Str.fromString( this.toString().replace(expr, newStr) );
}


Str.prototype.isEqual = function(other, aUnionFind) {
	if (! other instanceof Str) {
		return false;
	}
	return this.toString() === other.toString();
}

Str.prototype.lessThan = function(other) {
	return this.toString() < other.toString();
}

Str.prototype.greaterThan = function(other) {
	return this.toString() > other.toString();
}

Str.prototype.lessThanOrEqual = function(other) {
	return this.toString() <= other.toString();
}

Str.prototype.greaterThanOrEqual = function(other) {
	return this.toString() >= other.toString();
}


Str.prototype.set = function(i, c) {
	this.chars[i] = c;
}

Str.prototype.toUpperCase = function() {
	return Str.fromString( this.chars.join("").toUpperCase() );
}

Str.prototype.toLowerCase = function() {
	return Str.fromString( this.chars.join("").toLowerCase() );
}

Str.prototype.match = function(regexpr) {
	return this.toString().match(regexpr);
}


/*
// Strings
// For the moment, we just reuse Javascript strings.
String = String;
String.makeInstance = function(s) {
    return s.valueOf();
};
    
    
// WARNING
// WARNING: we are extending the built-in Javascript string class here!
// WARNING
String.prototype.isEqual = function(other, aUnionFind){
    return this == other;
};
    
var _quoteReplacingRegexp = new RegExp("[\"\\\\]", "g");
String.prototype.toWrittenString = function(cache) {
    return '"' + this.replace(_quoteReplacingRegexp,
			      function(match, submatch, index) {
				  return "\\" + match;
			      }) + '"';
};

String.prototype.toDisplayedString = function(cache) {
    return this;
};
*/


//////////////////////////////////////////////////////////////////////

// makeLowLevelEqHash: -> hashtable
// Constructs an eq hashtable that uses Moby's getEqHashCode function.
var makeLowLevelEqHash = function() {
    return new _Hashtable(function(x) { return getEqHashCode(x); },
			  function(x, y) { return x === y; });
};

makeLowLevelEqHash = makeLowLevelEqHash;









//////////////////////////////////////////////////////////////////////
// Hashtables
var EqHashTable = function(inputHash) {
    this.hash = makeLowLevelEqHash();

};
EqHashTable = EqHashTable;

EqHashTable.prototype.toWrittenString = function(cache) {
    return "<hash>";
};

EqHashTable.prototype.toDisplayedString = function(cache) {
    return "<hash>";
};

EqHashTable.prototype.isEqual = function(other, aUnionFind) {
    if (other == undefined || other == null || (! (other instanceof EqHashTable))) {
	return false; 
    }

    if (this.hash.keys().length != other.hash.keys().length) { 
	return false;
    }

    var keys = this.hash.keys();
    for (var i = 0; i < keys.length; i++){
	if (! (this.hash.get(keys[i]) === other.hash.get(keys[i]))) {
	    return false;
	}
    }
    return true;
};



var EqualHashTable = function(inputHash) {
	this.hash = new _Hashtable(function(x) {
			return toWrittenString(x); 
		},
		function(x, y) {
			return isEqual(x, y); 
		});
};

EqualHashTable = EqualHashTable;

EqualHashTable.prototype.toWrittenString = function(cache) {
    return "<hash>";
};
EqualHashTable.prototype.toDisplayedString = function(cache) {
    return "<hash>";
};

EqualHashTable.prototype.isEqual = function(other, aUnionFind) {
    if (other == undefined || other == null || (! (other instanceof EqualHashTable))) {
	return false; 
    }

    if (this.hash.keys().length != other.hash.keys().length) { 
	return false;
    }

    var keys = this.hash.keys();
    for (var i = 0; i < keys.length; i++){
	if (! (isEqual(this.hash.get(keys[i]),
		       other.hash.get(keys[i]),
		       aUnionFind))) {
	    return false;
	}
    }
    return true;
};











//////////////////////////////////////////////////////////////////////







var toWrittenString = function(x, cache) {
    // if (! cache) { 
    // 	cache = makeLowLevelEqHash();
    // }

    // if (x && cache.containsKey(x)) {
    // 	return "...";
    // }

    if (x == undefined || x == null) {
	return "<undefined>";
    }
    if (typeof(x) == 'string') {
	return x.toWrittenString();
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	return x.toString();
    }
    if (typeof(x.toWrittenString) !== 'undefined') {
	return x.toWrittenString(cache);
    }
    if (typeof(x.toDisplayedString) !== 'undefined') {
	return x.toDisplayedString(cache);
    } else {
	return x.toString();
    }
};



var toDisplayedString = function(x, cache) {
    // if (! cache) {
    // 	cache = makeLowLevelEqHash();
    // }
    // if (x && cache.containsKey(x)) {
    // 	return "...";
    // }

    if (x == undefined || x == null) {
	return "<undefined>";
    }
    if (typeof(x) == 'string') {
	return x.toDisplayedString();
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	return x.toString();
    }
    if (typeof(x.toDisplayedString) !== 'undefined') {
	return x.toDisplayedString(cache);
    }
    if (typeof(x.toWrittenString) !== 'undefined') {
	return x.toWrittenString(cache);
    } else {
	return x.toString();
    }
};



// toDomNode: scheme-value -> dom-node
var toDomNode = function(x, cache) {
    // if (! cache) {
    // 	cache = makeLowLevelEqHash();
    // }
    // if (x && cache.containsKey(x)) {
    // 	return document.createTextNode("...");
    // }

    if (x == undefined || x == null) {
	var node = document.createTextNode("<undefined>");
	return node;
    }
    if (typeof(x) == 'string') {
	var node = document.createTextNode(x.toWrittenString());
	return node;
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	var node = document.createTextNode(x.toString());
	return node;
    }
    if (x.nodeType) {
	return x;
    }
    if (typeof(x.toDomNode) !== 'undefined') {
	return x.toDomNode(cache);
    }
    if (typeof(x.toWrittenString) !== 'undefined') {
	var node = document.createTextNode(toWrittenString(x, cache));
	return node;
    }
    if (typeof(x.toDisplayedString) !== 'undefined') {
	var node = document.createTextNode(toDisplayedString(x, cache));
	return node;
    } else {
	var node = document.createTextNode(x.toString());
	return node;
    }
};




var isNumber = jsnums.isSchemeNumber;
/*
   function(x) {
    return (x != null && x != undefined && (x instanceof Rational || 
					    x instanceof FloatPoint ||
					    x instanceof Complex));
};
*/


// isEqual: X Y -> boolean
// Returns true if the objects are equivalent; otherwise, returns false.
var isEqual = function(x, y, aUnionFind) {
    if (x === y) { return true; }

    if (isNumber(x) && isNumber(y)) {
	return jsnums.equals(x, y);
    }

    if (x == undefined || x == null) {
	return (y == undefined || y == null);
    }

    if (typeof(x) == 'object' && typeof(y) == 'object' && 
	aUnionFind.find(x) === aUnionFind.find(y)) {
	return true;
    } else {
	if (typeof(x) == 'object' && typeof(y) == 'object') { 
	    aUnionFind.merge(x, y); 
	}
	return x.isEqual(y, aUnionFind);
    }
};





// liftToplevelToFunctionValue: primitive-function string fixnum scheme-value -> scheme-value
// Lifts a primitive toplevel or module-bound value to a scheme value.
var liftToplevelToFunctionValue = function(primitiveF,
				       name,
				       minArity, 
				       procedureArityDescription) {
    if (! primitiveF._mobyLiftedFunction) {
	var lifted = function(args) {
	    return primitiveF.apply(null, args.slice(0, minArity).concat([args.slice(minArity)]));
	};
	lifted.isEqual = function(other, cache) { 
	    return this === other; 
	}
	lifted.toWrittenString = function(cache) { 
	    return "<function:" + name + ">";
	};
	lifted.toDisplayedString = lifted.toWrittenString;
	lifted.procedureArity = procedureArityDescription;
	primitiveF._mobyLiftedFunction = lifted;
	    
    } 
    return primitiveF._mobyLiftedFunction;
};



//////////////////////////////////////////////////////////////////////
var ThreadCell = function(v, isPreserved) {
    this.v = v;
    this.isPreserved = isPreserved || false;
};



//////////////////////////////////////////////////////////////////////


// Wrapper around functions that return multiple values.
var ValuesWrapper = function(elts) {
    this.elts = elts;
};


var UndefinedValue = function() {
};
UndefinedValue.prototype.toString = function() {
    return "<undefined>";
};
var UNDEFINED_VALUE = new UndefinedValue();

var VoidValue = function() {};
VoidValue.prototype.toString = function() {
	return "<void>";
};

var VOID_VALUE = new VoidValue();


var ClosureValue = function(numParams, paramTypes, isRest, closureVals, body) {
    this.numParams = numParams;
    this.paramTypes = paramTypes;
    this.isRest = isRest;
    this.closureVals = closureVals;
    this.body = body;
};




ClosureValue.prototype.toString = function() {
    return "<closure>";
};


var CaseLambdaValue = function(name, closures) {
    this.name = name;
    this.closures = closures;
};



var ContinuationClosureValue = function(vstack, cstack) {
    this.vstack = vstack.slice(0);
    this.cstack = cstack.slice(0);
};



//////////////////////////////////////////////////////////////////////



var PrefixValue = function() {
    this.slots = [];
};

PrefixValue.prototype.addSlot = function() {
    this.slots.push(new UndefinedValue());
};

PrefixValue.prototype.ref = function(n) {
    return this.slots[n];
};

PrefixValue.prototype.set = function(n, v) {
    this.slots[n] = v;
};

PrefixValue.prototype.length = function() { 
    return this.slots.length;
};


//////////////////////////////////////////////////////////////////////


var VariableReference = function(prefix, pos) {
    this.prefix = prefix;
    this.pos = pos;
};

VariableReference.prototype.ref = function() {
    return this.prefix.ref(this.pos);
};

VariableReference.prototype.set = function(v) {
    this.prefix.set(this.pos, v);
}



//////////////////////////////////////////////////////////////////////





var makeList = function(args) {
    var result = Empty.EMPTY;
    for(var i = args.length-1; i >= 0; i--) {
	result = Cons.makeInstance(args[i], result);
    }
    return result;
};


var makeVector = function(args) {
    return Vector.makeInstance(args.length, args);
}

var makeString = function(s) {
	if (s instanceof Array) {
//		for (var i = 0; i < s.length; i++) {
//			if ( typeof s[i] !== 'string' || s[i].length != 1 ) {
//				return undefined;
//			}
//		}
		return Str.makeInstance(s);
	}
	else if (typeof s === 'string') {
		return Str.fromString(s);
	}
	else {
		throw new Error('makeString expects and array of 1-character strings or a string;' +
				' given ' + s.toString());
	}
}


types.symbol = Symbol.makeInstance;
types.rational = jsnums.makeRational;
types.float = jsnums.makeFloat;
types.complex = jsnums.makeComplex;
types.list = makeList;
types.vector = makeVector;
types.regexp = function(p) { return new RegularExpression(p) ; }
types.byteRegexp = function(p) { return new ByteRegularExpression(p) ; }
types['char'] = Char.makeInstance;
types['string'] = makeString;
types.box = function(x) { return new Box(x); };
types.path = function(x) { return new Path(x); };
types.bytes = function(x) { return new Bytes(x); };
types.keyword = function(k) { return new Keyword(k); };
types.pair = function(x, y) { return Cons.makeInstance(x, y); };
types.toWrittenString = toWrittenString;
types.toDisplayedString = toDisplayedString;


types.FALSE = Logic.FALSE;
types.TRUE = Logic.TRUE;
types.EMPTY = Empty.EMPTY;

types.isEqual = isEqual;
types.isNumber = isNumber;
types.isSymbol = function(x) { return x instanceof Symbol; };
types.isChar = function(x) { return x instanceof Char; };
types.isString = function(x) { return x instanceof Str; };
types.isCons = function(x) { return x instanceof Cons; };
types.isVector = function(x) { return x instanceof Vector; };
types.isBox = function(x) { return x instanceof Box; };
types.isHash = function(x) { return (x instanceof EqHashTable ||
				     x instanceof EqualHashTable); };
types.isStruct = function(x) { return (x instanceof Struct && !(x instanceof Box)); };

types.UnionFind = UnionFind;
types.cons = Cons.makeInstance;

types.UNDEFINED = UNDEFINED_VALUE;
types.VOID = VOID_VALUE;

types.ValuesWrapper = ValuesWrapper;
types.ClosureValue = ClosureValue;
types.ContinuationClosureValue = ContinuationClosureValue;
types.CaseLambdaValue = CaseLambdaValue;

types.PrefixValue = PrefixValue;
types.VariableReference = VariableReference;



types.Box = Box;
types.ThreadCell = ThreadCell;


//types.NumberTower = NumberTower;


types.makeStructureType = makeStructureType;


})();

