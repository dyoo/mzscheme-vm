//////////////////////////////////////////////////////////////////////
// helper functions

//var jsnums = require('./js-numbers');


var types = {};


(function () {

//////////////////////////////////////////////////////////////////////


var appendChild = function(parent, child) {
    parent.appendChild(child);
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

// Class inheritance infrastructure

// This code copied directly from http://ejohn.org/blog/simple-javascript-inheritance/
var Class = (function(){
	var initializing = false, fnTest = /xyz/.test(function(){xyz;}) ? /\b_super\b/ : /.*/;
	// The base Class implementation (does nothing)
	var innerClass = function(){};
	
	// Create a new Class that inherits from this class
	innerClass.extend = function(prop) {
		var _super = this.prototype;
		
		// Instantiate a base class (but only create the instance,
		// don't run the init constructor)
		initializing = true;
		var prototype = new this();
		initializing = false;
		
		// Copy the properties over onto the new prototype
		for (var name in prop) {
			// Check if we're overwriting an existing function
			prototype[name] = typeof prop[name] == "function" && 
				typeof _super[name] == "function" && fnTest.test(prop[name]) ?
				(function(name, fn){
					return function() {
						var tmp = this._super;
						
						// Add a new ._super() method that is the same method
						// but on the super-class
						this._super = _super[name];
						
						// The method only need to be bound temporarily, so we
						// remove it when we're done executing
						var ret = fn.apply(this, arguments);				
						this._super = tmp;
						
						return ret;
					};
				})(name, prop[name]) :
				prop[name];
		}
		
		// The dummy class constructor
		var Dummy = function() {
			// All construction is actually done in the init method
			if ( !initializing && this.init )
				this.init.apply(this, arguments);
		}
		
		// Populate our constructed prototype object
		Dummy.prototype = prototype;
		
		// Enforce the constructor to be what we expect
		Dummy.constructor = Dummy;

		// And make this class extendable
		Dummy.extend = arguments.callee;
		
		return Dummy;
	};
	return innerClass;
})();
 


//////////////////////////////////////////////////////////////////////


var makeStructureType = function(name, initFieldCnt, autoFieldCnt, parentType) {
    // If no parent type given, then the parent type is Struct
    if ( !parentType ) {
	    parentType = ({type: Struct});
    }

    // Create a new struct type inheriting from the parent
    var aStruct = parentType.type.extend({
	init: function(args) {
		this._super(name, args);
	}
    });

    // Compute the number of arguments and fields for the new type
    var numArgs = initFieldCnt;
    if (parentType) {
	    numArgs += parentType.numberOfArgs;
    }

    var numFields = initFieldCnt + autoFieldCnt;
    if (parentType) {
	    numFields += parentType.numberOfFields;
    }

    // construct and return the new type
    return { 
	type: aStruct,
	numberOfArgs: numArgs,
	numberOfFields: numFields,

	constructor: function(args) { return new aStruct(args);	},
	predicate: function(x) { return x instanceof aStruct; },
	accessor: function(x, i) { return x._fields[i]; },
	mutator: function(x, i, v) { x._fields[i] = v; }
    };
};


// Structures.
var Struct = Class.extend({
	init: function (constructorName, fields) {
	    this._constructorName = constructorName; 
	    this._fields = fields;
	},

	toWrittenString: function(cache) { 
	    //    cache.put(this, true);
	    var buffer = [];
	    buffer.push("(");
	    buffer.push(this._constructorName);
	    for(var i = 0; i < this._fields.length; i++) {
		buffer.push(" ");
		buffer.push(toWrittenString(this._fields[i], cache));
	    }
	    buffer.push(")");
	    return buffer.join("");
	},

	toDisplayedString: function(cache) { return this.toWrittenString(cache); },

	toDomNode: function(cache) {
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
	},


	isEqual: function(other, aUnionFind) {
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
	}
});



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

var Bytes = function(bts, mutable) {
    this.bytes = bts;
    this.mutable = (mutable === undefined) ? false : mutable;
};

Bytes.prototype.get = function(i) {
	return this.bytes[i];
};

Bytes.prototype.set = function(i, b) {
	if (this.mutable) {
		this.bytes[i] = b;
	}
};

Bytes.prototype.length = function() {
	return this.bytes.length;
};

Bytes.prototype.copy = function(mutable) {
	return new Bytes(this.bytes.slice(0), mutable);
};

Bytes.prototype.subbytes = function(start, end) {
	if (end == null || end == undefined) {
		end = this.bytes.length;
	}
	
	return new Bytes( this.bytes.slice(start, end), true );
};


Bytes.prototype.toString = function() {
	var ret = '';
	for (var i = 0; i < this.bytes.length; i++) {
		ret += String.fromCharCode(this.bytes[i]);
	}

	return ret;
};

Bytes.prototype.toDisplayedString = Bytes.prototype.toString;
Bytes.prototype.toWrittenString = function() {
	return '#"' + this.toString() + '"';
};



//////////////////////////////////////////////////////////////////////
// Boxes
    
var Box = function(x, mutable) {
	this.val = x;
	this.mutable = mutable
};

Box.prototype.unbox = function() {
    return this.val;
};

Box.prototype.set = function(newVal) {
    if (this.mutable) {
	    this.val = newVal;
    }
};

Box.prototype.toString = function() {
    return "#&" + this.val.toString();
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
    while ( !lst.isEmpty() ) {
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
	    texts.push(toDisplayedString(p, cache));
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
    this.mutable = true;
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






// Now using mutable strings
var Str = function(chars) {
	this.chars = chars;
	this.length = chars.length;
	this.mutable = true;
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

Str.prototype.toWrittenString = function(cache) {
    return escapeString(this.toString());
}

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


/*
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
*/


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


var _quoteReplacingRegexp = new RegExp("[\"\\\\]", "g");
var escapeString = function(s) {
    return '"' + s.replace(_quoteReplacingRegexp,
			      function(match, submatch, index) {
				  return "\\" + match;
			      }) + '"';
};


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
    this.mutable = true;

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
			return isEqual(x, y, new UnionFind()); 
		});
	this.mutable = true;
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
    if (! cache) { 
     	cache = makeLowLevelEqHash();
    }

    if (x && cache.containsKey(x)) {
     	return "...";
    }

    if (x == undefined || x == null) {
	return "<undefined>";
    }
    if (typeof(x) == 'string') {
	return escapeString(x.toString());
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
    if (! cache) {
    	cache = makeLowLevelEqHash();
    }
    if (x && cache.containsKey(x)) {
    	return "...";
    }

    if (x == undefined || x == null) {
	return "<undefined>";
    }
    if (typeof(x) == 'string') {
	return x.toString();
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
    if (! cache) {
    	cache = makeLowLevelEqHash();
    }
    
    if (x && cache.containsKey(x)) {
    	return document.createTextNode("...");
    }

    if (x == undefined || x == null) {
	var node = document.createTextNode("<undefined>");
	return node;
    }
    if (typeof(x) == 'string') {
	var node = document.createTextNode(toWrittenString(x));
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

var isString = function(s) {
	return (typeof s === 'string' || s instanceof Str);
}


// isEqual: X Y -> boolean
// Returns true if the objects are equivalent; otherwise, returns false.
var isEqual = function(x, y, aUnionFind) {
    if (x === y) { return true; }

    if (isNumber(x) && isNumber(y)) {
	return jsnums.equals(x, y);
    }

    if (isString(x) && isString(y)) {
	return x.toString() === y.toString();
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


var EofValue = function() {};
EofValue.prototype.toString = function() {
	return "<eof>";
}

var EOF_VALUE = new EofValue();


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
    this.definedMask = [];
};

PrefixValue.prototype.addSlot = function(v) {
    if (v === undefined) { 
	this.slots.push(types.UNDEFINED);
	this.definedMask.push(false);
    } else {
        this.slots.push(v);
	if (v instanceof GlobalBucket) {
	    if (v.value === types.UNDEFINED) {
		this.definedMask.push(false);
	    } else {
		this.definedMask.push(true);
	    }
	} else {
	    this.definedMask.push(true);
	}
    }
};

PrefixValue.prototype.ref = function(n) {
    if (this.slots[n] instanceof GlobalBucket) {
	if (this.definedMask[n]) {
	    return this.slots[n].value;
	} else {
	    throw new Error(this.slots[n].name + " is not defined");
	}

    } else {
	if (this.definedMask[n]) {
	    return this.slots[n];
	} else {
	    throw new Error("variable has not been defined");
	}
    }
};

PrefixValue.prototype.set = function(n, v) {
    if (this.slots[n] instanceof GlobalBucket) {
	this.slots[n].value = v;
	this.definedMask[n] = true;
    } else {
	this.slots[n] = v;
	this.definedMask[n] = true;
    }
};


PrefixValue.prototype.length = function() { 
    return this.slots.length;
};


var GlobalBucket = function(name, value) {
    this.name = name;
    this.value = value;
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

//////////////////////////////////////////////////////////////////////

var PrimProc = function(name, numParams, isRest, usesState, impl) {
    this.name = name;
    this.numParams = numParams;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};


var CasePrimitive = function(cases) {
    this.cases = cases;
};

var PrimConst = function(name, val) {
	this.name = name;
	this.val = val;
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
	if (s instanceof Str) {
		return s;
	}
	else if (s instanceof Array) {
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


var makeHashEq = function(lst) {
	var newHash = new EqHashTable();
	while ( !lst.isEmpty() ) {
		newHash.hash.put(lst.first().first(), lst.first().rest());
		lst = lst.rest();
	}
	return newHash;
}


var makeHashEqual = function(lst) {
	var newHash = new EqualHashTable();
	while ( !lst.isEmpty() ) {
		newHash.hash.put(lst.first().first(), lst.first().rest());
		lst = lst.rest();
	}
	return newHash;
}


var Posn = makeStructureType('posn', 2, false);
var Color = makeStructureType('color', 3, false);
var ArityAtLeast = makeStructureType('arity-at-least', 1, false);


types.symbol = Symbol.makeInstance;
types.rational = jsnums.makeRational;
types.float = jsnums.makeFloat;
types.complex = jsnums.makeComplex;
types.bignum = jsnums.makeBignum;
types.list = makeList;
types.vector = makeVector;
types.regexp = function(p) { return new RegularExpression(p) ; }
types.byteRegexp = function(p) { return new ByteRegularExpression(p) ; }
types['char'] = Char.makeInstance;
types['string'] = makeString;
types.box = function(x) { return new Box(x, true); };
types.boxImmutable = function(x) { return new Box(x, false); };
types.path = function(x) { return new Path(x); };
types.bytes = function(x, mutable) { return new Bytes(x, mutable); };
types.keyword = function(k) { return new Keyword(k); };
types.pair = function(x, y) { return Cons.makeInstance(x, y); };
types.hash = makeHashEqual;
types.hashEq = makeHashEq;

types.toWrittenString = toWrittenString;
types.toDisplayedString = toDisplayedString;
types.toDomNode = toDomNode;

types.posn = function(x, y) { return Posn.constructor([x, y]); };
types.posnX = function(psn) { return Posn.accessor(psn, 0); };
types.posnY = function(psn) { return Posn.accessor(psn, 1); };

types.color = function(r, g, b) { return Color.constructor([r, g, b]); };
types.colorRed = function(x) { return Color.accessor(x, 0); };
types.colorGreen = function(x) { return Color.accessor(x, 1); };
types.colorBlue = function(x) { return Color.accessor(x, 2); };

types.arityAtLeast = function(x) { return ArityAtLeast.constructor(x); };
types.arityValue = function(arity) { return ArityAtLeast.accessor(arity, 0); };


types.FALSE = Logic.FALSE;
types.TRUE = Logic.TRUE;
types.EMPTY = Empty.EMPTY;

types.isEqual = isEqual;
types.isNumber = isNumber;
types.isSymbol = function(x) { return x instanceof Symbol; };
types.isChar = function(x) { return x instanceof Char; };
types.isString = isString;
types.isPair = function(x) { return x instanceof Cons; };
types.isVector = function(x) { return x instanceof Vector; };
types.isBox = function(x) { return x instanceof Box; };
types.isHash = function(x) { return (x instanceof EqHashTable ||
				     x instanceof EqualHashTable); };
types.isByteString = function(x) { return x instanceof Bytes; };
types.isStruct = function(x) { return x instanceof Struct; };
types.isPosn = Posn.predicate;
types.isArityAtLeast = ArityAtLeast.predicate;
types.isColor = Color.predicate;

types.UnionFind = UnionFind;
types.cons = Cons.makeInstance;

types.UNDEFINED = UNDEFINED_VALUE;
types.VOID = VOID_VALUE;
types.EOF = EOF_VALUE;

types.ValuesWrapper = ValuesWrapper;
types.ClosureValue = ClosureValue;
types.ContinuationClosureValue = ContinuationClosureValue;
types.CaseLambdaValue = CaseLambdaValue;
types.PrimProc = PrimProc;
types.CasePrimitive = CasePrimitive;
types.PrimConst = PrimConst;

types.PrefixValue = PrefixValue;
types.GlobalBucket = GlobalBucket;
types.VariableReference = VariableReference;



types.Box = Box;
types.ThreadCell = ThreadCell;



types.makeStructureType = makeStructureType;


// Error type exports
var SchemeError = function(val) {
	this.val = val;
}
types.schemeError = function(v) { return new SchemeError(v); };
types.isSchemeError = function(v) { return v instanceof SchemeError; };

var Exn = makeStructureType('exn', 2, 0, false);
var ExnFail = makeStructureType('exn:fail', 0, 0, Exn);
var ExnFailContract = makeStructureType('exn:fail:contract', 0, 0, ExnFail);

types.exn = function(msg, contMrks) { return Exn.constructor([msg, contMrks]); };
types.isExn = Exn.predicate;
types.exnAccessor = Exn.accessor;

types.exnFail = function(msg, contMrks) { return ExnFail.constructor([msg, contMrks]); };
types.isExnFail = ExnFail.predicate;
types.exnFailAccessor = ExnFail.accessor;

types.exnFailContract = function(msg, contMrks) { return ExnFailContract.constructor([msg, contMrks]); };
types.isExnFailContract = ExnFailContract.predicate;
types.exnFailContractAccessor = ExnFailContract.accessor;



})();

