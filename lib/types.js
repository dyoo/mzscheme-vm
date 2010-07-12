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


var makeStructureType = function(theName, initFieldCnt, autoFieldCnt, parentType, autoV) {
    // If no parent type given, then the parent type is Struct
    if ( !parentType ) {
	parentType = ({type: Struct,
		       numberOfArgs: 0,
		       numberOfFields: 0,
		       firstField: 0});
    }
    var numParentArgs = parentType.numberOfArgs;

    // Create a new struct type inheriting from the parent
    var aStruct = parentType.type.extend({
	init: function(name, args) {
		this._super(name, args);

		for (var i = 0; i < initFieldCnt; i++) {
			this._fields.push(args[i+numParentArgs]);
		}
		for (var i = 0; i < autoFieldCnt; i++) {
			this._fields.push(autoV);
		}
	}
    });
    // Set type, necessary for equality checking
    aStruct.prototype.type = aStruct;

    // construct and return the new type
    return { 
	type: aStruct,
	numberOfArgs: initFieldCnt + numParentArgs,
	numberOfFields: initFieldCnt + autoFieldCnt,
	firstField: parentType.firstField + parentType.numberOfFields,

	constructor: function() { return new aStruct(theName, arguments); },
	predicate: function(x) { return x instanceof aStruct; },
	accessor: function(x, i) { return x._fields[i + this.firstField]; },
	mutator: function(x, i, v) { x._fields[i + this.firstField] = v; }
    };
};


// Structures.
var Struct = Class.extend({
	init: function (constructorName, fields) {
	    this._constructorName = constructorName; 
	    this._fields = [];
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
	    if ( this.type === undefined ||
		 other.type === undefined ||
		 this.type !== other.type ) {
		    return false;
	    }
//	    if (typeof(other) != 'object') {
//		return false;
//	    }
//	    if (! other._constructorName) {
//		return false;
//	    }
//	    if (other._constructorName != this._constructorName) {
//		return false;
//	    }
//	    if (typeof(other._fields) === 'undefined') {
//		return false;
//	    }
//	    if (this._fields.length != other._fields.length) {
//		return false;
//	    }
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
Struct.prototype.type = Struct;



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
	var ret = ['#"'];
	for (var i = 0; i < this.bytes.length; i++) {
		ret.push( escapeByte(this.bytes[i]) );
	}
	ret.push('"');
	return ret.join('');
};

var escapeByte = function(aByte) {
	var ret = [];
	var returnVal;
	switch(aByte) {
		case 7: returnVal = '\\a'; break;
		case 8: returnVal = '\\b'; break;
		case 9: returnVal = '\\t'; break;
		case 10: returnVal = '\\n'; break;
		case 11: returnVal = '\\v'; break;
		case 12: returnVal = '\\f'; break;
		case 13: returnVal = '\\r'; break;
		case 34: returnVal = '\\"'; break;
		case 92: returnVal = '\\\\'; break;
		default: if (val >= 32 && val <= 126) {
				 returnVal = String.fromCharCode(val);
			 }
			 else {
				 ret.push( '\\' + val.toString(8) );
			 }
			 break;
	}
	return returnVal;
};




//////////////////////////////////////////////////////////////////////
// Boxes
    
var Box = function(x, mutable) {
	this.val = x;
	this.mutable = mutable;
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
};

Box.prototype.toWrittenString = function(cache) {
    return "#&" + toWrittenString(this.val, cache);
};

Box.prototype.toDisplayedString = function(cache) {
    return "#&" + toDisplayedString(this.val, cache);
};

Box.prototype.toDomNode = function(cache) {
    var parent = document.createElement("span");
    parent.appendChild(document.createTextNode('#&'));
    parent.appendChild(toDomNode(this.val, cache));
    return parent;
};

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

Char.prototype.toString = function() {
	var code = this.val.charCodeAt(0);
	var returnVal;
	switch (code) {
		case 0: returnVal = '#\\nul'; break;
		case 8: returnVal = '#\\backspace'; break;
		case 9: returnVal = '#\\tab'; break;
		case 10: returnVal = '#\\newline'; break;
		case 11: returnVal = '#\\vtab'; break;
		case 12: returnVal = '#\\page'; break;
		case 13: returnVal = '#\\return'; break;
		case 20: returnVal = '#\\space'; break;
		case 127: returnVal = '#\\rubout'; break;
		default: if (code >= 32 && code <= 126) {
				 returnVal = ("#\\" + this.val);
			 }
			 else {
				 var numStr = code.toString(16).toUpperCase();
				 while (numStr.length < 4) {
					 numStr = '0' + numStr;
				 }
				 returnVal = ('#\\u' + numStr);
			 }
			 break;
	}
	return returnVal;
};

Char.prototype.toWrittenString = Char.prototype.toString;

Char.prototype.toDisplayedString = function (cache) {
    return this.val;
};

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
    if (b === Empty.EMPTY)
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
    while ( p instanceof Cons ) {
	texts.push(toWrittenString(p.first(), cache));
	p = p.rest();
    }
    if ( p !== Empty.EMPTY ) {
	texts.push('.');
	texts.push(toWrittenString(p, cache));
    }
//    while (true) {
//	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
//	    texts.push(".");
//	    texts.push(toWrittenString(p, cache));
//	    break;
//	}
//	if (p.isEmpty())
//	    break;
//	texts.push(toWrittenString(p.first(), cache));
//	p = p.rest();
//    }
    return "(" + texts.join(" ") + ")";
};

Cons.prototype.toString = Cons.prototype.toWrittenString;

Cons.prototype.toDisplayedString = function(cache) {
    //    cache.put(this, true);
    var texts = [];
    var p = this;
    while ( p instanceof Cons ) {
	texts.push(toDisplayedString(p.first(), cache));
	p = p.rest();
    }
    if ( p !== Empty.EMPTY ) {
	texts.push('.');
	texts.push(toDisplayedString(p, cache));
    }
//    while (true) {
//	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
//	    texts.push(".");
//	    texts.push(toDisplayedString(p, cache));
//	    break;
//	}
//	if (p.isEmpty()) 
//	    break;
//	texts.push(toDisplayedString(p.first(), cache));
//	p = p.rest();
//    }
    return "(" + texts.join(" ") + ")";
};



Cons.prototype.toDomNode = function(cache) {
    //    cache.put(this, true);
    var node = document.createElement("span");
    node.appendChild(document.createTextNode("("));
    var p = this;
    while ( p instanceof Cons ) {
	appendChild(node, toDomNode(p.first(), cache));
	p = p.rest();
	if ( p !== Empty.EMPTY ) {
	    appendChild(node, document.createTextNode(" "));
	}
    }
    if ( p !== Empty.EMPTY ) {
	appendChild(node, document.createTextNode("."));
	appendChild(node, document.createTextNode(" "));
	appendChild(node, toDomNode(p, cache));
    }
//    while (true) {
//	if ((!(p instanceof Cons)) && (!(p instanceof Empty))) {
//	    appendChild(node, document.createTextNode(" "));
//	    appendChild(node, document.createTextNode("."));
//	    appendChild(node, document.createTextNode(" "));
//	    appendChild(node, toDomNode(p, cache));
//	    break;
//	}
//	if (p.isEmpty())
//	    break;
//	appendChild(node, toDomNode(p.first(), cache));
//	p = p.rest();
//	if (! p.isEmpty()) {
//	    appendChild(node, document.createTextNode(" "));
//	}
//    }
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
    var node = document.createElement("span");
    node.appendChild(document.createTextNode("#("));
    for (var i = 0; i < this.length(); i++) {
	appendChild(node,
		    toDomNode(this.ref(i), cache));
	if (i !== this.length()-1) {
	    appendChild(node, document.createTextNode(" "));
	}
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


Str.prototype.isEqual = function(other, aUnionFind) {
	if ( !(other instanceof Str || typeof(other) == 'string') ) {
		return false;
	}
	return this.toString() === other.toString();
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


//var _quoteReplacingRegexp = new RegExp("[\"\\\\]", "g");
var escapeString = function(s) {
    return '"' + replaceUnprintableStringChars(s) + '"';
//    return '"' + s.replace(_quoteReplacingRegexp,
//			      function(match, submatch, index) {
//				  return "\\" + match;
//			      }) + '"';
};

var replaceUnprintableStringChars = function(s) {
	var ret = [];
	for (var i = 0; i < s.length; i++) {
		var val = s.charCodeAt(i);
		switch(val) {
			case 7: ret.push('\\a'); break;
			case 8: ret.push('\\b'); break;
			case 9: ret.push('\\t'); break;
			case 10: ret.push('\\n'); break;
			case 11: ret.push('\\v'); break;
			case 12: ret.push('\\f'); break;
			case 13: ret.push('\\r'); break;
			case 34: ret.push('\\"'); break;
			case 92: ret.push('\\\\'); break;
			default: if (val >= 32 && val <= 126) {
					 ret.push( s.charAt(i) );
				 }
				 else {
					 var numStr = val.toString(16).toUpperCase();
					 while (numStr.length < 4) {
						 numStr = '0' + numStr;
					 }
					 ret.push('\\u' + numStr);
				 }
				 break;
		}
	}
	return ret.join('');
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








//////////////////////////////////////////////////////////////////////
// Hashtables
var EqHashTable = function(inputHash) {
    this.hash = makeLowLevelEqHash();
    this.mutable = true;

};
EqHashTable = EqHashTable;

EqHashTable.prototype.toWrittenString = function(cache) {
    var keys = this.hash.keys();
    var ret = [];
    for (var i = 0; i < keys.length; i++) {
	    var keyStr = types.toWrittenString(keys[i], cache);
	    var valStr = types.toWrittenString(this.hash.get(keys[i]), cache);
	    ret.push('(' + keyStr + ' . ' + valStr + ')');
    }
    return ('#hasheq(' + ret.join(' ') + ')');
};

EqHashTable.prototype.toDisplayedString = function(cache) {
    var keys = this.hash.keys();
    var ret = [];
    for (var i = 0; i < keys.length; i++) {
	    var keyStr = types.toDisplayedString(keys[i], cache);
	    var valStr = types.toDisplayedString(this.hash.get(keys[i]), cache);
	    ret.push('(' + keyStr + ' . ' + valStr + ')');
    }
    return ('#hasheq(' + ret.join(' ') + ')');
};

EqHashTable.prototype.isEqual = function(other, aUnionFind) {
    if ( !(other instanceof EqHashTable) ) {
	return false; 
    }

    if (this.hash.keys().length != other.hash.keys().length) { 
	return false;
    }

    var keys = this.hash.keys();
    for (var i = 0; i < keys.length; i++){
	if ( !(other.hash.containsKey(keys[i]) &&
	       isEqual(this.hash.get(keys[i]),
		       other.hash.get(keys[i]),
		       aUnionFind)) ) {
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
    var keys = this.hash.keys();
    var ret = [];
    for (var i = 0; i < keys.length; i++) {
	    var keyStr = types.toWrittenString(keys[i], cache);
	    var valStr = types.toWrittenString(this.hash.get(keys[i]), cache);
	    ret.push('(' + keyStr + ' . ' + valStr + ')');
    }
    return ('#hash(' + ret.join(' ') + ')');
};
EqualHashTable.prototype.toDisplayedString = function(cache) {
    var keys = this.hash.keys();
    var ret = [];
    for (var i = 0; i < keys.length; i++) {
	    var keyStr = types.toDisplayedString(keys[i], cache);
	    var valStr = types.toDisplayedString(this.hash.get(keys[i]), cache);
	    ret.push('(' + keyStr + ' . ' + valStr + ')');
    }
    return ('#hash(' + ret.join(' ') + ')');
};

EqualHashTable.prototype.isEqual = function(other, aUnionFind) {
    if ( !(other instanceof EqualHashTable) ) {
	return false; 
    }

    if (this.hash.keys().length != other.hash.keys().length) { 
	return false;
    }

    var keys = this.hash.keys();
    for (var i = 0; i < keys.length; i++){
	if (! (other.hash.containsKey(keys[i]) &&
	       isEqual(this.hash.get(keys[i]),
		       other.hash.get(keys[i]),
		       aUnionFind))) {
	    return false;
	}
    }
    return true;
};


//////////////////////////////////////////////////////////////////////

var JsObject = function(name, obj) {
	this.name = name;
	this.obj = obj;
};

JsObject.prototype.toString = function() {
	return '#<js-object:' + this.name + '>';
};

//////////////////////////////////////////////////////////////////////

var WorldConfig = function(startup, shutdown, args) {
	this.startup = startup;
	this.shutdown = shutdown;
	this.startupArgs = args;
	this.shutdownArg = undefined;
};

WorldConfig.prototype.toString = function() {
	return '#<world-config>';
};

WorldConfig.prototype.isEqual = function(other, aUnionFind) {
	if ( ! isEqual(this.startup, other.startup, aUnionFind) ||
	     ! isEqual(this.shutdown, other.shutdown, aUnionFind) ||
	     this.startupArgs.length != other.startupArgs.length || 
	     ! isEqual(this.shutdownArg, other.shutdownArg, aUnionFind) ) {
		return false;
	}

	for (var i = 0; i < args.length; i++) {
		if ( !isEqual(this.startupArgs[i], other.startupArgs[i], aUnionFind) )
			return false;
	}
	return true;
};


//var CoercedWorldHandler = function(handler) {
//	this.handler = handler;
//};
//
//CoercedWorldHandler.prototype.toString = function() {
//	return '#<coerced-world-handler>';
//};
//
//CoercedWorldHandler.prototype.isEqual = function(other, aUnionFind) {
//	return isEqual(this.handler, other.handler, aUnionFind);
//};

//////////////////////////////////////////////////////////////////////









//////////////////////////////////////////////////////////////////////







var toWrittenString = function(x, cache) {
    if (! cache) { 
     	cache = makeLowLevelEqHash();
    }

    if (typeof(x) == 'object') {
	    if (cache.containsKey(x)) {
		    return "...";
	    }
	    cache.put(x, true);
    }

    if (x == undefined || x == null) {
	return "#<undefined>";
    }
    if (typeof(x) == 'string') {
	return escapeString(x.toString());
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	return x.toString();
    }

    var returnVal;
    if (typeof(x.toWrittenString) !== 'undefined') {
	returnVal = x.toWrittenString(cache);
    } else if (typeof(x.toDisplayedString) !== 'undefined') {
	returnVal = x.toDisplayedString(cache);
    } else {
	returnVal = x.toString();
    }
    cache.remove(x);
    return returnVal;
};



var toDisplayedString = function(x, cache) {
    if (! cache) {
    	cache = makeLowLevelEqHash();
    }
    if (typeof(x) == 'object') {
	    if (cache.containsKey(x)) {
		    return "...";
	    }
	    cache.put(x, true);
    }

    if (x == undefined || x == null) {
	return "#<undefined>";
    }
    if (typeof(x) == 'string') {
	return x;
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	return x.toString();
    }

    var returnVal;
    if (typeof(x.toDisplayedString) !== 'undefined') {
	returnVal = x.toDisplayedString(cache);
    } else if (typeof(x.toWrittenString) !== 'undefined') {
	returnVal = x.toWrittenString(cache);
    } else {
	returnVal = x.toString();
    }
    cache.remove(x);
    return returnVal;
};



// toDomNode: scheme-value -> dom-node
var toDomNode = function(x, cache) {
    if (! cache) {
    	cache = makeLowLevelEqHash();
    }
    
    if (typeof(x) == 'object') {
	    if (cache.containsKey(x)) {
		    return document.createTextNode("...");
	    }
	    cache.put(x, true);
    }

    if (x == undefined || x == null) {
	var node = document.createTextNode("#<undefined>");
	return node;
    }
    if (typeof(x) == 'string') {
	var wrapper = document.createElement("span");
        wrapper.style["white-space"] = "pre";	
	var node = document.createTextNode(toWrittenString(x));
	wrapper.appendChild(node);
	return wrapper;
    }
    if (typeof(x) != 'object' && typeof(x) != 'function') {
	var node = document.createTextNode(x.toString());
	return node;
    }

    var returnVal;
    if (x.nodeType) {
	returnVal =  x;
    } else if (typeof(x.toDomNode) !== 'undefined') {
	returnVal =  x.toDomNode(cache);
    } else if (typeof(x.toWrittenString) !== 'undefined') {
	var node = document.createTextNode(x.toWrittenString(cache));
	returnVal =  node;
    } else if (typeof(x.toDisplayedString) !== 'undefined') {
	var node = document.createTextNode(x.toDisplayedString(cache));
	returnVal =  node;
    } else {
	var node = document.createTextNode(x.toString());
	returnVal =  node;
    }
    cache.remove(x);
    return returnVal;
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

    if ( typeof(x) == 'object' &&
	 typeof(y) == 'object' &&
	 x.isEqual &&
	 y.isEqual) {
	if (aUnionFind.find(x) === aUnionFind.find(y)) {
	    return true;
	}
	else {
	    aUnionFind.merge(x, y); 
	    return x.isEqual(y, aUnionFind);
	}
    }
    return false;
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
	    return "#<procedure:" + name + ">";
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
    return "#<undefined>";
};
var UNDEFINED_VALUE = new UndefinedValue();

var VoidValue = function() {};
VoidValue.prototype.toString = function() {
	return "#<void>";
};

var VOID_VALUE = new VoidValue();


var EofValue = function() {};
EofValue.prototype.toString = function() {
	return "#<eof>";
}

var EOF_VALUE = new EofValue();


var ClosureValue = function(numParams, paramTypes, isRest, closureVals, body) {
    this.name = false;
    this.numParams = numParams;
    this.paramTypes = paramTypes;
    this.isRest = isRest;
    this.closureVals = closureVals;
    this.body = body;
};




ClosureValue.prototype.toString = function() {
    return "#<procedure>";
};


var CaseLambdaValue = function(name, closures) {
    this.name = name;
    this.closures = closures;
};

CaseLambdaValue.prototype.toString = function() {
    return "#<case-lambda-procedure>";
};



var ContinuationClosureValue = function(vstack, cstack) {
    this.name = false;
    this.vstack = vstack.slice(0);
    this.cstack = cstack.slice(0);
};

ContinuationClosureValue.prototype.toString = function() {
    return "#<procedure>";
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
	    throw types.schemeError(types.exnFailContractVariable(
			"reference to an identifier before its definition: " + this.slots[n].name,
			false, this.slots[n].name));
	}

    } else {
	if (this.definedMask[n]) {
	    return this.slots[n];
	} else {
	    throw types.schemeError(types.exnFailContractVariable(
			"variable has not been defined", false, false));
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

// Continuation Marks

var ContMarkRecordControl = function(dict) {
    this.dict = dict || makeLowLevelEqHash();
};

ContMarkRecordControl.prototype.invoke = function(state) {
    // No-op: the record will simply pop off the control stack.
};

ContMarkRecordControl.prototype.update = function(key, val) {
    var newDict = makeLowLevelEqHash();
    // FIXME: what's the javascript idiom for hash key copy?
    // Maybe we should use a rbtree instead?
    var oldKeys = this.dict.keys();
    for (var i = 0; i < oldKeys.length; i++) {
	    newDict.put( oldKeys[i], this.dict.get(oldKeys[i]) );
    }
    newDict.put(key, val);
    return new ContMarkRecordControl(newDict);
};



var ContinuationMarkSet = function(dict) {
    this.dict = dict;
}

ContinuationMarkSet.prototype.toDomNode = function(cache) {
    var dom = document.createElement("span");
    dom.appendChild(document.createTextNode('#<continuation-mark-set>'));
    return dom;
};

ContinuationMarkSet.prototype.toWrittenString = function(cache) {
    return '#<continuation-mark-set>';
};

ContinuationMarkSet.prototype.toDisplayedString = function(cache) {
    return '#<continuation-mark-set>';
};

ContinuationMarkSet.prototype.ref = function(key) {
    if ( this.dict.containsKey(key) ) {
	    return this.dict.get(key);
    }
    return [];
};



//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////

var PrimProc = function(name, numParams, isRest, usesState, impl) {
    this.name = name;
    this.numParams = numParams;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};

PrimProc.prototype.toString = function() {
    return ("#<procedure:" + this.name + ">");
};

PrimProc.prototype.toWrittenString = function(cache) {
    return ("#<procedure:" + this.name + ">");
};

PrimProc.prototype.toDisplayedString = function(cache) {
    return ("#<procedure:" + this.name + ">");
};


PrimProc.prototype.toDomNode = function(cache) {
    var div = document.createElement("span");
    div.appendChild(document.createTextNode("#<procedure:"+ this.name +">"));
    return div;
};


var CasePrimitive = function(name, cases) {
    this.name = name;
    this.cases = cases;
};


CasePrimitive.prototype.toDomNode = function(cache) {
    var div = document.createElement("span");
    div.appendChild(document.createTextNode("#<procedure:"+ this.name +">"));
    return div;    
};

CasePrimitive.prototype.toWrittenString = function(cache) {
    return ("#<procedure:" + this.name + ">");
};

CasePrimitive.prototype.toDisplayedString = function(cache) {
    return ("#<procedure:" + this.name + ">");
};





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
		throw types.internalError('makeString expects and array of 1-character strings or a string;' +
					  ' given ' + s.toString(),
					  false);
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


var Posn = makeStructureType('posn', 2, 0, false, false);
var Color = makeStructureType('color', 3, 0, false, false);
var ArityAtLeast = makeStructureType('arity-at-least', 1, 0, false, false);


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
types.jsObject = function(name, obj) { return new JsObject(name, obj); };

types.toWrittenString = toWrittenString;
types.toDisplayedString = toDisplayedString;
types.toDomNode = toDomNode;

types.posn = Posn.constructor;
types.posnX = function(psn) { return Posn.accessor(psn, 0); };
types.posnY = function(psn) { return Posn.accessor(psn, 1); };

types.color = Color.constructor;
types.colorRed = function(x) { return Color.accessor(x, 0); };
types.colorGreen = function(x) { return Color.accessor(x, 1); };
types.colorBlue = function(x) { return Color.accessor(x, 2); };

types.arityAtLeast = ArityAtLeast.constructor;
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
types.isFunction = function(x) {
	return (x instanceof PrimProc ||
		x instanceof CasePrimitive ||
		x instanceof ClosureValue ||
		x instanceof CaseLambdaValue ||
		x instanceof ContinuationClosureValue);
};
types.isJsObject = function(x) { return x instanceof JsObject; };

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

types.contMarkRecordControl = function(dict) { return new ContMarkRecordControl(dict); };
types.isContMarkRecordControl = function(x) { return x instanceof ContMarkRecordControl; };
types.continuationMarkSet = function(dict) { return new ContinuationMarkSet(dict); };
types.isContinuationMarkSet = function(x) { return x instanceof ContinuationMarkSet; };


types.PrefixValue = PrefixValue;
types.GlobalBucket = GlobalBucket;
types.VariableReference = VariableReference;

types.Box = Box;
types.ThreadCell = ThreadCell;



types.Class = Class;


types.makeStructureType = makeStructureType;


types.makeLowLevelEqHash = makeLowLevelEqHash;


// Error type exports
var InternalError = function(val, contMarks) {
	this.val = val;
	this.contMarks = (contMarks ? contMarks : false);
}
types.internalError = function(v, contMarks) { return new InternalError(v, contMarks); };
types.isInternalError = function(x) { return x instanceof InternalError; };

var SchemeError = function(val) {
	this.val = val;
}
types.schemeError = function(v) { return new SchemeError(v); };
types.isSchemeError = function(v) { return v instanceof SchemeError; };

var Exn = makeStructureType('exn', 2, 0, false, false);
types.exn = Exn.constructor;
types.isExn = Exn.predicate;
types.exnMessage = function(exn) { return Exn.accessor(exn, 0); };
types.exnContMarks = function(exn) { return Exn.accessor(exn, 1); };
types.exnSetContMarks = function(exn, v) { Exn.mutator(exn, 1, v); };

// (define-struct (exn:break exn) (continuation))
var ExnBreak = makeStructureType('exn:break', 1, 0, Exn, false);
types.exnBreak = ExnBreak.constructor;
types.isExnBreak = ExnBreak.predicate;
types.exnBreakContinuation = function(exn) {
    return ExnBreak.accessor(exn, 0); };

var ExnFail = makeStructureType('exn:fail', 0, 0, Exn, false);
types.exnFail = ExnFail.constructor;
types.isExnFail = ExnFail.predicate;

var ExnFailContract = makeStructureType('exn:fail:contract', 0, 0, ExnFail, false);
types.exnFailContract = ExnFailContract.constructor;
types.isExnFailContract = ExnFailContract.predicate;

var ExnFailContractArity = makeStructureType('exn:fail:contract:arity', 0, 0, ExnFailContract, false);
types.exnFailContractArity = ExnFailContract.constructor;
types.isExnFailContractArity = ExnFailContract.predicate;

var ExnFailContractVariable = makeStructureType('exn:fail:contract:variable', 1, 0, ExnFailContract, false);
types.exnFailContractVariable = ExnFailContract.constructor;
types.isExnFailContractVariable = ExnFailContract.predicate;
types.exnFailContractVariableId = function(exn) { return ExnFailContractVariable.accessor(exn, 0); };

var ExnFailContractDivisionByZero = makeStructureType('exn:fail:contract:division-by-zero', 0, 0, ExnFailContract, false);
types.exnFailContractDivisionByZero = ExnFailContractDivisionByZero.constructor;
types.isExnFailContractDivisionByZero = ExnFailContractDivisionByZero.predicate;


///////////////////////////////////////
// World-specific exports

types.worldConfig = function(startup, shutdown, args) { return new WorldConfig(startup, shutdown, args); };
types.isWorldConfig = function(x) { return x instanceof WorldConfig; };

//types.coercedWorldHandler = function(handler) { return new CoercedWorldHandler(handler); };
//types.isCoercedWorldHandler = function(x) { return x instanceof CoercedWorldHandler; };


})();

