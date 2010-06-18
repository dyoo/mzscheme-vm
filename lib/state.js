//var types = require('./types');



// Represents the interpreter state.


var state = {};

(function () {

var DEBUG_ON = false;

var setDebug = function(v) {
    DEBUG_ON = v;
}

var debug = function(s) {
    if (DEBUG_ON) {
	sys.debug(s);
    }
}

var debugF = function(f_s) {
    if (DEBUG_ON) {
	sys.debug(f_s());
    }
}


var defaultPrintHook = function(thing) { 
    sys.print(types.toWrittenString(thing) + "\n"); };

var defaultDisplayHook = function(thing) { 
    sys.print(types.toDisplayedString(thing)); };

var defaultToplevelNodeHook = function() { 
    throw new Error("There is a software configuration error by the system's maintainer: the toplevel node has not been initialized yet.");
};



// Interpreter
var State = function() {
    this.v = [];       // value register
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack
    this.heap = {};    // map from name to closures
    this.globals = {}; // map from string to types.GlobalBucket values
    this.hooks = { printHook: defaultPrintHook,
		   displayHook: defaultPrintHook,
		   toplevelNodeHook: defaultToplevelNodeHook };
};


State.prototype.clear = function() {
    this.v = [];
    this.vstack = [];
    this.cstack = [];
    this.heap = {};
    this.globals = {};
    this.hooks = { printHook: defaultPrintHook,
		   displayHook: defaultDisplayHook,
		   getToplevelNodeHook: defaultGetToplevelNodeHook };
};


State.prototype.save = function() {
    return { v: this.v,
	     vstack: this.vstack.slice(0),
	     cstack: this.cstack.slice(0),
	     heap: this.heap,
	     globals: this.globals,
             hooks: this.hooks };
};


State.prototype.restore = function(params) {
    this.v = params.v;
    this.vstack = params.vstack;
    this.cstack = params.cstack;
    this.heap = params.heap;
    this.globals = params.globals;
    this.hooks = params.hooks;
};



// Add the following form to the control stack.
State.prototype.pushControl = function(aForm) {
    this.cstack.push(aForm);
};


// Add several forms to the control stack in reverse order.
State.prototype.pushManyControls = function(forms) {
    for (var i = 0; i < forms.length; i++) {
	this.cstack.push(forms[forms.length-1-i]);
    }
};


// Returns true if the machine is in a stuck state.
State.prototype.isStuck = function() {
    return this.cstack.length === 0;
};

// Pop the last pushed form.
State.prototype.popControl = function() {
    if (this.cstack.length === 0) {
	throw new Error("cstack empty");
    }
    return this.cstack.pop();
};


// Push a value.
State.prototype.pushValue = function(aVal) {
    debugF(function(){ return "pushValue" + sys.inspect(aVal); } );
    this.vstack.push(aVal);
};


// Pop a value.
State.prototype.popValue = function() {
    debugF(function(){ return "popValue" });
    if (this.vstack.length === 0) {
 	throw new Error("vstack empty");
    }
    return this.vstack.pop();
};

// Push n undefined values onto the stack.
State.prototype.pushn = function(n) {
    debugF(function(){ return "PUSHN " + n } );
    for (var i = 0; i < n; i++) {
	this.vstack.push(types.UNDEFINED);
    }
};

// Pop n values from the stack.
State.prototype.popn = function(n) {
    debugF(function(){ return "POPN " + n } );
    for (var i = 0; i < n; i++) {
	if (this.vstack.length === 0) {
 	    throw new Error("vstack empty");
	}
	this.vstack.pop();
    }
};


// Peek at the nth value on the stack.
State.prototype.peekn = function(depth) {
    if (this.vstack.length - 1 - (depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    return this.vstack[this.vstack.length - 1 - (depth || 0)];
};

// Set the nth value on the stack.
State.prototype.setn = function(depth, v) {
    this.vstack[this.vstack.length - 1 - (depth || 0)] = v;
};


// Reference an element of a prefix on the value stack.
State.prototype.refPrefix = function(depth, pos) {
    return this.vstack[this.vstack.length-1 - depth].ref(pos);
};


// Set an element of a prefix on the value stack.
State.prototype.setPrefix = function(depth, pos, v) {
    debug("setPrefix");
    this.vstack[this.vstack.length - 1 - depth].set(pos, v);
};




State.prototype.setPrintHook = function(printHook) {
    this.hooks['printHook'] = printHook;
};


State.prototype.getPrintHook = function() {
    return this.hooks['printHook'];
};


State.prototype.setDisplayHook = function(printHook) {
    this.hooks['displayHook'] = printHook;
};


State.prototype.getDisplayHook = function() {
    return this.hooks['displayHook'];
};


State.prototype.getToplevelNodeHook = function() {
    return this.hooks['toplevelNodeHook'];
};


State.prototype.setToplevelNodeHook = function(hook) {
    this.hooks['toplevelNodeHook'] = hook;
};




state.State = State;

})();
