// For node.js
var sys = require('sys');


// Interpreter
var state = function(bytecode) {
    this.vstack = [];
    this.cstack = [bytecode];
};

state.prototype.toString = function() {
    return ('vstack=' + this.vstack
	    +', cstack=' + this.cstack);
};


// load: bytecode -> state
// Load up the bytecode into a state, ready for evaluation.
var load = function(bytecode) {
    sys.p(bytecode);
    // I want to be able to pattern match the bytecode.
    // What's most convenient in Javascript is to work with
    // hashes.  I need the emitter to dump out hashes and arrays.
    return new state(bytecode);
};


// run: state -> void
var run = function(state) {
    sys.p(state);
//     while(true) {
//     }
};




exports.load = load;
exports.run = run;