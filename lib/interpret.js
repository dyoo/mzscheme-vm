// For node.js
var sys = require('sys');


// Interpreter
var state = function(bytecode) {
    this.vstack = [];
    this.cstack = [];
};


// load: bytecode -> state
var load = function(bytecode) {
    return new state(bytecode);
};


// run: state -> void
var run = function(state) {
    sys.print("42\n");
//     while(true) {
//     }
};




exports.load = load;
exports.run = run;