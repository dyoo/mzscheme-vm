// For node.js.
var sys = require('sys');


// Interpreter
var State = function() {
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack
    this.heap = {};
};



// Add the following form to the control stack.
State.prototype.pushControl = function(aForm) {
    this.cstack.push(aForm);
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
    this.vstack.push(aVal);
};


// Pop a value.
State.prototype.popValue = function() {
    if (this.cstack.length === 0) {
	throw new Error("vstack empty");
    }
    return this.vstack.pop();
};







// load: compilationTop -> state
// Load up the bytecode into a state, ready for evaluation.
var load = function(compilationTop) {
    var state = new State();
//     sys.p("Processing " + compilationTop.name);
//     sys.p(compilationTop);

    // Install the indirects table.
    state.processIndirects(compilationTop['compiled-indirects']);

    // Process the prefix.
    state.processPrefix(compilationTop.prefix);

    // Add the code form to the control stack.
    state.pushControl(compilationTop.code);

    return state;
};


State.prototype.processIndirects = function(indirects) {
    for (var i = 0 ;i < indirects.length; i++) {
	var anIndirect = indirects[i];
	sys.p("Installing " + anIndirect.id);
	sys.p(anIndirect.lam);
	this.heap[anIndirect.id] = evaluateLam(anIndirect.lam);
    }
};



State.prototype.processPrefix = function(prefix) {
    var numLifts = prefix['num-lifts'].toFixnum();
    var newPrefix = [];
    for (var i = 0; i< prefix['toplevels'].length + numLifts; i++) {
	newPrefix.push(undefined);
    }
    this.pushValue(newPrefix);
};







//////////////////////////////////////////////////////////////////////



// run: state -> void
var run = function(state) {
    sys.p(state);
    while(true) {
	state.step();
    }
};




State.prototype.step = function() {
    var nextCode = this.popControl();
    switch(nextCode.$) {
    default:
	throw new Error("I don't know how to handle " + nextCode.$);
    }
}



// evaluateLam: lam -> scheme-runtime-value
// Evaluates the lam form and returns a scheme runtime value.
var evaluateLam = function(lam) {
    return lam;
}






exports.load = load;
exports.run = run;