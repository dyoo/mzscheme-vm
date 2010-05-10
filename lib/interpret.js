// For node.js.
var sys = require('sys');





//////////////////////////////////////////////////////////////////////

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


// Returns true if the machine is in a stuck state.
State.prototype.isStuck = function() {
    return this.cstack.length === 0;
}

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



// run: state -> void
var run = function(state) {
    while(! state.isStuck()) {
	sys.print("\n\nstate:\n");
	sys.p(state);
	state.step();
    }
};



State.prototype.processIndirects = function(indirects) {
    for (var i = 0 ;i < indirects.length; i++) {
	var anIndirect = indirects[i];
	this.heap[anIndirect.id] = this.evaluateLam(anIndirect.lam);
    }
};


State.prototype.processPrefix = function(prefix) {
    var numLifts = prefix['num-lifts'].toFixnum();
    var newPrefix = [];
    for (var i = 0; i< prefix['toplevels'].length + numLifts; i++) {
	newPrefix.push(new UndefinedValue());
    }
    this.pushValue(newPrefix);
};




State.prototype.step = function() {
    var nextCode = this.popControl();
    if (typeof(nextCode) === 'function')
	nextCode();
    else {
	switch(nextCode.$) {
	case 'mod':
	    this.executeMod(nextCode);
	    break;
	case 'def-values':
	    this.executeDefValues(nextCode);
	    break;
	case 'indirect':
	    this.pushValue(this.evaluateIndirect(nextCode));
	    break;
	default:
	    throw new Error("I don't know how to handle " + nextCode.$);
	}
    }
};




// executeMod: mod -> void
State.prototype.executeMod = function(mod) {
    this.processPrefix(mod.prefix);
    for(var i = mod.body.length - 1; i >= 0; i--) {
	this.pushControl(mod.body[i]);
    }
};


// evaluateLam: lam -> scheme-runtime-value
// Evaluates the lam form and returns a scheme runtime value.
State.prototype.evaluateLam = function(lam) {
    return new ClosureValue(lam);
};

// executeLam: lam -> void
State.prototype.executeLam = function(lam) {
    this.pushValue(this.evaluateLam);
};


// executeDefValues: defValues -> void
State.prototype.executeDefValues = function(aDefValues) {
    this.pushControl(new InstallDefValues(aDefValues.ids));
    this.pushControl(aDefValues.body);
};


var InstallDefValues = function(ids) {

    return function() {
	for (var i = 0; i < ids; i++) {
	    
	    sys.p("I need to install ");
	    sys.p(ids[i]);
	}
    }
};





// executeApplyValues: applyValues -> void
State.prototype.executeApplyValues = function(anApplyValues) {
};

// evaluateApplyValues: applyValues -> scheme-runtime-value
State.prototype.evaluateApplyValues = function(anApplyValues) {
};



State.prototype.evaluateIndirect = function(anIndirect) {
    return this.heap[anIndirect.value.val];
};


//////////////////////////////////////////////////////////////////////

var UndefinedValue = function() {
};
UndefinedValue.prototype.toString = function() {
    return "<undefined>";
};


var ClosureValue = function(lam) {
    this.lam = lam;
};
ClosureValue.prototype.toString = function() {
    return "<closure>";
}


//////////////////////////////////////////////////////////////////////


exports.load = load;
exports.run = run;