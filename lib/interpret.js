// For node.js.
var sys = require('sys');
var types = require('./types');



//////////////////////////////////////////////////////////////////////

// Interpreter
var State = function() {
    // FIXME: missing the value register.
    this.v = [];    // value register
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack
    this.heap = {};
};


// Set the value of the value register.
State.prototype.setValue = function(v) {
    this.v = v;
};


State.prototype.getValue = function() {
    return this.v;
};

// Add the following form to the control stack.
State.prototype.pushControl = function(aForm) {
    this.cstack.push(aForm);
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
    this.vstack.push(aVal);
};


// Pop a value.
State.prototype.popValue = function() {
    if (this.vstack.length === 0) {
	throw new Error("vstack empty");
    }
    return this.vstack.pop();
};

State.prototype.peekValue = function(depth) {
    if (this.vstack.length - 1 - (depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    return this.vstack[this.vstack.length - 1 - (depth || 0)];
};




// Reference an element of a prefix on the value stack.
State.prototype.refPrefix = function(depth, pos) {
    return this.vstack[this.vstack.length-1 - depth][pos];
};

// Set an element of a prefix on the value stack.
State.prototype.setPrefix = function(depth, pos, v) {
    this.vstack[this.vstack.length - 1 - depth][pos] = v;
};


//////////////////////////////////////////////////////////////////////

// Here are the other values that will appear on the control stack when
// we need to do some extra work.


var FRAME_POP = function(state) {
    sys.print("FRAME_POP\n");
    state.popValue();
};

var FRAME_PUSH = function(state) {
    sys.print("FRAME_PUSH\n");
    state.pushValue("FIXME: get from the value register!");
};

var SET = function(depth) {
    return function(state) {
	sys.print("SET\n");
	if (state.vstack.length - 1 - (depth || 0) < 0) {
	    throw new Error("vstack not long enough");
	}
	state.vstack[state.vstack.length - 1 - (depth || 0)] = state.v;
    };
};

var SWAP = function(depth) {
    return function(state) {
	sys.print("SWAP\n");
	if (state.vstack.length - 1 - (depth || 0) < 0) {
	    throw new Error("vstack not long enough");
	}
	var tmp = state.vstack[state.vstack.length - 1 - (depth || 0)];
	state.vstack[state.vstack.length - 1 - (depth || 0)] = state.v;
	state.v = tmp;
    };
};


var CALL_FOR_APPLY_VALUES = function(state) {
    sys.print("CALL_FOR_APPLY_VALUES\n");
    var procValue = state.getValue();
    var argValue = state.popValue();
    if (argValue instanceof ValuesWrapper) {
	// FIXME
    } else {
	// We have a single argument, which might be wrapped
	// a list because the procedure is vararity.
	state.pushControl(procValue.body);
	if (procValue.isRest) {
	    state.pushValue(types.list([argValue]));
	} else {
	    // Otherwise, just push it back.
	    state.pushValue(argValue);
	}
    }
};

var CALL = function(n) {
    return function(state) {
	sys.print("CALL");
	var procValue = state.popValue();
	var argValue = state.popValue();
	// We have a single argument, which might be wrapped
	// a list because the procedure is vararity.
	state.pushControl(procValue.body);
	if (procValue.isRest) {
	    state.pushValue(types.list([argValue]));
	} else {
	    // Otherwise, just push it back.
	    state.pushValue(argValue);
	}
    };
};


var DEF_VALUES = function(ids) {
    return function(state) {
	sys.print("DEF_VALUES\n");
	var bodyValue = state.getValue();
	if (bodyValue instanceof ValuesWrapper) {
	    if (ids.length !== bodyValue.elts.length) {
		throw new Error("define-vlues: expected " + ids.length 
				+ " values, but received " 
				+ bodyValue.elts.length);
	    }
	    for (var i = 0; i < ids.length; i++) {
		state.setPrefix(ids[i].depth.toFixnum(),
			       ids[i].pos.toFixnum(), 
			       bodyValue.elts[i]);
	    }
	}

	if (ids.length !== 1) {
	    throw new Error("define-values: expected " + ids.length 
			    + " values, but only received one.");
	} else {
	    state.setPrefix(ids[0].depth.toFixnum(),
			   ids[0].pos.toFixnum(),
			   bodyValue);
	}
    }
};


//////////////////////////////////////////////////////////////////////






// load: compilationTop -> state
// Load up the bytecode into a state, ready for evaluation.
var load = function(compilationTop) {
    var state = new State();

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
	nextCode(this);
    else {
	switch(nextCode.$) {
	case 'mod':
	    this.executeMod(nextCode);
	    break;
	case 'def-values':
	    this.executeDefValues(nextCode);
	    break;
	case 'indirect':
	    this.setValue(this.evaluateIndirect(nextCode));
	    break;
	case 'apply-values':
	    this.executeApplyValues(nextCode);
	    break;
	case 'toplevel':
	    this.setValue(this.evaluateToplevel(nextCode));
	    break;
	case 'constant':
	    this.setValue(this.evaluateConstant(nextCode));
	    break;
	case 'seq':
	    this.executeSeq(nextCode);
	    break;
	case 'application':
	    this.executeApplication(nextCode);
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
    return new ClosureValue(this, lam);
};


// executeDefValues: defValues -> void
State.prototype.executeDefValues = function(aDefValues) {
    this.pushControl(DEF_VALUES(aDefValues.ids));
    this.pushControl(aDefValues.body);
};






// executeApplyValues: applyValues -> void
State.prototype.executeApplyValues = function(anApplyValues) {
    this.pushControl(CALL_FOR_APPLY_VALUES);
    this.pushControl(SWAP(0));
    this.pushControl(anApplyValues['args-expr']);
    this.pushControl(SET(0));
    this.pushControl(FRAME_PUSH);
    this.pushControl(anApplyValues.proc);

};



State.prototype.evaluateIndirect = function(anIndirect) {
    return this.heap[anIndirect.value.val];
};


State.prototype.evaluateToplevel = function(aToplevel) {
    return this.refPrefix(aToplevel.depth.toFixnum(),
			  aToplevel.pos.toFixnum());
};


State.prototype.evaluateConstant = function(aConstant) {
    return aConstant.value;
};



State.prototype.executeSeq = function(aSeq) {
    var forms = aSeq.forms;
    for (var i = aSeq.forms.length - 1; i >= 0; i--) {
	this.pushControl(aSeq.forms[i]);
    }
};




State.prototype.executeApplication = function(anApp) {
    var rator = anApp.rator;
    var rands = anApp.rands;

    this.pushControl(CALL(rands.length));
    this.pushControl(anApp.rator);

    for (var i = 0; i < rands.length; i++) {
	this.pushControl(FRAME_PUSH);
    }

    for (var i = 0; i < rands.length; i++) {
	this.pushControl(SET(i));
	this.pushControl(anApp.rands[i]);
    }
};






//////////////////////////////////////////////////////////////////////

var UndefinedValue = function() {
};
UndefinedValue.prototype.toString = function() {
    return "<undefined>";
};


var ClosureValue = function(state, lam) {
    this.numParams = lam['num-params'].toFixnum();
    this.body = lam.body;
    this.isRest = lam['rest?']
    this.closureMap = lam['closure-map'];
    this.closureTypes = lam['closure-types'];
    // not right: we need to capture the values from
    // the value stack.
};
ClosureValue.prototype.toString = function() {
    return "<closure>";
};



// Wrapper around functions that return multiple values.
var ValuesWrapper = function(elts) {
    this.elts = elts;
};



//////////////////////////////////////////////////////////////////////


exports.load = load;
exports.run = run;