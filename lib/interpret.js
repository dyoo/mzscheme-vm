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
// 	sys.print("\n\nstate:\n");
// 	sys.p(state);

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
	    this.pushValue(this.evaluateIndirect(nextCode));
	    break;
	case 'apply-values':
	    this.executeApplyValues(nextCode);
	    break;
	case 'toplevel':
	    this.pushValue(this.evaluateToplevel(nextCode));
	    break;
	case 'constant':
	    this.pushValue(this.evaluateConstant(nextCode));
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

// executeLam: lam -> void
State.prototype.executeLam = function(lam) {
    this.pushValue(this.evaluateLam);
};


// executeDefValues: defValues -> void
State.prototype.executeDefValues = function(aDefValues) {
    this.pushControl(InstallDefValues(this, aDefValues.ids));
    this.pushControl(aDefValues.body);
};




var InstallDefValues = function(that, ids) {
    return function(state) {
	var bodyValue = that.popValue();
	if (bodyValue instanceof ValuesWrapper) {
	    if (ids.length !== bodyValue.elts.length) {
		throw new Error("define-vlues: expected " + ids.length 
				+ " values, but received " 
				+ bodyValue.elts.length);
	    }
	    for (var i = 0; i < ids.length; i++) {
		that.setPrefix(ids[i].depth.toFixnum(),
			       ids[i].pos.toFixnum(), 
			       bodyValue.elts[i]);
	    }
	}

	if (ids.length !== 1) {
	    throw new Error("define-values: expected " + ids.length 
			    + " values, but only received one.");
	} else {
	    that.setPrefix(ids[0].depth.toFixnum(),
			   ids[0].pos.toFixnum(),
			   bodyValue);
	}
    }
};



// executeApplyValues: applyValues -> void
State.prototype.executeApplyValues = function(anApplyValues) {
    this.pushControl(CallApplyValuesFunction());
    this.pushControl(anApplyValues.proc);
    this.pushControl(anApplyValues['args-expr']);
};

var CallApplyValuesFunction = function() {
    return function(state) {
	sys.p(state);
	var procValue = state.popValue();
	if (state.peekValue() instanceof ValuesWrapper) {
	    // FIXME
	} else {
	    sys.print("procedure: "); sys.p(procValue); sys.print("\n");
	    sys.print("argument: "); sys.p(state.peekValue()); sys.print("\n");
	    sys.print("I should be calling the function right now.");
	    // FIXME: actually evaluate something here, and put it on the stack.
	    
	    state.pushControl(procValue.body);
	    state.pushValue(42);	// FIXME!
	}
    };
}



State.prototype.evaluateIndirect = function(anIndirect) {
    return this.heap[anIndirect.value.val];
};


State.prototype.evaluateToplevel = function(aToplevel) {
    return this.refPrefix(aToplevel.depth.toFixnum(),
			  aToplevel.pos.toFixnum());
};


State.prototype.evaluateConstant = function(aConstant) {
    return aConstant.value;
}




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