// For node.js.
var sys = require('sys');
var types = require('./types');
var primitive = require('./primitive');

var DEBUG_ON = true;

var debug = function(s) {
    if (DEBUG_ON) {
	sys.debug(s);
    }
}



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


State.prototype.FRAME_POP = function(state) {
    debug("FRAME_POP");
    state.popValue();
};


State.prototype.FRAME_PUSH = function(state) {
    debug("FRAME_PUSH");
    state.pushValue(new types.UndefinedValue());
};


State.prototype.BRANCH = function(thenPart, elsePart) {
    return function(state) {
	debug("BRANCH");
	if (state.v) {
	    state.pushControl(thenPart);
	} else {
	    state.pushControl(elsePart);
	}
    };
};


State.prototype.SET = function(depth) {
    return function(state) {
	debug("SET " + depth);
	if (state.vstack.length - 1 - (depth || 0) < 0) {
	    throw new Error("vstack not long enough");
	}
	state.vstack[state.vstack.length - 1 - (depth || 0)] = state.v;
    };
};

State.prototype.SWAP = function(depth) {
    return function(state) {
	debug("SWAP " + depth);
	if (state.vstack.length - 1 - (depth || 0) < 0) {
	    throw new Error("vstack not long enough");
	}
	var tmp = state.vstack[state.vstack.length - 1 - (depth || 0)];
	state.vstack[state.vstack.length - 1 - (depth || 0)] = state.v;
	state.v = tmp;
    };
};


State.prototype.CALL_FOR_APPLY_VALUES = function(state) {
    debug("CALL_FOR_APPLY_VALUES");
    var procValue = state.getValue();
    var argValue = state.popValue();
    if (argValue instanceof types.ValuesWrapper) {
	throw new Error("not implemented yet");
    } else {
	// Pop off the argument at the end.
	state.pushControl(state.FRAME_POP);

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


// Assumes intermediate arguments on the stack, procedure on the
// value register.
State.prototype.CALL = function(n) {
    return function(state) {
	debug("CALL " + n);
	var procValue = state.getValue();

	if (primitive.isPrimitive(procValue)) {
	    var args = preparePrimitiveArguments(state, procValue, n);
	    var result = procValue.impl.apply(procValue.impl, args);
	    if (! procValue.usesState) {
		state.setValue(result);
	    }
	} else if (procValue instanceof types.ClosureValue) {
	    throw new Error("not handled yet");
	    // At this point, arguments on are on the stack.
	    // Handle rest arguments
	    if (procValue.isRest) {
		throw new Error("Not implemented yet")
	    } else {
	    }
	    state.pushControl(procValue.body);
	} else {
	    throw new Error("Procedure is neither primitive nor a closure");
	}
    };
};

var preparePrimitiveArguments = function(state, primitiveValue, n) {
    var args = [];

    if (primitiveValue.usesState) {
	args.push(state);
    }

    if (n < primitiveValue.arity) {
	throw new Error("arity error: expected at least "
			+ primitiveValue.arity + " arguments, but "
			+ "received " + n + " arguments instead.");
    }
    if (primitiveValue.isRest) {
	for(var i = 0; i < primitiveValue.arity; i++) {
	    args.push(state.popValue());
	}
	var restArgs = [];
	for(var i = 0; i < n - primitiveValue.arity; i++) {
	    restArgs.push(state.popValue());
	}
	args.push(restArgs);
    } else {
	if (primitiveValue.arity !== n) {
	    throw new Error("arity error: expected " 
			    + primitiveValue.arity 
			    + " but received " + n);
	}
	for(var i = 0; i < primitiveValue.arity; i++) {
	    args.push(state.popValue());
	}
    }
    return args;
};




var DEF_VALUES = function(ids) {
    return function(state) {
	debug("DEF_VALUES");
	var bodyValue = state.getValue();
	if (bodyValue instanceof types.ValuesWrapper) {
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
	debug("\n\nstate:\n");
	debug(sys.inspect(state));

	state.step();
    }

    debug("\n\nFinished with: \n")
    debug(sys.inspect(state));
    debug("\n\nfinal value: ")
    debug(sys.inspect(state.v));
    
    return state.v;
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
	newPrefix.push(new types.UndefinedValue());
    }
    this.pushValue(newPrefix);
};




State.prototype.step = function() {
    var nextCode = this.popControl();
    if (typeof(nextCode) === 'function')
	nextCode.call(this, this);
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
	case 'localref':
	    this.setValue(this.evaluateLocalRef(nextCode));
	    break;
	case 'primval':
	    this.setValue(this.evaluatePrimval(nextCode));
	    break;
	case 'branch':
	    this.executeBranch(nextCode);
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
    return new types.ClosureValue(this, lam);
};


// executeDefValues: defValues -> void
State.prototype.executeDefValues = function(aDefValues) {
    this.pushControl(DEF_VALUES(aDefValues.ids));
    this.pushControl(aDefValues.body);
};






// executeApplyValues: applyValues -> void
State.prototype.executeApplyValues = function(anApplyValues) {
    this.pushControl(this.CALL_FOR_APPLY_VALUES);
    this.pushControl(this.SWAP(0));
    this.pushControl(anApplyValues['args-expr']);
    this.pushControl(this.SET(0));
    this.pushControl(this.FRAME_PUSH);
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


State.prototype.evaluateLocalRef = function(aLocalRef) {
    var val = this.peekValue(aLocalRef.pos.toFixnum());
    if (aLocalRef['unbox?']) {
	val = val.unbox();
    }
    return val;
};

State.prototype.evaluatePrimval = function(aPrimval) {
    var prim = primitive.getPrimitive(aPrimval.value.val);
    if (! prim) {
	throw new Error("Primitive " + aPrimval.value.val 
			+ " not implemented!");
    }
    return prim;
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
    this.pushControl(this.CALL(rands.length));

    if (rands.length !== 0) {
	this.pushControl(this.SWAP(rands.length-1));
    }

    for (var i = rands.length-1; i >= 0; i--) {
	if (i !== rands.length - 1)
	    this.pushControl(this.SET(i));
	this.pushControl(anApp.rands[i]);
    }
    
    if (rands.length !== 0) {
	this.pushControl(this.SET(rands.length-1));
    }
    this.pushControl(anApp.rator);
    for (var i = 0; i < rands.length; i++) {
	this.pushControl(this.FRAME_PUSH);
    }
};


State.prototype.executeBranch = function(aBranch) {
    this.pushControl(this.BRANCH(aBranch['then'],
				 aBranch['else']));
    this.pushControl(aBranch.test);
}






//////////////////////////////////////////////////////////////////////






//////////////////////////////////////////////////////////////////////


exports.load = load;
exports.run = run;