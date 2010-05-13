// For node.js.
var sys = require('sys');
var types = require('./types');
var primitive = require('./primitive');
var assert = require('assert');

var DEBUG_ON = false;

var debug = function(s) {
    if (DEBUG_ON) {
	sys.debug(s);
    }
}



//////////////////////////////////////////////////////////////////////


var Prefix = function() {
    this.slots = [];
};

Prefix.prototype.addSlot = function() {
    this.slots.push(new types.UndefinedValue());
};

Prefix.prototype.ref = function(n) {
    return this.slots[n];
};

Prefix.prototype.set = function(n, v) {
    this.slots[n] = v;
};

Prefix.prototype.length = function() { 
    return this.slots.length;
};


//////////////////////////////////////////////////////////////////////



// Interpreter
var State = function() {
    // FIXME: missing the value register.
    this.v = [];    // value register
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack

    this.globals = new Prefix(); // prefix of globals

    this.heap = {};
};


// Set the value of the value register.
State.prototype.setValueRegister = function(v) {
    this.v = v;
};


State.prototype.getValueRegister = function() {
    return this.v;
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
    this.vstack.push(aVal);
};


// Pop a value.
State.prototype.popValue = function() {
    if (this.vstack.length === 0) {
 	throw new Error("vstack empty");
    }
    return this.vstack.pop();
};

// Push n undefined values onto the stack.
State.prototype.pushn = function(n) {
    for (var i = 0; i < n; i++) {
	this.vstack.push(new types.UndefinedValue());
    }
};

// Pop n values from the stack.
State.prototype.popn = function(n) {
    for (var i = 0; i < n; i++) {
	if (this.vstack.length === 0) {
 	    throw new Error("vstack empty");
	}
	this.vstack.pop();
    }
};


// Peek at the nth value on the stack.
State.prototype.peekValue = function(depth) {
    if (this.vstack.length - 1 - (depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    return this.vstack[this.vstack.length - 1 - (depth || 0)];
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


//////////////////////////////////////////////////////////////////////

// Here are the other values that will appear on the control stack when
// we need to do some extra work.

State.prototype.PUSHN = function(n) { 
    return new PushControl(n);
};
var PushControl = function(n) {
    this.n = n;
};
PushControl.prototype.invoke = function(state) {
    state.pushn(this.n);
};



State.prototype.POPN = function(n) {
    return new PopControl(n);
};
var PopControl = function(n) { 
    this.n = n;
}
PopControl.prototype.invoke = function(state) {
    state.popn(this.n);
};



State.prototype.BRANCH = function(thenPart, elsePart) {
    return new BranchControl(thenPart, elsePart);
};

var BranchControl = function(thenPart, elsePart) {
    this.thenPart = thenPart;
    this.elsePart = elsePart;
}
BranchControl.prototype.invoke = function(state) {
    debug("BRANCH");
    if (state.v) {
	state.pushControl(this.thenPart);
    } else {
	state.pushControl(this.elsePart);
    }
};


State.prototype.SET = function(depth) {
    return new SetControl(depth);
};
var SetControl = function(depth) {
    this.depth = depth;
};
SetControl.prototype.invoke = function(state) {
    debug("SET " + this.depth);
    if (state.vstack.length - 1 - (this.depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    state.vstack[state.vstack.length - 1 - (this.depth || 0)] = state.v;
};



State.prototype.SWAP = function(depth) {
    return new SwapControl(depth);
}
var SwapControl = function(depth) {
    this.depth = depth;
};

SwapControl.prototype.invoke = function(state) {
    debug("SWAP " + this.depth);
    if (state.vstack.length - 1 - (this.depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    var tmp = state.vstack[state.vstack.length - 1 - (this.depth || 0)];
    state.vstack[state.vstack.length - 1 - (this.depth || 0)] = state.v;
    state.v = tmp;
};



// Assumes intermediate arguments on the stack, procedure on the
// value register.
State.prototype.CALL = function(n) {
    return new CallControl(n);
}
var CallControl = function(n) {
    this.n = n;
};
CallControl.prototype.invoke = function(state) {
    debug("CALL " + this.n);

    var procValue = state.getValueRegister();
    var operandValues = [];
    for (var i = 0; i < this.n; i++) {
	operandValues.push(state.popValue());
    }

    // TODO
    // If the call is in tail position (i.e. the next control is a POPN),
    // then let's do tail call optimization.
    if (atTailPosition(state)) {
	doTailCallOptimization(state);
    }
    
    if (primitive.isPrimitive(procValue)) {
	var args = preparePrimitiveArguments(state, 
					     procValue, 
					     operandValues,
					     this.n);
	var result = procValue.impl.apply(procValue.impl, args);
	if (! procValue.usesState) {
	    state.setValueRegister(result);
	}
	return;
    }

    // TODO: case lambda support

    if (procValue instanceof types.ClosureValue) {
	var argCount = 0;
	if (procValue.isRest) {
	    var restArg = types.EMPTY;
	    for (var i = 0; i < procValue.numParams - n; i++) {
		types.cons(operandValues.pop(), restArg);
	    }
	    state.pushValue(restArg);
	    argCount++;
	}
	
	for (var i = operandValues.length -1; i >= 0; i--) {
	    state.pushValue(operandValues[i]);
	    argCount++;
	}
	
	for(var i = procValue.closureVals.length-1; i >= 0; i--) {
	    state.pushValue(procValue.closureVals[i]);
	    argCount++;
	}

	var cmds = [];
	cmds.push(procValue.body);
	cmds.push(state.POPN(argCount));
	state.pushManyControls(cmds);

	return;
} 
    
    throw new Error("Procedure is neither primitive nor a closure");

};



var atTailPosition = function(state) {
    return (state.cstack.length !== 0 && 
	    state.cstack[state.cstack.length - 1] instanceof PopControl);
};


var doTailCallOptimization = function(state) {
    assert.ok(atTailPosition(state));
    var popnControl = state.cstack.pop();
    popnControl.invoke(state);
};



var preparePrimitiveArguments = function(state, primitiveValue, operandValues, n) {
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
	    args.push(operandValues.shift());
	}
	var restArgs = [];
	for(var i = 0; i < n - primitiveValue.arity; i++) {
	    restArgs.push(operandValues.shift());
	}
	args.push(restArgs);
    } else {
	if (primitiveValue.arity !== n) {
	    throw new Error("arity error: expected " 
			    + primitiveValue.arity 
			    + " but received " + n);
	}
	for(var i = 0; i < primitiveValue.arity; i++) {
	    args.push(operandValues.shift());
	}
    }
    return args;
};




var DEF_VALUES = function(ids) {
    return new DefValuesControl(ids);
};

var DefValuesControl = function(ids) {
    this.ids = ids;
};

DefValuesControl.prototype.invoke = function(state) {
    debug("DEF_VALUES");
    var bodyValue = state.getValueRegister();
    if (bodyValue instanceof types.ValuesWrapper) {
	if (this.ids.length !== bodyValue.elts.length) {
	    throw new Error("define-values: expected " + this.ids.length 
			    + " values, but received " 
			    + bodyValue.elts.length);
	}
	for (var i = 0; i < this.ids.length; i++) {
	    state.setPrefix(this.ids[i].depth.toFixnum(),
			    this.ids[i].pos.toFixnum(), 
			    bodyValue.elts[i]);
	}
    }

    if (this.ids.length !== 1) {
	throw new Error("define-values: expected " + this.ids.length 
			+ " values, but only received one.");
    } else {
	state.setPrefix(this.ids[0].depth.toFixnum(),
			this.ids[0].pos.toFixnum(),
			bodyValue);
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



// run: state -> scheme-value
State.prototype.run = function() {
    var state = this;
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
    var newPrefix = new Prefix();
    for (var i = 0; i< prefix['toplevels'].length + numLifts; i++) {
	newPrefix.addSlot();
    }
    this.vstack.push(newPrefix);
};




State.prototype.step = function() {
    var nextCode = this.popControl();
    if (typeof(nextCode) === 'object' && nextCode['invoke'])
	nextCode.invoke(this);
    else {
	switch(nextCode.$) {
	case 'mod':
	    this.executeMod(nextCode);
	    break;
	case 'def-values':
	    this.executeDefValues(nextCode);
	    break;
	case 'indirect':
	    this.setValueRegister(this.evaluateIndirect(nextCode));
	    break;
	case 'apply-values':
	    this.executeApplyValues(nextCode);
	    break;
	case 'toplevel':
	    this.setValueRegister(this.evaluateToplevel(nextCode));
	    break;
	case 'constant':
	    this.setValueRegister(this.evaluateConstant(nextCode));
	    break;
	case 'seq':
	    this.executeSeq(nextCode);
	    break;
	case 'application':
	    this.executeApplication(nextCode);
	    break;
	case 'localref':
	    this.setValueRegister(this.evaluateLocalRef(nextCode));
	    break;
	case 'primval':
	    this.setValueRegister(this.evaluatePrimval(nextCode));
	    break;
	case 'branch':
	    this.executeBranch(nextCode);
	    break;
	case 'lam':
	    this.setValueRegister(this.evaluateLam(nextCode));
	    break;
	default:
	    throw new Error("I don't know how to handle " + sys.inspect(nextCode));
	}
    }
};




// executeMod: mod -> void
State.prototype.executeMod = function(mod) {
    this.processPrefix(mod.prefix);
    var cmds = [];
    for(var i = 0; i < mod.body.length-1; i++) {
	cmds.push(mod.body[i]);
    }
    this.pushManyControls(cmds);
};


// evaluateLam: lam -> scheme-runtime-value
// Evaluates the lam form and returns a scheme runtime value.
State.prototype.evaluateLam = function(lam) {
    var numParams = lam['num-params'].toFixnum();
    var paramTypes = lam["param-types"];
    var isRest = lam['rest?'];
    var closureVals = [];
    var body = lam.body;
    for (var i = 0; i < lam['closure-map'].length; i++) {
	var val, type;
	val = this.peekValue(lam['closure-map'][i].toFixnum());
	type = lam['closure-types'][i];
	// FIXME: look at the type; will be significant?
	closureVals.push(val);
    }
    return new types.ClosureValue(numParams, 
				  paramTypes, 
				  isRest, 
				  closureVals, 
				  body);
};




// executeDefValues: defValues -> void
State.prototype.executeDefValues = function(aDefValues) {
    var cmds = [];
    cmds.push(aDefValues.body);
    cmds.push(DEF_VALUES(aDefValues.ids))
    this.pushManyControls(cmds);

};




// executeApplyValues: applyValues -> void
State.prototype.executeApplyValues = function(anApplyValues) {
    var cmds = [];
    // FIXME: this is wrong: we have to squirrel away
    // some of the intermediate values to avoid stack space
    // usage.
    cmds.push(anApplyValues.proc);
    cmds.push(this.SET(0));
    // THIS IS WRONG
//     cmds.push(anApplyValues['args-expr']);
//     cmds.push(this.SWAP(0));
//     cmds.push(this.CALL(1));
    this.pushManyControls(cmds);
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
    var prim = primitive.getPrimitive(aPrimval.value+'');
    if (! prim) {
	throw new Error("Primitive " + aPrimval.value
			+ " not implemented!");
    }
    return prim;
};


State.prototype.executeSeq = function(aSeq) {
    var forms = aSeq.forms;
    var cmds = [];
    for (var i = 0; i < forms.length; i++) {
	cmds.push(forms[i]);
    }
    this.pushManyControls(cmds);
};



State.prototype.executeApplication = function(anApp) {
    var rator = anApp.rator;
    var rands = anApp.rands;


    // FIXME: allocate stack space on the current frame.
    var cmds = [];    
    cmds.push(this.PUSHN(rands.length));
    cmds.push(rator);    
    if (rands.length !== 0) {
	cmds.push(this.SET(rands.length-1));
    }

    for (var i = 0; i < rands.length; i++) {
	if (i !== rands.length - 1) {
	    cmds.push(anApp.rands[i]);
	    cmds.push(this.SET(i));
	} else {
	    cmds.push(anApp.rands[rands.length-1]);
	    cmds.push(this.SWAP(rands.length-1));
	}
    }
    cmds.push(this.CALL(rands.length));
    // CALL will be responsible for popping off the auxilliary
    // stack elements.

    this.pushManyControls(cmds);
};



State.prototype.executeBranch = function(aBranch) {
    var cmds = [];
    cmds.push(aBranch.test);
    cmds.push(this.BRANCH(aBranch['then'],
			  aBranch['else']));
    this.pushManyControls(cmds);
};



//////////////////////////////////////////////////////////////////////






//////////////////////////////////////////////////////////////////////


exports.load = load;
exports.run = function(state) { return state.run(); }
exports.State = State;
exports.Prefix = Prefix;