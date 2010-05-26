// For node.js.
var sys = require('sys');
var types = require('./types');
var primitive = require('./primitive');
var assert = require('assert');

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
	sys.debug(f_s);
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



// Interpreter
var State = function() {
    // FIXME: missing the value register.
    this.v = [];    // value register
    this.vstack = [];  // value stack
    this.cstack = [];  // control stack
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
};

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




State.prototype.SET_TOPLEVEL = function(depth, pos, isUndefOk) {
    return new SetToplevelControl(depth, pos, isUndefOk);
};
var SetToplevelControl = function(depth, pos, isUndefOk) {
    this.depth = depth;
    this.pos = pos;
    this.isUndefOk = isUndefOk;
};
SetToplevelControl.prototype.invoke = function(state) {
    debug("SET_TOPLEVEL " + this.depth + ", " + this.pos);
    if (state.vstack.length - 1 - (this.depth || 0) < 0) {
	throw new Error("vstack not long enough");
    }
    state.setPrefix(this.depth, this.pos, state.v)
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
    state.setn(this.depth, state.v);
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

    if (procValue instanceof types.CaseLambdaValue) {
	for (var j = 0; j < procValue.closures.length; j++) {
	    if (this.n === procValue.closures[j].numParams ||
		(this.n > procValue.closures[j].numParams && 
		 procValue.closures[j].isRest)) {
		procValue = procValue.closures[j];
		break;
	    }
	}
    }
    else if (procValue instanceof primitive.CasePrimitive) {
	for (var j = 0; j < procValue.cases.length; j++) {
	    if (this.n === procValue.cases[j].numParams ||
		(this.n > procValue.cases[j].numParams && 
		 procValue.cases[j].isRest)) {
		procValue = procValue.cases[j];
		break;
	    }
	}
    }


    if (primitive.isPrimitive(procValue)) {
	// Tail call optimization:
	if (state.cstack.length !== 0 && 
	    state.cstack[state.cstack.length - 1] instanceof PopControl) {
	    state.cstack.pop().invoke(state);
	}
	var args = preparePrimitiveArguments(state, 
					     procValue, 
					     operandValues,
					     this.n);
	var result = procValue.impl.apply(procValue.impl, args);
	if (! procValue.usesState) {
	    state.setValueRegister(result);
	}
    } else if (procValue instanceof types.ClosureValue) {
	// Tail call optimization
	if (state.cstack.length !== 0 && 
	    state.cstack[state.cstack.length - 1] instanceof PopControl) {
	    state.cstack.pop().invoke(state);
	    var argCount = prepareClosureArgumentsOnStack(state, 
							  procValue, 
							  operandValues,
							  this.n);
	    state.pushControl(state.POPN(argCount));
	    state.pushControl(procValue.body);

	} else if (state.cstack.length >= 2 &&
		   state.cstack[state.cstack.length - 1] instanceof ContMarkRecordControl &&
		   state.cstack[state.cstack.length - 2] instanceof PopControl) {
	    // Other tail call optimzation: if there's a continuation mark frame...
	    state.cstack[state.cstack.length - 2].invoke(state);
	    var argCount = prepareClosureArgumentsOnStack(state, 
							  procValue, 
							  operandValues,
							  this.n);
	    state.cstack[state.cstack.length - 2] = state.POPN(argCount);
	    state.pushControl(procValue.body);
	} else {
	    // General case:
	    var argCount = prepareClosureArgumentsOnStack(state, 
							  procValue, 
							  operandValues,
							  this.n);
	    state.pushControl(state.POPN(argCount));
	    state.pushControl(procValue.body);
	}
    } else if (procValue instanceof types.ContinuationClosureValue) {
	state.setValueRegister(operandValues[0]);
	state.vstack = procValue.vstack;
	state.cstack = procValue.cstack;
    } else {
	throw new Error("Procedure is neither primitive nor a closure");
    }
};


var prepareClosureArgumentsOnStack = function(state, procValue, operandValues, n) {
    var argCount = 0;
    if (procValue.isRest) {
	var restArg = types.EMPTY;
	for (var i = 0; i < n - procValue.numParams ; i++) {
	    restArg = types.cons(operandValues.pop(), restArg);
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
    return argCount;
}




var preparePrimitiveArguments = function(state, primitiveValue, operandValues, n) {
    var args = [];

    if (primitiveValue.usesState) {
	args.push(state);
    }

    if (n < primitiveValue.numParams) {
	throw new Error("arity error: expected at least "
			+ primitiveValue.numParams + " arguments, but "
			+ "received " + n + " arguments instead.");
    }
    if (primitiveValue.isRest) {
	for(var i = 0; i < primitiveValue.numParams; i++) {
	    args.push(operandValues.shift());
	}
	var restArgs = [];
	for(var i = 0; i < n - primitiveValue.numParams; i++) {
	    restArgs.push(operandValues.shift());
	}
	args.push(restArgs);
    } else {
	if (primitiveValue.numParams !== n) {
	    throw new Error("arity error: expected " 
			    + primitiveValue.numParams 
			    + " but received " + n);
	}
	for(var i = 0; i < primitiveValue.numParams; i++) {
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
	    state.setPrefix(this.ids[i].depth,
			    this.ids[i].pos,
			    bodyValue.elts[i]);
	}
    } else {
	if (this.ids.length !== 1) {
	    throw new Error("define-values: expected " + this.ids.length 
			    + " values, but only received one: " + bodyValue);
	} else {
	    state.setPrefix(this.ids[0].depth,
			    this.ids[0].pos,
			    bodyValue);
	}
    } 
};



var APPLY_VALUES_ARG = function(expr) {
    return new ApplyValuesArgControl(expr);
};
var ApplyValuesArgControl = function(expr) {
    this.expr = expr;
};
ApplyValuesArgControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.expr);
    cmds.push(APPLY_VALUES_APP(state.getValueRegister()));
    state.pushManyControls(cmds);

};



var APPLY_VALUES_APP = function(procVal) {
    return new ApplyValuesAppControl(procVal);
};

var ApplyValuesAppControl = function(procVal) {
    this.procVal = procVal;
};

ApplyValuesAppControl.prototype.invoke = function(state) {
    var exprValue = state.getValueRegister();
    state.setValueRegister(this.procVal);
    if (exprValue instanceof types.ValuesWrapper) {
	var elts = exprValue.elts;
	for(var i = elts.length - 1; i >= 0; i--) {
	    state.pushValue(elts[i]);
	}
	state.pushControl(state.CALL(elts.length));
    } else {
	state.pushValue(exprValue);
	state.pushControl(state.CALL(1));
    }

};


var BEG0_REST = function(rest) {
    return new Beg0Control(rest);
};

var Beg0Control = function(rest) {
    this.rest = rest;
};

Beg0Control.prototype.invoke = function(state) {
    // Rearrange the control stack so the rest of the
    // begin sequence will evaluate, followed by 
    // bringing the first expression's value back into
    // the value register.
    state.pushControl(makeConstant(state.getValueRegister()));
    state.pushManyControls(this.rest);
};

var makeConstant = function(v){
    return {$: 'constant', value: v};
};


var INSTALL_VALUE = function(count, pos, isBoxes, body) {
    return new InstallValueControl(count, pos, isBoxes, body);
};

var InstallValueControl = function(count, pos, isBoxes, body) {
    this.count = count;
    this.pos = pos;
    this.isBoxes = isBoxes;
    this.body = body;
};

InstallValueControl.prototype.invoke = function(state) {
    // The value's on the stack.  First check the proper number
    // of arguments.
    var aValue = state.getValueRegister();
    var vals = [];
    if (aValue instanceof types.ValuesWrapper) {
	if (this.count !== aValue.elts.length) {  
	    throw new Error("expected " + this.count 
			    + " values, but received " 
			    + aValue.elts.length)
	}
	vals = aValue.elts;
    } else {
	if (this.count !== 1) {
	    throw new Error("expected " + this.count 
			    + " values, but received"
			    + " one");
	}
	vals = [aValue];
    }
    if (this.isBoxes) {
	for (var i = 0; i < this.count; i++) {
	    state.peekn(i + this.pos).set(vals[i]);
	}
    } else {
	for (var i = 0; i < this.count; i++) {
	    state.setn(i + this.pos, vals[i]);
	}
    }
    state.pushControl(this.body);
};


var WITH_CONT_MARK_KEY = function(val, body) {
    return new WithContMarkKeyControl(val, body);
};

var WithContMarkKeyControl = function(val, body) {
    this.val = val;
    this.body = body;
};

WithContMarkKeyControl.prototype.invoke = function(state) {
    var evaluatedKey = state.getValueRegister();
    var cmds = [];
    cmds.push(this.val);
    cmds.push(new WithContMarkVal(evaluatedKey,
				  this.body));
    state.pushManyControls(cmds);
};

var WithContMarkVal = function(key, body) {
    this.key = key;
    this.body = body;
};

WithContMarkVal.prototype.invoke = function(state) {
    var evaluatedKey = this.key + "";
    var evaluatedVal = state.getValueRegister();
    // Check to see if there's an existing ContMarkRecordControl
    if (state.cstack.length !== 0 && 
	(state.cstack[state.cstack.length - 1] instanceof 
	 ContMarkRecordControl)) {
	state.pushControl(state.cstack.pop().update
			  (evaluatedKey, evaluatedVal));
    } else {
	var aHash = {};
	aHash[evaluatedKey] = evaluatedVal;
	state.pushControl(new ContMarkRecordControl(aHash));
    }
    state.pushControl(this.body);
};

var ContMarkRecordControl = function(dict) {
    this.dict = dict || {};
};

ContMarkRecordControl.prototype.invoke = function(state) {
    // No-op: the record will simply pop off the control stack.
};

ContMarkRecordControl.prototype.update = function(key, val) {
    var newDict = {};
    // FIXME: what's the javascript idiom for hash key copy?
    // Maybe we should use a rbtree instead?
    for (var k in this.dict) {
	if (this.dict.hasOwnProperty(key)) {
	    newDict[k] = this.dict[key];
	}
    }
    newDict[key] = val;
    return new ContMarkRecordControl(newDict);
};




var CASE_LAMBDA_COMPUTE = function(name, lamsToEvaluate, evaluatedLams) {
    return new CaseLambdaComputeControl(name, lamsToEvaluate, evaluatedLams);
};

var CaseLambdaComputeControl = function(name, lamsToEvaluate, evaluatedLams) {
    this.name = name;
    this.lamsToEvaluate = lamsToEvaluate;
    this.evaluatedLams = evaluatedLams;
};


CaseLambdaComputeControl.prototype.invoke = function(state) {
    var nextEvaluatedLam = state.getValueRegister();
    if (this.lamsToEvaluate.isEmpty()) {
	var clauseList = (types.cons(nextEvaluatedLam, this.evaluatedLams)).reverse();
	var clauses = [];
	while (!clauseList.isEmpty()) {
	    clauses.push(clauseList.first());
	    clauseList = clauseList.rest();
	}
	state.setValueRegister(
	    new types.CaseLambdaValue(this.name, clauses));
    } else {
	state.pushControl(new CaseLambdaComputeControl(
	    this.name,
	    this.lamsToEvaluate.rest(),
	    types.cons(nextEvaluatedLam,
		       this.evaluatedLams)));
	state.pushControl(this.lamsToEvaluate.first());
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

    // TODO: do some processing of the bytecode so that all the
    // constants are native, enable quick dispatching based on
    // bytecode type, rewrite the indirect loops, etc...
};



// run: state -> scheme-value
State.prototype.run = function() {
    var state = this;
    while(! state.isStuck()) {
	debug("\n\nstate:\n");
	// NOTE: sys.inspect is expensive, so we want to 
	// control the order of evaluation within this tight
	// loop.
	debugF(function() { sys.inspect(state) });

	state.step();
    }

    debug("\n\nFinished with: \n")
    debugF(function() { sys.inspect(state) });
    debug("\n\nfinal value: ")
    debugF(function() { sys.inspect(state.v) });
    
    return state.v;
};



State.prototype.processIndirects = function(indirects) {
    for (var i = 0 ;i < indirects.length; i++) {
	var anIndirect = indirects[i];
	this.heap[anIndirect.id] = this.evaluateLam(anIndirect.lam);
    }
};


State.prototype.processPrefix = function(prefix) {
    var numLifts = prefix['num-lifts'];
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
	case 'let-one':
	    this.executeLetOne(nextCode);
	    break;
	case 'let-void':
	    this.executeLetVoid(nextCode);
	    break;
	case 'beg0':
	    this.executeBeg0(nextCode);
	    break;
	case 'boxenv':
	    this.executeBoxenv(nextCode);
	    break;
	case 'install-value':
	    this.executeInstallValue(nextCode);
	    break;
	case 'with-cont-mark':
	    this.executeWithContMark(nextCode);
	    break;
	case 'assign':
	    this.executeAssign(nextCode);
	    break;
	case 'varref':
	    this.executeVarref(nextCode);
	    break;
	case 'closure':
	    this.executeClosure(nextCode);
	    break;
	case 'case-lam':
	    this.executeCaseLam(nextCode);
	    break;
	case 'let-rec':
	    this.executeLetRec(nextCode);
	    break;
	default:
	    throw new Error("I don't know how to handle " 
			    + sys.inspect(nextCode));
	}
    }
};




// executeMod: mod -> void
State.prototype.executeMod = function(mod) {
    this.processPrefix(mod.prefix);
    var cmds = [];
    for(var i = 0; i < mod.body.length; i++) {
	cmds.push(mod.body[i]);
    }
    this.pushManyControls(cmds);
};


// evaluateLam: lam -> scheme-runtime-value
// Evaluates the lam form and returns a scheme runtime value.
State.prototype.evaluateLam = function(lam) {
    var numParams = lam['num-params'];
    var paramTypes = lam["param-types"];
    var isRest = lam['rest?'];
    var closureVals = makeClosureValsFromMap(this,
					     lam['closure-map'], 
					     lam['closure-types']);
    var body = lam.body;
    return new types.ClosureValue(numParams, 
				  paramTypes, 
				  isRest, 
				  closureVals, 
				  body);
};


// makeClosureValsFromMap: state [number ...] -> [scheme-value ...]
// Builds the array of closure values, given the closure map and its
// types.
var makeClosureValsFromMap = function(state, closureMap, closureTypes) {
    var closureVals = [];
    for (var i = 0; i < closureMap.length; i++) {
	var val, type;
	val = state.peekn(closureMap[i]);
	type = closureTypes[i];
	// FIXME: look at the type; will be significant?
	closureVals.push(val);
    }
    return closureVals;
};


State.prototype.executeLetOne = function(aLetOne) {
    var cmds = [];
    this.pushn(1);
    cmds.push(aLetOne.rhs);
    cmds.push(this.SET(0));
    cmds.push(aLetOne.body);
    cmds.push(this.POPN(1));
    this.pushManyControls(cmds);
};


State.prototype.executeLetVoid = function(aLetVoid) {
    var cmds = [];
    var n = aLetVoid['count'];
    this.pushn(n);
    if (aLetVoid['boxes?']) {
	for (var i = 0; i < n; i++) {
	    this.setn(i, new types.Box(new types.UndefinedValue()));
	}
    }
    cmds.push(aLetVoid.body);
    cmds.push(this.POPN(n));
    this.pushManyControls(cmds);
};


State.prototype.executeLetRec = function(aLetrec) {
    var cmds = [];
    var n = aLetrec.procs.length;
    this.pushn(n);
    for (var i = 0; i < n; i++) {
	cmds.push(aLetrec.procs[i]);
	cmds.push(this.SET(n - 1 - i));
    }
    cmds.push(new LetrecReinstallClosureControls(aLetrec.procs));
    cmds.push(aLetrec.body);
    cmds.push(this.POPN(n));
    this.pushManyControls(cmds);
};

var LetrecReinstallClosureControls = function(procs) {
    this.procs = procs;
};

LetrecReinstallClosureControls.prototype.invoke = function(state) {
    // By this point, all the closures in this.proc are installed, but
    // their closures need to be refreshed.
    var n = this.procs.length;
    for (var i = 0; i < n; i++) {
	var procRecord = this.procs[i];
	var closureVal = state.peekn(n - 1 - i);
	closureVal.closureVals = makeClosureValsFromMap(state, 
							procRecord['closure-map'],
							procRecord['closure-types']);
    }  
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
    cmds.push(APPLY_VALUES_ARG(anApplyValues['args-expr']));
    this.pushManyControls(cmds);
};



State.prototype.evaluateIndirect = function(anIndirect) {
    return this.heap[anIndirect.value+''];
};

State.prototype.executeClosure = function(aClosure) {
    this.v = this.heap[aClosure['gen-id']+''];
};


State.prototype.evaluateToplevel = function(aToplevel) {
    return this.refPrefix(aToplevel.depth, aToplevel.pos);
};


State.prototype.evaluateConstant = function(aConstant) {
    return aConstant.value;
};


State.prototype.evaluateLocalRef = function(aLocalRef) {
    var val = this.peekn(aLocalRef.pos);
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


State.prototype.executeBeg0 = function(aBeg0) {
    if (aBeg0.seq.length === 1) {
	this.pushControl(aBeg0.seq[0]);
    } else {
	var rest = [];
	for (var i = 1; i < aBeg0.seq.length; i++) {
	    rest.push(aBeg0.seq[i]);
	}
	this.pushManyControls([aBeg0.seq[0], BEG0_REST(rest)]);
    }
};


State.prototype.executeBoxenv = function(aBoxenv) {
    this.setn(aBoxenv.pos,
	      new types.Box(this.peekn(aBoxenv.pos)));
    this.pushControl(aBoxenv.body);
};


State.prototype.executeInstallValue = function(anInstallValue) {
    var cmds = [];
    cmds.push(anInstallValue.rhs);
    cmds.push(INSTALL_VALUE(anInstallValue.count,
			    anInstallValue.pos,
			    anInstallValue['boxes?'],
			    anInstallValue.body));
    this.pushManyControls(cmds);
};



State.prototype.executeWithContMark = function(withContMark) {
    var cmds = [];
    cmds.push(withContMark.key);
    cmds.push(WITH_CONT_MARK_KEY(withContMark.val,
				 withContMark.body));
    this.pushManyControls(cmds);
};


State.prototype.executeAssign = function(anAssign) {
    var cmds = [];
    cmds.push(anAssign.rhs);
    cmds.push(this.SET_TOPLEVEL(anAssign.id.depth,
				anAssign.id.pos,
				anAssign['undef-ok?']));
    this.pushManyControls(cmds);
};

State.prototype.executeVarref = function(aVarref) {
    var depth, pos;
    depth = aVarref.toplevel.depth;
    pos = aVarref.toplevel.pos;
    assert.ok(this.vstack.length - 1 - depth >= 0);
    assert.ok(this.vstack[this.vstack.length - 1 - depth] instanceof Prefix);
    this.v = new VariableReference(this.vstack[this.vstack.length - 1 - depth],
				   pos);
};



State.prototype.executeCaseLam = function(aCaseLam) {
    var clauses = aCaseLam.clauses;
    if (clauses.length === 0) {
	state.v = new types.CaseLambdaValue(aCaseLam.name, []);
    } else {
	this.pushControl(CASE_LAMBDA_COMPUTE(aCaseLam.name, 
					     types.list(clauses).rest(),
					     types.list([])));
	this.pushControl(clauses[0]);
    }
};



//////////////////////////////////////////////////////////////////////

primitive.addPrimitive(
    "apply",
    new primitive.Primitive('apply', 
			    2, 
			    true, true,
			    function(state, f, firstArg, restArgs) {
				var numArgs = 0;
				restArgs.unshift(firstArg);
				if (restArgs.length > 0) {
				    var lastLst = 
					restArgs[restArgs.length - 1].reverse();
				    while (! lastLst.isEmpty()) {
					state.pushValue(lastLst.first());
					numArgs++;
					lastLst = lastLst.rest();
				    }
				}
				for (var i = restArgs.length - 2; i >= 0; i--) {
				    state.pushValue(restArgs[i]);
				    numArgs++;
				}

				state.setValueRegister(f);
				state.pushControl(state.CALL(numArgs));
			    }));


primitive.addPrimitive('values',
		       new primitive.Primitive(
			   'values', 
			   1, 
			   true, false,
			   function(firstArg, restArgs) {
			       var bundledArgs = [firstArg].concat(restArgs);
			       if (bundledArgs.length === 1) {
				   return firstArg;
			       }
			       return new types.ValuesWrapper(bundledArgs);
			   }));




primitive.addPrimitive('call/cc', 
		       new primitive.Primitive('call/cc',
					       1,
					       false, true,
					       function(state, f) {
						   var continuationClosure = 
						       captureContinuationClosure(state);
						   state.pushValue(continuationClosure);
						   state.setValueRegister(f);
						   state.pushControl(state.CALL(1));
					       }));


var captureContinuationClosure = function(state) {
    return new types.ContinuationClosureValue(state.vstack,
					      state.cstack);
};



//////////////////////////////////////////////////////////////////////


exports.load = load;
exports.run = function(state) { return state.run(); }
exports.State = State;
exports.Prefix = Prefix;
exports.VariableReference = VariableReference;
exports.ContMarkRecordControl = ContMarkRecordControl;

exports.setDebug = setDebug;