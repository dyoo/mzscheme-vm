// Control structures

/*
var sys = require('sys');
var types = require('./types');
var primitive = require('./primitive');
var types = require('./types');



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
*/



var control = {};


(function() {


    var PrimProc = types.PrimProc;
    var isFunction = types.isFunction;
    var isInternalCall= types.isInternalCall;
    var isInternalPause = types.isInternalPause;


//////////////////////////////////////////////////////////////////////



var labelCounter = 0;
var makeLabel = function(l) {
    return l + (labelCounter++);
}



//////////////////////////////////////////////////////////////////////
// Set
// Setting stack values.

var SetControl = function(depth) {
    this.depth = depth;
};

SetControl.prototype.invoke = function(aState) {
//    debug("SET " + this.depth);
    if (aState.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  aState.captureCurrentContinuationMarks(aState));
    }
    aState.setn(this.depth, aState.v);
};



//////////////////////////////////////////////////////////////////////
// Push a value into the nth position on the stack
var PushnControl = function(n) {
    this.n = n;
};
PushnControl.prototype.invoke = function(aState) {
    aState.pushn(this.n);
};



var SwapControl = function(depth) {
    this.depth = depth;
};

SwapControl.prototype.invoke = function(aState) {
    if (aState.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  state.captureCurrentContinuationMarks(aState));
    }
    var tmp = aState.vstack[aState.vstack.length - 1 - (this.depth || 0)];
    aState.vstack[aState.vstack.length - 1 - (this.depth || 0)] = aState.v;
    aState.v = tmp;
};





// Internal
// Pop n values
var PopnControl = function(n) { 
    this.n = n;
};

PopnControl.prototype.invoke = function(aState) {
    aState.popn(this.n);
};




//////////////////////////////////////////////////////////////////////














//////////////////////////////////////////////////////////////////////
// Modules

var Prefix = function(params) {
    this.numLifts = params.numLifts;
    this.toplevels = params.toplevels;
};


// ModControl: prefix (arrayof require) code -> ModControl
var ModControl = function(prefix, requires, body) {
    this.prefix = prefix;
    this.requires = requires;
    this.body = body;
};

ModControl.prototype.invoke = function(state) {
    var prefixValue = processPrefix(state, this.prefix);
    var cstack = state.cstack;

    // After evaluating the body, do ModControlAfterControl.
    cstack.push(new ModControlAfterControl(prefixValue));

    // Add the body
    for(var i = this.body.length - 1; i >= 0; i--) {
	cstack.push(this.body[i]);
    }
    // Invoke all the requires before hitting the body.
    for (var i = this.requires.length - 1; i >= 0; i--) {
	cstack.push(new RequireControl(this.requires[i]));
    }
};


// After a module is evaluated, set the value register to the prefix.
var ModControlAfterControl = function(prefixValue) {
    this.prefixValue = prefixValue;
};

ModControlAfterControl.prototype.invoke = function(state) {
    state.v = this.prefixValue;
};


//////////////////////////////////////////////////////////////////////

var processPrefix = function(aState, prefix) {
    var numLifts = prefix.numLifts;
    var newPrefix = new types.PrefixValue();
    for (var i = 0; i < prefix.toplevels.length; i++) {
	var top = prefix.toplevels[i];
	if (top === false) {
	    newPrefix.addSlot();
	} else if (types.isSymbol(top)) {
	    installSymbolicVariable(aState, newPrefix, top);
	} else if (top['$'] === 'module-variable') {
	    installModuleVariable(aState, newPrefix, top);
	} else if (top['$'] === 'global-bucket') {
	    installGlobalBucket(aState, newPrefix, top);
	} else {
	    throw types.internalError("unable to install toplevel element " + top,
				      state.captureCurrentContinuationMarks(aState)); 
	}
    }
    for (var i = 0; i < numLifts; i++) {
	newPrefix.addSlot();
    }
    aState.vstack.push(newPrefix);
    return newPrefix;
};
    

var installSymbolicVariable = function(aState, newPrefix, sym) {
    newPrefix.addSlot(new types.NamedSlot(sym+'', types.UNDEFINED));
};

    

// Module variables are looked up and installed into the prefix.
var installModuleVariable = function(aState, newPrefix, top) {
    var resolvedModuleName = resolveModulePathIndex(top['modidx'], aState);
    var primName = top.sym + '';

    // FIXME: we should not be looking into the primitives
    // without inspecting the resolved module name, as not all
    // modules variables will refer to bindings in base.rkt anymore

    if (isHardcodedModule(resolvedModuleName)) {
	// FIXME: do something different based on which module this is referring to!
	var aPrim = primitive.getPrimitive(primName, resolvedModuleName);
	newPrefix.addSlot(new types.NamedSlot(primName, aPrim));
    } else {
	newPrefix.addSlot(new types.ModuleVariableRecord(resolvedModuleName,
							 primName));
    }
};


var installGlobalBucket = function(aState, newPrefix, top) {
    var name = top.value+'';
    if (! aState.globals[name]) {
	aState.globals[name] =
	    new types.GlobalBucket(name, types.UNDEFINED);
    } else {
	// Otherwise, do nothing but reuse the global bucket.
    }
    newPrefix.addSlot(aState.globals[name]);
};



// resolveModulePathIndex -> (resolved-module-path | false)
// A resolved module path is either a symbol or a primitive string.
// False is returned if the path is self.
var resolveModulePathIndex = function(modulePathIndex, aState) {
    var path = modulePathIndex['path'];
    var base = modulePathIndex['base'];    

    if (base === false) {
	// path is relative to an unspecified directory.
	return modulePathIndex['path'];
    } else if (typeof(base) === 'object' && base['$'] === 'module-path') {
	base = resolveModulePathIndex(base, aState);
    }

    // By this point, base is a resolved module path or false
    if (path === false) {
	return false;
    } else {
	// path is a module path
	if (types.isSymbol(path)) {
	    return path;
	} else if (types.isString(path)) {
	    if (base === false) {
		return path;
	    } else if (types.isSymbol(base)) {
		// fixme: this isn't quite right yet.
		return path;
	    } else {
		return simplifyPath(pathOnly(base) + path);
	    }
	} else {
	    // Defensive.  This should never happen.
	    throw types.internalError("unable to resolve " + path,
				      state.captureCurrentContinuationMarks(aState));
	}
    }
};


// simplifyPath: string -> string
var simplifyPath = function(aPath) {
    return aPath;
};


// pathOnly: string -> string
// pathOnly("hello/world.rkt")  should be "hello/"
// pathOnly("world.rkt")  should be ""
var pathOnly = function(aPath) {
    var chunks = aPath.split("/");
    if (chunks.length <= 1) {
	return "";
    }
    return (chunks.slice(0, chunks.length - 1).join('/') + '/');
};





//////////////////////////////////////////////////////////////////////
// Constants


var ConstantControl = function(value) {
    this.value = value;
};


ConstantControl.prototype.invoke = function(state) {
    state.v = this.value;
};





//////////////////////////////////////////////////////////////////////
// Branches


var BranchControl = function(test, thenPart, elsePart) {
    this.test = test;
    this.thenPart = thenPart;
    this.elsePart = elsePart;
};


BranchControl.prototype.invoke = function(aState) {
    aState.cstack.push(new BranchRestControl(
	this.thenPart, this.elsePart));
    aState.cstack.push(this.test);
};

var BranchRestControl = function(thenPart, elsePart) {
    this.thenPart = thenPart;
    this.elsePart = elsePart;
};


BranchRestControl.prototype.invoke = function(state) {
//    debug("BRANCH");
    if (state.v !== false && state.v !== undefined) {
	state.cstack.push(this.thenPart);
    } else {
	state.cstack.push(this.elsePart);
    }
};



//////////////////////////////////////////////////////////////////////
// Require statements
var RequireControl = function(name) {
    this.name = name;
};


RequireControl.prototype.invoke = function(aState) {
    var that = this;
    var resolvedModuleName = resolveModulePathIndex(this.name, aState);
    var onPause = function(caller, onSuccess, onFail) {
	invokeModuleAndRestart(aState, 
			       resolvedModuleName,
			       function() {
				   onSuccess(types.VOID) 
			       }, 
			       onFail);
    };
    throw new PauseException(onPause);
};



var hardcodedModules = [
    types.list([types.symbol("quote"), types.symbol("#%kernel")]),
    types.list([types.symbol("quote"), types.symbol("#%paramz")]),
    types.list([types.symbol("quote"), types.symbol("#%utils")]),
    types.symbol("moby/kernel"),
    types.symbol("moby/paramz"),
    types.symbol("moby/js-impl")];
    

var isHardcodedModule = function(resolvedModuleName) {
    for (var i = 0; i < hardcodedModules.length; i++) {
	if (types.isEqual(hardcodedModules[i], resolvedModuleName))
	    return true;
    }
    return false;
};



// invokeModuleAndRestart: state moduleRecord (-> void) -> void
// Invokes the given moduleRecord and restarts the parent evaluation.
// The invoked module is installed, along with its provides.
var invokeModuleAndRestart = function(aState, 
				      resolvedModuleName,
				      onSuccess, onFail) {
    
    // Defensive: check for invariant: run() must NOT be called
    // if we're already running.
    if (aState.running) {
	onFail(types.internalError(
	    "invokeModuleAndRestart: run() called in unsafe re-entrant context",
	    state.captureCurrentContinuationMarks(aState)));
	return;
    }


    // Check to see if we've already invoked.
    if (aState.invokedModules[resolvedModuleName] || 
	isHardcodedModule(resolvedModuleName)) {
	// Already invoked.
	onSuccess(types.VOID);
	return;
    }

    // Check that the module exists.
    if (typeof(MODULES) === 'undefined' ||
	typeof(MODULES[resolvedModuleName]) === 'undefined') {
	onFail(types.internalError(
	    "unable to require " + resolvedModuleName,
	    state.captureCurrentContinuationMarks(aState)));
	return;
    }

	
    var moduleRecord = MODULES[resolvedModuleName];
    // Finally, dispatch based on module record type.
    if (isJavascriptModuleRecord(moduleRecord)) {
	invokeJavascriptModuleAndRestart(
	    aState, resolvedModuleName, moduleRecord, onSuccess, onFail);
    } else {
	invokeSchemeModuleAndRestart(
	    aState, resolvedModuleName, moduleRecord, onSuccess, onFail);
    }
};


var InvokedModule = function(record, providedValues, prefix) {
    this.record = record;
    this.providedValues = providedValues;
    this.prefix = prefix;
};


InvokedModule.prototype.lookup = function(n) {
    return this.prefix.lookup(n);
};







var invokeSchemeModuleAndRestart = function(aState, resolvedModuleName, moduleRecord, onSuccess, onFail) {
    var newOnSuccess = function(modulePrefix) {
	var providedValues = {};

	var moduleControl = moduleRecord.bytecode.code;
	var provides = moduleControl.provides;

	for (var i = 0; i < provides.length; i++) {
	    var phaseVariablesSyntaxes = provides[i];
	    if (phaseVariablesSyntaxes.phase === 0) {
		var providedes = phaseVariablesSyntaxes.variables;
		for (var j = 0 ; j < providedes.length; j++) {
		    var provided = providedes[j];
		    if (provided['src']) {
			providedValues[provided['name']] = 
			    new types.ModuleVariableRecord(
				resolveModulePathIndex(provided['src'], aState),
				provided['src-name']);
		    } else {
			providedValues[provided['name']] = 
			    new types.ModuleVariableRecord(
				resolvedModuleName,
				provided['src-name']);
		    }
		}
	    } else {
		// skip other phases
	    }
	}	
	aState.invokedModules[moduleRecord.name] = 
	    new InvokedModule(moduleRecord, providedValues, modulePrefix);

	onSuccess(types.VOID);
    };

    aState.clearForEval({preserveBreak: true, clearGlobals: true});
    interpret.load(moduleRecord.bytecode, aState);

    aState.onSuccess = function(v) {
	newOnSuccess(v);
    };
    aState.onFail = function(e) {
	onFail(e);
    };

    interpret.run(aState);
};



var invokeJavascriptModuleAndRestart = function(aState, resolvedModuleName, 
						moduleRecord,
						onSuccess, onFail) {    
    var invokeAllRequiredModules = function(modules, after) {
	if (modules === types.EMPTY) {
	    after();
	} else {
	    invokeModuleAndRestart(aState,
				   modules.first+'',
				   function() {
				       invokeAllRequiredModules(modules.rest, after);
				   },
				   onFail);
	}
    }


    var afterRequiresInvoked = function() {
	var EXPORTS = {};
	try {
	    (moduleRecord.jsImplementation)(aState, EXPORTS);
	} catch (e) {
	    onFail(e);
	    return;
	}
	var providedValues = {};
	for (var i = 0 ; i < moduleRecord.provides.length; i++) {
	    providedValues[moduleRecord.provides[i]] =
		new types.ModuleVariableRecord(
		    resolvedModuleName,
		    moduleRecord.provides[i]);
	}
	var modulePrefix = { lookup : function(name) {
	    return EXPORTS[name];
	}};

	aState.invokedModules[moduleRecord.name] = 
	    new InvokedModule(moduleRecord, providedValues, modulePrefix);
	onSuccess(types.VOID);
    }

    invokeAllRequiredModules(types.list(moduleRecord.requires),
			     afterRequiresInvoked);
};




var isJavascriptModuleRecord = function(aModuleRecord) {
    return (typeof(aModuleRecord.jsImplementation) !== 'undefined');
}





//////////////////////////////////////////////////////////////////////
// Sequences


var SeqControl = function(forms) {
    this.formsRev = forms.slice(0).reverse();
};


SeqControl.prototype.invoke = function(state) {
    state.cstack.splice.apply(state.cstack,
			      [state.cstack.length,
			       0].concat(this.formsRev));
};



//////////////////////////////////////////////////////////////////////
// Beg0

var Beg0Control = function(seq) {
    this.seq = seq;
};

Beg0Control.prototype.invoke = function(state) {
    var rest;
    if (this.seq.length === 1) {
	state.cstack.push(this.seq[0]);
    } else {
	rest = this.seq.slice(1);
	rest.reverse();
	state.cstack.push(new Beg0RestControl(rest));
	state.cstack.push(this.seq[0]);
    }
};


var Beg0RestControl = function(restRev) {
    this.restRev = restRev;
};

Beg0RestControl.prototype.invoke = function(state) {
    // Rearrange the control stack so the rest of the
    // begin sequence will evaluate, followed by 
    // bringing the first expression's value back into
    // the value register.
    state.cstack.push(new ConstantControl(state.v));
    state.cstack.splice.apply(state.cstack,
			      [state.cstack.length,
			       0].concat(this.restRev));
};



//////////////////////////////////////////////////////////////////////
// Toplevel variable lookup

var ToplevelControl = function(depth, pos) {
    this.depth = depth;
    this.pos = pos;
    // FIXME: use isConst and isReady 
};

ToplevelControl.prototype.invoke = function(state) {
    state.v = state.refPrefix(this.depth, this.pos);
};



//////////////////////////////////////////////////////////////////////
// Local variable references

var LocalrefControl = function(pos, isUnbox) {
    this.pos = pos;
    this.isUnbox = isUnbox;
};

LocalrefControl.prototype.invoke = function(state) {
    var val = state.peekn(this.pos);
    if (this.isUnbox) {
	val = val.unbox();
    }
    state.v = val;
};



//////////////////////////////////////////////////////////////////////
// Primitive value lookup

var PrimvalControl = function(name) {
    this.name = name + '';
};

PrimvalControl.prototype.invoke = function(aState) {
    var prim = primitive.getPrimitive(this.name, undefined);
    if (! prim) {
	throw types.internalError("Primitive " + this.name + " not implemented!",
				  state.captureCurrentContinuationMarks(aState));
    }
    aState.v = prim;
};



//////////////////////////////////////////////////////////////////////
// Lambdas

var LamControl = function(params) {
    this.name = params.name;
    this.numParams = params.numParams;
    this.paramTypes = params.paramTypes;
    this.isRest = params.isRest;
    this.closureMap = params.closureMap;
    this.closureTypes = params.closureTypes;
    this.body = params.body;
};


LamControl.prototype.invoke = function(state) {
    state.v = new types.ClosureValue(this.name,
				     this.numParams, 
				     this.paramTypes, 
				     this.isRest, 
				     makeClosureValsFromMap(state,
							    this.closureMap,
							    this.closureTypes), 
				     this.body);
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



//////////////////////////////////////////////////////////////////////
// Letrec
// Recursive definitions.

var LetRecControl = function(procs, body) {
    this.procs = procs.concat([]);
    this.procs.reverse();
    this.body = body;
};

LetRecControl.prototype.invoke = function(aState) {
    var n = this.procs.length;
    var cstack = aState.cstack;
    cstack.push(this.body);
    cstack.push(new LetrecReinstallClosureControls(this.procs));
    for (var i = n-1; i >= 0; i--) {
	cstack.push(new SetControl(n - 1 - i));
	cstack.push(this.procs[i]);
    }
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
							procRecord.closureMap,
							procRecord.closureTypes);
    }  
};



//////////////////////////////////////////////////////////////////////
// Define Values


var DefValuesControl = function(ids, body) {
    this.ids = ids;
    this.body = body;
};


DefValuesControl.prototype.invoke = function(state) {
    state.cstack.push(new DefValuesInstallControl(this.ids))
    state.cstack.push(this.body);
};


var DefValuesInstallControl = function(ids) {
    this.ids = ids;
};

DefValuesInstallControl.prototype.invoke = function(aState) {
//    debug("DEF_VALUES");
    var bodyValue = aState.v;
    if (bodyValue instanceof types.ValuesWrapper) {
	if (this.ids.length !== bodyValue.elts.length) {
	    helpers.raise(
		types.exnFailContractArity("define-values: expected " + this.ids.length 
					   + " values, but received " + bodyValue.elts.length,
					   state.captureCurrentContinuationMarks(aState)));
	}
	for (var i = 0; i < this.ids.length; i++) {
	    aState.setPrefix(this.ids[i].depth,
			    this.ids[i].pos,
			    bodyValue.elts[i]);
	}
    } else {
	if (this.ids.length !== 1) {
	    helpers.raise(
		types.exnFailContractArity("define-values: expected " + this.ids.length 
					   + " values, but only received one: " + bodyValue,
					   state.captureCurrentContinuationMarks(aState)));
	} else {
	    aState.setPrefix(this.ids[0].depth,
			    this.ids[0].pos,
			    bodyValue);
	}
    } 
};



//////////////////////////////////////////////////////////////////////
// Procedure application

var ApplicationControl = function(rator, rands) {
    this.rator = rator;
    this.rands = rands;
};


ApplicationControl.prototype.invoke = function(state) {
    var rator = this.rator;
    var rands = this.rands;
    var randsLength = rands.length;
    var randsLengthSub1 = rands.length - 1;
    var cstack = state.cstack;
    var i;

    cstack.push(new CallControl(randsLength));
    for (i = randsLengthSub1; i >= 0; i--) {
	if (i !== randsLengthSub1) {
	    cstack.push(new SetControl(i));
	    cstack.push(rands[i]);
	} else {
	    cstack.push(new SwapControl(randsLengthSub1));
	    cstack.push(rands[randsLengthSub1]);
	}

    }
    if (randsLength !== 0) {
	cstack.push(new SetControl(randsLengthSub1));
    }
    cstack.push(rator);    

    // We allocate as many values as there are operands.
    if (randsLength !== 0) {
	cstack.push(new PushnControl(randsLength));
    }
};




var CallControl = function(n) {
    this.n = n;
};

CallControl.prototype.invoke = function(aState) {
    var operandValues;
    if (aState.vstack.length >= this.n) {
	operandValues = aState.vstack.splice(
	    aState.vstack.length - this.n,
	    this.n);
	operandValues.reverse();
	callProcedure(aState, aState.v, this.n, operandValues);
    } else {
	throw types.internalError(
	    "vstack empty", 
	    state.captureCurrentContinuationMarks(aState));
    }
//    debug("CALL " + this.n);
//    var operandValues = [], i;
//     for (i = 0; i < this.n; i++) {
// 	operandValues.push(state.popValue());
//     }
//    callProcedure(state, state.v, this.n, operandValues);
};





var callProcedure = function(aState, procValue, n, operandValues) {
    var args, result, argCount;

    procValue = selectProcedureByArity(n, procValue, operandValues);

    if (procValue instanceof PrimProc) {
	// callPrimitiveProcedure

	// Tail call optimization:
	if (aState.cstack.length !== 0 && 
	    aState.cstack[aState.cstack.length - 1] instanceof PopnControl) {
	    aState.cstack.pop().invoke(aState);
	}
	args = preparePrimitiveArguments(aState, 
					 procValue, 
					 operandValues,
					 n);
	result = procValue.impl.apply(procValue.impl, args);
	processPrimitiveResult(aState, result, procValue);

    } else if (procValue instanceof types.ClosureValue) {

	// Tail call optimization
	if (aState.cstack.length !== 0 && 
	    aState.cstack[aState.cstack.length - 1] instanceof PopnControl) {
	    aState.cstack.pop().invoke(aState);
	    argCount = prepareClosureArgumentsOnStack(aState, 
						      procValue, 
						      operandValues,
						      n);
	    aState.cstack.push(new PopnControl(argCount));
	    aState.cstack.push(procValue.body);

	} else if (aState.cstack.length >= 2 &&
		   aState.cstack[aState.cstack.length - 1] instanceof ContMarkRecordControl &&
		   aState.cstack[aState.cstack.length - 2] instanceof PopnControl) {
	    // Other tail call optimzation: if there's a continuation mark frame...
	    aState.cstack[aState.cstack.length - 2].invoke(aState);
	    argCount = prepareClosureArgumentsOnStack(aState, 
						      procValue, 
						      operandValues,
						      n);
	    aState.cstack[aState.cstack.length - 2] = new PopnControl(argCount);
	    aState.cstack.push(procValue.body);
	} else {
	    // General case:
	    argCount = prepareClosureArgumentsOnStack(aState, 
						      procValue, 
						      operandValues,
						      n);
	    aState.cstack.push(new PopnControl(argCount));
	    aState.cstack.push(procValue.body);
	}

    } else if (procValue instanceof types.ContinuationClosureValue) {

	if (n === 1) {
	    aState.v = operandValues[0];
	} else {
	    aState.v = new types.ValuesWrapper(operandValues);
	}
	aState.vstack = procValue.vstack.slice();
	aState.cstack = procValue.cstack.slice();
    } else {


	throw types.internalError("Something went wrong with checking procedures!",
				  state.captureCurrentContinuationMarks(aState));
    }
};




var processPrimitiveResult = function(state, result, procValue) {
    if (isInternalCall(result)) {
	state.cstack.push(new InternalCallRestartControl
			  (result.k, procValue));
	callProcedure(state,
		      result.operator, 
		      result.operands.length, 
		      result.operands);
    } else if (isInternalPause(result)) {
	throw new PauseException(result.onPause);
    } else {
	if (! procValue.usesState) {
	    state.v = result;
	}
    }
};



var PauseException = function(onPause) {
    this.onPause = onPause;
};




//////////////////////////////////////////////////////////////////////

var InternalCallRestartControl = function(k, procValue) {
    this.k = k;
    this.procValue = procValue;
};

InternalCallRestartControl.prototype.invoke = function(state) {
    processPrimitiveResult(state,
			   this.k(state.v), 
			   this.procValue);
};


//////////////////////////////////////////////////////////////////////








var acceptableParameterArityToString = function(acceptableParameterArity) {
    if (acceptableParameterArity.length === 1 &&
	acceptableParameterArity[0] === "0") {
	return "no arguments";
    }

    if (acceptableParameterArity.length === 1 &&
	acceptableParameterArity[0] === "1") {
	return "1 argument";
    }
    return "[" + acceptableParameterArity.join(', ') + "] arguments";
};





var getArgStr = function(operands) {
    var argStrBuffer;
    if (operands.length > 0) {
	argStrBuffer = [':'];
	for (var i = 0; i < operands.length; i++) {
	    argStrBuffer.push( types.toWrittenString(operands[i]) );
	}
	return argStrBuffer.join(' ');
    }
    return '';
};


// selectProcedureByArity: (CaseLambdaValue | CasePrimitive | Continuation | Closure | Primitive) -> (Continuation | Closure | Primitive)
var selectProcedureByArity = function(n, procValue, operands) {
    if ( ! isFunction(procValue) ) {
	    helpers.raise(
		types.incompleteExn(types.exnFailContract,
				    helpers.format("procedure application: expected procedure, given: ~s~a",
						   [procValue,
						    (operands.length == 0) ? ' (no arguments)' : '; arguments were' + getArgStr(operands)]),
				    []));
    }

    if (procValue instanceof types.CaseLambdaValue) {
	for (var j = 0; j < procValue.closures.length; j++) {
	    if (n === procValue.closures[j].numParams ||
		(n > procValue.closures[j].numParams && 
		 procValue.closures[j].isRest)) {
		return procValue.closures[j];
	    }
	}
	var acceptableParameterArity = [];
	for (var i = 0; i < procValue.closures.length; i++) {
	    acceptableParameterArity.push(procValue.closures[i].numParams + '');
	}
	helpers.raise(types.incompleteExn(
		types.exnFailContractArity,
		helpers.format("~a: expects ~a, given ~s~a",
			       [(procValue.name ? procValue.name : "#<case-lambda-procedure>"),
			        acceptableParameterArityToString(acceptableParameterArity),
				n,
				getArgStr(operands)]),
		[]));
    } else if (procValue instanceof types.CasePrimitive) {
	for (var j = 0; j < procValue.cases.length; j++) {
	    if (n === procValue.cases[j].numParams ||
		(n > procValue.cases[j].numParams && 
		 procValue.cases[j].isRest)) {
		return procValue.cases[j];
	    }
	}
	var acceptableParameterArity = [];
	for (var i = 0; i < procValue.cases.length; i++) {
	    acceptableParameterArity.push(procValue.cases[i].numParams + '');
	}
	helpers.raise(types.incompleteExn(
		types.exnFailContractArity,
		helpers.format("~a: expects ~a, given ~s~a",
			       [procValue.name, 
				acceptableParameterArityToString(acceptableParameterArity), n, getArgStr(operands)]),
		[]));
    }


    // At this point, procValue must be either a Continuation,
    // Closure, or Primitive.  We check to see that the number of
    // arguments n matches the acceptable number of arguments from the
    // procValue.
    if (procValue instanceof types.ContinuationClosureValue) {
	// The continuation can accept any number of arguments
	return procValue;
    } else {
	if ((n === procValue.numParams) ||
	    (n > procValue.numParams && procValue.isRest)) {
	    return procValue;
	} else {
	    helpers.raise(types.incompleteExn(
		types.exnFailContractArity,
		helpers.format("~a: expects ~a ~a, given ~s~a",
			       [(procValue.name !== types.EMPTY ? procValue.name : "#<procedure>"),
			        (procValue.isRest ? 'at least' : ''),
				numParamsToString(procValue.numParams),
				n,
				getArgStr(operands)]),
		[]));
	}
    }
};


// numParamsToString: number -> string
var numParamsToString = function(numParams) {
    if (numParams === 0) {
	return "no arguments";
    }
    if (numParams === 1) {
	return "1 argument";
    }
    else {
	return numParams + " arguments";
    }
};




var prepareClosureArgumentsOnStack = function(state, procValue, operandValues, n) {
    var argCount = 0;
    if (procValue.isRest) {
	var restArg = types.EMPTY;
	for (var i = 0; i < n - procValue.numParams ; i++) {
	    restArg = types.cons(operandValues.pop(), restArg);
	}
	state.vstack.push(restArg);
	argCount++;
    }	
    for (var i = operandValues.length -1; i >= 0; i--) {
	state.vstack.push(operandValues[i]);
	argCount++;
    }
    for(var i = procValue.closureVals.length-1; i >= 0; i--) {
	state.vstack.push(procValue.closureVals[i]);
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
//	throw new Error("arity error: expected at least "
//			+ primitiveValue.numParams + " arguments, but "
//			+ "received " + n + " arguments instead.");
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
//	    throw new Error("arity error: expected " 
//			    + primitiveValue.numParams 
//			    + " but received " + n);
	}
	for(var i = 0; i < primitiveValue.numParams; i++) {
	    args.push(operandValues.shift());
	}
    }
    return args;
};






//////////////////////////////////////////////////////////////////////
// Continuation marks
var WithContMarkControl = function(key, val, body) {
    this.key = key;
    this.val = val;
    this.body = body;
};

WithContMarkControl.prototype.invoke = function(aState) {
    aState.cstack.push(new WithContMarkKeyControl(this.val,
						  this.body));
    aState.cstack.push(this.key);
};


var WithContMarkKeyControl = function(val, body) {
    this.val = val;
    this.body = body;
};

WithContMarkKeyControl.prototype.invoke = function(aState) {
    aState.cstack.push(new WithContMarkVal(aState.v, this.body));
    aState.cstack.push(this.val);
};

var WithContMarkVal = function(key, body) {
    this.key = key;
    this.body = body;
};

WithContMarkVal.prototype.invoke = function(aState) {
    var evaluatedVal = aState.v;
    var cstack = aState.cstack;
    // Check to see if there's an existing ContMarkRecordControl
    if (cstack.length !== 0 && 
	(cstack[cstack.length - 1] instanceof ContMarkRecordControl)) {
	cstack.push(cstack.pop().update
		    (this.key, evaluatedVal));
    } else {
	cstack.push(new ContMarkRecordControl(
	    types.cons(types.cons(this.key, evaluatedVal),
		       types.EMPTY)));
    }
    cstack.push(this.body);
};





//////////////////////////////////////////////////////////////////////
// Apply-values


var ApplyValuesControl = function(proc, argsExpr) {
    this.proc = proc;
    this.argsExpr = argsExpr;
};

ApplyValuesControl.prototype.invoke = function(state) {
    state.cstack.push(new ApplyValuesArgControl(this.argsExpr));
    state.cstack.push(this.proc);
};

var ApplyValuesArgControl = function(expr) {
    this.expr = expr;
};

ApplyValuesArgControl.prototype.invoke = function(state) {
    state.cstack.push(new ApplyValuesAppControl(state.v));
    state.cstack.push(this.expr);
};


var ApplyValuesAppControl = function(procVal) {
    this.procVal = procVal;
};

ApplyValuesAppControl.prototype.invoke = function(state) {
    var exprValue = state.v;
    state.v = this.procVal;
    if (exprValue instanceof types.ValuesWrapper) {
	var elts = exprValue.elts;
	for(var i = elts.length - 1; i >= 0; i--) {
	    state.vstack.push(elts[i]);
	}
	state.cstack.push(new CallControl(elts.length));
    } else {
	state.vstack.push(exprValue);
	state.cstack.push(new CallControl(1));
    }
};




//////////////////////////////////////////////////////////////////////
// Let one
var LetOneControl = function(rhs, body) {
    this.rhs = rhs;
    this.body = body;
};


LetOneControl.prototype.invoke = function(state) {
    var cstack = state.cstack;
    state.pushn(1);
    cstack.push(new PopnControl(1));
    cstack.push(this.body);
    cstack.push(new SetControl(0));
    cstack.push(this.rhs);
};


//////////////////////////////////////////////////////////////////////
// Let void

var LetVoidControl = function(params) {
    this.count = params.count;
    this.isBoxes = params.isBoxes;
    this.body = params.body;
};

LetVoidControl.prototype.invoke = function(state) {
    var n = this.count;
    state.pushn(n);
    if (this.isBoxes) {
	for (var i = 0; i < n; i++) {
	    state.setn(i, types.box(types.UNDEFINED));
	}
    }
    state.cstack.push(new PopnControl(n));
    state.cstack.push(this.body);
};






//////////////////////////////////////////////////////////////////////

var BoxenvControl = function(pos, body) {
    this.pos = pos;
    this.body = body;
};


BoxenvControl.prototype.invoke = function(state) {
    state.setn(this.pos,
	       types.box(state.peekn(this.pos)));
    state.cstack.push(this.body);
};



//////////////////////////////////////////////////////////////////////
// install-value

var InstallValueControl = function(params) {
    this.count = params.count;
    this.pos = params.pos;
    this.isBoxes = params.isBoxes;
    this.rhs = params.rhs;
    this.body = params.body;
};


InstallValueControl.prototype.invoke = function(state) {
    state.cstack.push(new InstallValueRhsControl(this.count,
					 this.pos,
					 this.isBoxes,
					 this.body));
    state.cstack.push(this.rhs);
};


var InstallValueRhsControl = function(count, pos, isBoxes, body) {
    this.count = count;
    this.pos = pos;
    this.isBoxes = isBoxes;
    this.body = body;
};

InstallValueRhsControl.prototype.invoke = function(state) {
    // The value's on the stack.  First check the proper number
    // of arguments.
    var aValue = state.v;
    var vals = [];
    if (aValue instanceof types.ValuesWrapper) {
	if (this.count !== aValue.elts.length) {  
	    helpers.raise(
		types.exnFailContractArity("expected " + this.count 
					   + " values, but received " + aValue.elts.length,
					   state.captureCurrentContinuationMarks(aState)));
	}
	vals = aValue.elts;
    } else {
	if (this.count !== 1) {
	    helpers.raise(
		types.exnFailContractArity("expected " + this.count 
					   + " values, but received one",
					   state.captureCurrentContinuationMarks(aState)));
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
    state.cstack.push(this.body);
};









//////////////////////////////////////////////////////////////////////

var AssignControl = function(params) {
    this.id = params.id;
    this.rhs = params.rhs;
    this.isUndefOk = params.isUndefOk;
};


AssignControl.prototype.invoke = function(state) {
    state.cstack.push(new SetToplevelControl(this.id.depth,
					     this.id.pos,
					     this.isUndefOk));
    state.cstack.push(this.rhs);
};



var SetToplevelControl = function(depth, pos, isUndefOk) {
    this.depth = depth;
    this.pos = pos;
    this.isUndefOk = isUndefOk;
};

SetToplevelControl.prototype.invoke = function(aState) {
//    debug("SET_TOPLEVEL " + this.depth + ", " + this.pos);
    if (aState.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  state.captureCurrentContinuationMarks(aState));
    }
    aState.setPrefix(this.depth, this.pos, aState.v)
    aState.v = types.VOID;
};




//////////////////////////////////////////////////////////////////////
// Variable references

var VarrefControl = function(toplevel) {
    this.toplevel = toplevel;
};

VarrefControl.prototype.invoke = function(state) {
    var depth, pos;
    depth = this.toplevel.depth;
    pos = this.toplevel.pos;
    state.v = new types.VariableReference(state.vstack[state.vstack.length - 1 - depth],
					  pos);
};

//////////////////////////////////////////////////////////////////////




var ClosureControl = function(genId) {
    this.genId = genId + '';
};

ClosureControl.prototype.invoke = function(state) {
    state.v = state.heap[this.genId];
};




//////////////////////////////////////////////////////////////////////
// Case lambda

var CaseLamControl = function(name, clauses) {
    this.name = name;
    this.clauses = clauses;
};

CaseLamControl.prototype.invoke = function(state) {
    var clauses = this.clauses;
    if (clauses.length === 0) {
	state.v = new types.CaseLambdaValue(this.name, []);
    } else {
	state.cstack.push(new CaseLambdaComputeControl(this.name, 
						       types.list(clauses).rest,
						       types.list([])));
	state.cstack.push(clauses[0]);
    }
};


var CaseLambdaComputeControl = function(name, lamsToEvaluate, evaluatedLams) {
    this.name = name;
    this.lamsToEvaluate = lamsToEvaluate;
    this.evaluatedLams = evaluatedLams;
};


CaseLambdaComputeControl.prototype.invoke = function(state) {
    var nextEvaluatedLam = state.v;
    if (this.lamsToEvaluate === types.EMPTY) {
	var clauseList = (types.cons(nextEvaluatedLam, this.evaluatedLams)).reverse();
	var clauses = [];
	while (clauseList !== types.EMPTY) {
	    clauses.push(clauseList.first);
	    clauseList = clauseList.rest;
	}
	state.v = new types.CaseLambdaValue(this.name, clauses);
    } else {
	state.cstack.push(new CaseLambdaComputeControl(
	    this.name,
	    this.lamsToEvaluate.rest,
	    types.cons(nextEvaluatedLam,
		       this.evaluatedLams)));
	state.cstack.push(this.lamsToEvaluate.first);
    }
};




// PromptControl: integer ContinuationPromptTag Proc -> Control
// PromptControl cooperates with abort.
var PromptControl = function(vstackLength, promptTag, handler) {
    this.vstackLength = vstackLength; // how long is the value stack?
    this.promptTag = promptTag;
    // The handler is called in tail position with respect
    // to the prompt if we ever abort to it.
    this.handler = handler;
};


PromptControl.prototype.invoke = function(state) {
    // Does nothing.
};




// setupAbortToPrompt: state prompt-tag [arrayof value] -> void
// Does the bruntwork of the abort-to-prompt.  
var setupAbortToPrompt = function(aState, promptTag, args) {
    // First, find the appropriate prompt.
    var promptIndex = findPromptIndexInControlStack(aState, promptTag);
    if (promptIndex === -1) {
	// Error out: we can't abort properly.
	helpers.raise(types.incompleteExn(
	    types.exnFailContract,
	    helpers.format('abort-current-continuation: ' +
			   'continuation includes no prompt ', [])));

    }
    var promptValue = aState.cstack[promptIndex];

    // Chop the context down.
    aState.cstack.splice(promptIndex,
			 aState.cstack.length - promptIndex);

    aState.vstack.splice(promptValue.vstackLength,
			 aState.vstack.length - promptValue.vstackLength);


    // Set up the call the prompt's handler with the given arguments.
    // The handler will be called in tail position with respect to its prompt.    
    aState.cstack.push(
	new control.ApplicationControl(
	    new control.ConstantControl(promptValue.handler), 
	    helpers.map(function(op) {
			    return new control.ConstantControl(op)},
			args)));
};



// findPromptIndexInControlStack: state -> number
// Returns index into cstack of the prompt with the given
// prompt tag.  Returns -1 if we can't find it.
var findPromptIndexInControlStack = function(aState, promptTag) {
    var cstack = aState.cstack;
    for (var i =  cstack.length - 1; i >= 0; i--) {
	if (cstack[i] instanceof PromptControl &&
	    cstack[i].promptTag === promptTag) {
	    return i;
	}
    }
    return -1;
};






//////////////////////////////////////////////////////////////////////

// Continuation Marks

var ContMarkRecordControl = function(listOfPairs) {
    this.listOfPairs = listOfPairs || types.EMPTY;
};

ContMarkRecordControl.prototype.invoke = function(state) {
    // No-op: the record will simply pop off the control stack.
};

ContMarkRecordControl.prototype.update = function(key, val) {
    var l = this.listOfPairs;
    var acc;
    while (l !== types.EMPTY) {
	if (l.first.first === key) {
	    // slow path: walk the list and replace with the
	    // new key/value pair.
	    l = this.listOfPairs;
	    acc = types.EMPTY;
	    while (l !== types.EMPTY) {
		if (l.first.first === key) {
		    acc = types.cons(types.cons(key, val), 
				     acc);
		} else {
		    acc = types.cons(l.first, acc);
		}
		l = l.rest;
	    }
	    return new ContMarkRecordControl(acc);
	}
	l = l.rest;
    }
    // fast path: just return a new record with the element tacked at the
    // front of the original list.
    return new ContMarkRecordControl(types.cons(types.cons(key, val),
						this.listOfPairs));
};





//////////////////////////////////////////////////////////////////////
control.processPrefix = processPrefix;

control.ConstantControl = ConstantControl;
control.BranchControl = BranchControl;
control.SeqControl = SeqControl;
control.Beg0Control = Beg0Control;
control.ModControl = ModControl;
control.Prefix = Prefix;
control.ToplevelControl = ToplevelControl;
control.DefValuesControl = DefValuesControl;
control.LamControl = LamControl;
control.PrimvalControl = PrimvalControl;
control.ApplicationControl = ApplicationControl;
control.LocalrefControl = LocalrefControl;
control.ApplyValuesControl = ApplyValuesControl;
control.LetOneControl = LetOneControl;
control.LetVoidControl = LetVoidControl;
control.BoxenvControl = BoxenvControl;
control.InstallValueControl = InstallValueControl;
control.WithContMarkControl = WithContMarkControl;
control.AssignControl = AssignControl;
control.VarrefControl = VarrefControl;
control.ClosureControl = ClosureControl;
control.CaseLamControl = CaseLamControl;
control.LetRecControl = LetRecControl;
control.CallControl = CallControl;
control.RequireControl = RequireControl;
control.ContMarkRecordControl = ContMarkRecordControl;


control.PromptControl = PromptControl;
control.setupAbortToPrompt = setupAbortToPrompt;


control.PauseException = PauseException;


control.invokeModule = invokeModuleAndRestart;


})();

