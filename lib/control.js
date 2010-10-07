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


//////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////
// INTERNAL
// Set
// Setting stack values.

var SetControl = function(depth) {
    this.depth = depth;
};
SetControl.prototype.invoke = function(state) {
    debug("SET " + this.depth);
    if (state.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  state.captureCurrentContinuationMarks(aState));
    }
    state.setn(this.depth, state.v);
};


//////////////////////////////////////////////////////////////////////
// INTERNAL
// Push a value into the nth position on the stack

var PushnControl = function(n) {
    this.n = n;
};
PushnControl.prototype.invoke = function(state) {
    state.pushn(this.n);
};


// INTERNAL
var SwapControl = function(depth) {
    this.depth = depth;
};

SwapControl.prototype.invoke = function(state) {
    debug("SWAP " + this.depth);
    if (state.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  state.captureCurrentContinuationMarks(aState));
    }
    var tmp = state.vstack[state.vstack.length - 1 - (this.depth || 0)];
    state.vstack[state.vstack.length - 1 - (this.depth || 0)] = state.v;
    state.v = tmp;
};



// Internal
// Pop n values
var PopnControl = function(n) { 
    this.n = n;
};

PopnControl.prototype.invoke = function(state) {
    state.popn(this.n);
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
    var cmds = [];
    
    // Invoke all the requires before hitting the body.
    for (var i = 0; i < this.requires.length; i++) {
	cmds.push(new RequireControl(this.requires[i]));
    }

    for(var i = 0; i < this.body.length; i++) {
	cmds.push(this.body[i]);
    }
    cmds.push(new ModControlAfterControl(prefixValue));
    state.pushManyControls(cmds);
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
    // modules variables will refer to bindings in moby-lang anymore

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


BranchControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.test);
    cmds.push(new BranchRestControl(this.thenPart, this.elsePart));
    state.pushManyControls(cmds);
};

var BranchRestControl = function(thenPart, elsePart) {
    this.thenPart = thenPart;
    this.elsePart = elsePart;
};


BranchRestControl.prototype.invoke = function(state) {
    debug("BRANCH");
    if (state.v !== false && state.v !== undefined) {
	state.pushControl(this.thenPart);
    } else {
	state.pushControl(this.elsePart);
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


var isHardcodedModule = function(resolvedModuleName) {
    var hardcodedModules = [
	types.list([types.symbol("quote"), types.symbol("#%kernel")]),
	types.list([types.symbol("quote"), types.symbol("#%paramz")]),
	types.list([types.symbol("quote"), types.symbol("#%utils")]),
	types.symbol("moby/kernel"),
	types.symbol("moby/paramz"),
	types.symbol("moby/js-impl")];
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
    aState.onSuccess = newOnSuccess;
    aState.onFail = onFail;
    interpret.run(aState);
};



var invokeJavascriptModuleAndRestart = function(aState, resolvedModuleName, 
						moduleRecord,
						onSuccess, onFail) {    
    var invokeAllRequiredModules = function(modules, after) {
	if (modules.isEmpty()) {
	    after();
	} else {
	    invokeModuleAndRestart(aState,
				   modules.first()+'',
				   function() {
				       invokeAllRequiredModules(modules.rest(), after);
				   },
				   onFail);
	}
    }


    var afterRequiresInvoked = function() {
	var EXPORTS = {};
	try {
	    (moduleRecord.jsImplementation)(EXPORTS);
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
    this.forms = forms;
};


SeqControl.prototype.invoke = function(state) {
    var forms = this.forms;
    var cmds = [];
    for (var i = 0; i < forms.length; i++) {
	cmds.push(forms[i]);
    }
    state.pushManyControls(cmds);    
};



//////////////////////////////////////////////////////////////////////
// Beg0

var Beg0Control = function(seq) {
    this.seq = seq;
};

Beg0Control.prototype.invoke = function(state) {
    if (this.seq.length === 1) {
	state.pushControl(this.seq[0]);
    } else {
	var rest = [];
	for (var i = 1; i < this.seq.length; i++) {
	    rest.push(this.seq[i]);
	}
	state.pushManyControls([this.seq[0], new Beg0RestControl(rest)]);
    }
};


var Beg0RestControl = function(rest) {
    this.rest = rest;
};

Beg0RestControl.prototype.invoke = function(state) {
    // Rearrange the control stack so the rest of the
    // begin sequence will evaluate, followed by 
    // bringing the first expression's value back into
    // the value register.
    state.pushControl(new ConstantControl(state.v));
    state.pushManyControls(this.rest);
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

LetRecControl.prototype.invoke = function(state) {
    var cmds = [];
    var n = this.procs.length;
    for (var i = 0; i < n; i++) {
	cmds.push(this.procs[i]);
	cmds.push(new SetControl(n - 1 - i));
    }
    cmds.push(new LetrecReinstallClosureControls(this.procs));
    cmds.push(this.body);
    state.pushManyControls(cmds);
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
    var cmds = [];
    cmds.push(this.body);
    cmds.push(new DefValuesInstallControl(this.ids))
    state.pushManyControls(cmds);
};


var DefValuesInstallControl = function(ids) {
    this.ids = ids;
};

DefValuesInstallControl.prototype.invoke = function(aState) {
    debug("DEF_VALUES");
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

    var cmds = [];    
    // We allocate as many values as there are operands.
    if (rands.length !== 0) {
	cmds.push(new PushnControl(rands.length));
    }
    cmds.push(rator);    
    if (rands.length !== 0) {
	cmds.push(new SetControl(rands.length-1));
    }

    for (var i = 0; i < rands.length; i++) {
	if (i !== rands.length - 1) {
	    cmds.push(rands[i]);
	    cmds.push(new SetControl(i));
	} else {
	    cmds.push(rands[rands.length-1]);
	    cmds.push(new SwapControl(rands.length-1));
	}
    }
    cmds.push(new CallControl(rands.length));
    // CallControl will be responsible for popping off the 
    // value stack elements.

    state.pushManyControls(cmds);
};




var CallControl = function(n) {
    this.n = n;
};

CallControl.prototype.invoke = function(state) {
    debug("CALL " + this.n);
    var operandValues = [];
    for (var i = 0; i < this.n; i++) {
	operandValues.push(state.popValue());
    }
    callProcedure(state, state.v, this.n, operandValues);
};


var callProcedure = function(aState, procValue, n, operandValues) {
    procValue = selectProcedureByArity(n, procValue, operandValues);
    if (primitive.isPrimitive(procValue)) {
	callPrimitiveProcedure(aState, procValue, n, operandValues);
    } else if (procValue instanceof types.ClosureValue) {
	callClosureProcedure(aState, procValue, n, operandValues);
    } else if (procValue instanceof types.ContinuationClosureValue) {
	callContinuationProcedure(aState, procValue, n, operandValues);
    } else {
	throw types.internalError("Something went wrong with checking procedures!",
				  state.captureCurrentContinuationMarks(aState));
    }
};


var callPrimitiveProcedure = function(state, procValue, n, operandValues) {
    // Tail call optimization:
    if (state.cstack.length !== 0 && 
	state.cstack[state.cstack.length - 1] instanceof PopnControl) {
	state.cstack.pop().invoke(state);
    }
    var args = preparePrimitiveArguments(state, 
					 procValue, 
					 operandValues,
					 n);
    var result = procValue.impl.apply(procValue.impl, args);
    processPrimitiveResult(state, result, procValue);
};


var processPrimitiveResult = function(state, result, procValue) {
    if ( types.isInternalCall(result) ) {
	state.cstack.push(new InternalCallRestartControl
			  (result.k, procValue));
	callProcedure(state,
		      result.operator, 
		      result.operands.length, 
		      result.operands);
    } else if ( types.isInternalPause(result) ) {
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








var callClosureProcedure = function(state, procValue, n, operandValues) {
    // Tail call optimization
    if (state.cstack.length !== 0 && 
	state.cstack[state.cstack.length - 1] instanceof PopnControl) {
	state.cstack.pop().invoke(state);
	var argCount = prepareClosureArgumentsOnStack(state, 
						      procValue, 
						      operandValues,
						      n);
	state.pushControl(new PopnControl(argCount));
	state.pushControl(procValue.body);

    } else if (state.cstack.length >= 2 &&
	       types.isContMarkRecordControl(state.cstack[state.cstack.length - 1]) &&
	       state.cstack[state.cstack.length - 2] instanceof PopnControl) {
	// Other tail call optimzation: if there's a continuation mark frame...
	state.cstack[state.cstack.length - 2].invoke(state);
	var argCount = prepareClosureArgumentsOnStack(state, 
						      procValue, 
						      operandValues,
						      n);
	state.cstack[state.cstack.length - 2] = new PopnControl(argCount);
	state.pushControl(procValue.body);
    } else {
	// General case:
	var argCount = prepareClosureArgumentsOnStack(state, 
						      procValue, 
						      operandValues,
						      n);
	state.pushControl(new PopnControl(argCount));
	state.pushControl(procValue.body);
    }
};


var callContinuationProcedure = function(state, procValue, n, operandValues) {
    if (n === 1) {
	state.v = operandValues[0];
    } else {
	state.v = new types.ValuesWrapper(operandValues);
    }
    state.vstack = procValue.vstack;
    state.cstack = procValue.cstack;
};




// selectProcedureByArity: (CaseLambdaValue | CasePrimitive | Continuation | Closure | Primitive) -> (Continuation | Closure | Primitive)
var selectProcedureByArity = function(n, procValue, operands) {
    var getArgStr = function() {
	var argStr = '';
	if (operands.length > 0) {
		var argStrBuffer = [':'];
		for (var i = 0; i < operands.length; i++) {
			argStrBuffer.push( types.toWrittenString(operands[i]) );
		}
		argStr = argStrBuffer.join(' ');
	}
	return argStr;
    }

    if ( !types.isFunction(procValue) ) {
	    var argStr = getArgStr('; arguments were:');
	    helpers.raise(
		types.incompleteExn(types.exnFailContract,
				    helpers.format("procedure application: expected procedure, given: ~s~a",
						   [procValue,
						    (operands.length == 0) ? ' (no arguments)' : '; arguments were' + getArgStr()]),
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
		helpers.format("~a: expects [~a] arguments, given ~s~a",
			       [(procValue.name ? procValue.name : "#<case-lambda-procedure>"),
			        acceptableParameterArity.join(', '),
				n,
				getArgStr()]),
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
		helpers.format("~a: expects [~a] arguments, given ~s~a",
			       [procValue.name, acceptableParameterArity.join(', '), n, getArgStr()]),
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
		helpers.format("~a: expects ~a ~a argument~a, given ~s~a",
			       [(procValue.name !== types.EMPTY ? procValue.name : "#<procedure>"),
			        (procValue.isRest ? 'at least' : ''),
				procValue.numParams,
				(procValue.numParams == 1) ? '' : 's',
				n,
				getArgStr()]),
		[]));
	}
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

WithContMarkControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.key);
    cmds.push(new WithContMarkKeyControl(this.val,
					 this.body));
    state.pushManyControls(cmds);
};


var WithContMarkKeyControl = function(val, body) {
    this.val = val;
    this.body = body;
};

WithContMarkKeyControl.prototype.invoke = function(state) {
    var evaluatedKey = state.v;
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
    var evaluatedVal = state.v;
    // Check to see if there's an existing ContMarkRecordControl
    if (state.cstack.length !== 0 && 
	( types.isContMarkRecordControl(state.cstack[state.cstack.length - 1]) )) {
	state.pushControl(state.cstack.pop().update
			  (this.key, evaluatedVal));
    } else {
	var aHash = types.makeLowLevelEqHash();
	aHash.put(this.key, evaluatedVal);
	state.pushControl(types.contMarkRecordControl(aHash));
    }
    state.pushControl(this.body);
};





//////////////////////////////////////////////////////////////////////
// Apply-values


var ApplyValuesControl = function(proc, argsExpr) {
    this.proc = proc;
    this.argsExpr = argsExpr;
};

ApplyValuesControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.proc);
    cmds.push(new ApplyValuesArgControl(this.argsExpr));
    state.pushManyControls(cmds);
};

var ApplyValuesArgControl = function(expr) {
    this.expr = expr;
};

ApplyValuesArgControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.expr);
    cmds.push(new ApplyValuesAppControl(state.v));
    state.pushManyControls(cmds);

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
	    state.pushValue(elts[i]);
	}
	state.pushControl(new CallControl(elts.length));
    } else {
	state.pushValue(exprValue);
	state.pushControl(new CallControl(1));
    }
};




//////////////////////////////////////////////////////////////////////
// Let one
var LetOneControl = function(rhs, body) {
    this.rhs = rhs;
    this.body = body;
};


LetOneControl.prototype.invoke = function(state) {
    var cmds = [];
    state.pushn(1);
    cmds.push(this.rhs);
    cmds.push(new SetControl(0));
    cmds.push(this.body);
    cmds.push(new PopnControl(1));
    state.pushManyControls(cmds);
};


//////////////////////////////////////////////////////////////////////
// Let void

var LetVoidControl = function(params) {
    this.count = params.count;
    this.isBoxes = params.isBoxes;
    this.body = params.body;
};

LetVoidControl.prototype.invoke = function(state) {
    var cmds = [];
    var n = this.count;
    state.pushn(n);
    if (this.isBoxes) {
	for (var i = 0; i < n; i++) {
	    state.setn(i, types.box(types.UNDEFINED));
	}
    }
    cmds.push(this.body);
    cmds.push(new PopnControl(n));
    state.pushManyControls(cmds);
};






//////////////////////////////////////////////////////////////////////

var BoxenvControl = function(pos, body) {
    this.pos = pos;
    this.body = body;
};


BoxenvControl.prototype.invoke = function(state) {
    state.setn(this.pos,
	       types.box(state.peekn(this.pos)));
    state.pushControl(this.body);
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
    var cmds = [];
    cmds.push(this.rhs);
    cmds.push(new InstallValueRhsControl(this.count,
					 this.pos,
					 this.isBoxes,
					 this.body));
    state.pushManyControls(cmds);
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
    state.pushControl(this.body);
};









//////////////////////////////////////////////////////////////////////

var AssignControl = function(params) {
    this.id = params.id;
    this.rhs = params.rhs;
    this.isUndefOk = params.isUndefOk;
};


AssignControl.prototype.invoke = function(state) {
    var cmds = [];
    cmds.push(this.rhs);
    cmds.push(new SetToplevelControl(this.id.depth,
				     this.id.pos,
				     this.isUndefOk));
    state.pushManyControls(cmds);
};



var SetToplevelControl = function(depth, pos, isUndefOk) {
    this.depth = depth;
    this.pos = pos;
    this.isUndefOk = isUndefOk;
};

SetToplevelControl.prototype.invoke = function(aState) {
    debug("SET_TOPLEVEL " + this.depth + ", " + this.pos);
    if (aState.vstack.length - 1 - (this.depth || 0) < 0) {
	throw types.internalError("vstack not long enough",
				  state.captureCurrentContinuationMarks(aState));
    }
    aState.setPrefix(this.depth, this.pos, aState.v)
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
	state.pushControl(new CaseLambdaComputeControl(this.name, 
						       types.list(clauses).rest(),
						       types.list([])));
	state.pushControl(clauses[0]);
    }
};


var CaseLambdaComputeControl = function(name, lamsToEvaluate, evaluatedLams) {
    this.name = name;
    this.lamsToEvaluate = lamsToEvaluate;
    this.evaluatedLams = evaluatedLams;
};


CaseLambdaComputeControl.prototype.invoke = function(state) {
    var nextEvaluatedLam = state.v;
    if (this.lamsToEvaluate.isEmpty()) {
	var clauseList = (types.cons(nextEvaluatedLam, this.evaluatedLams)).reverse();
	var clauses = [];
	while (!clauseList.isEmpty()) {
	    clauses.push(clauseList.first());
	    clauseList = clauseList.rest();
	}
	state.v = new types.CaseLambdaValue(this.name, clauses);
    } else {
	state.pushControl(new CaseLambdaComputeControl(
	    this.name,
	    this.lamsToEvaluate.rest(),
	    types.cons(nextEvaluatedLam,
		       this.evaluatedLams)));
	state.pushControl(this.lamsToEvaluate.first());
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
    aState.pushControl(
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


control.PromptControl = PromptControl;
control.setupAbortToPrompt = setupAbortToPrompt;


control.PauseException = PauseException;



})();

