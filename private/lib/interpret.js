
//////////////////////////////////////////////////////////////////////

var interpret = {};


(function() {

// load: compilationTop state? -> state
// Load up the bytecode into a state, ready for evaluation.  If
// an old state is given, then reuses it.
var load = function(compilationTop, aState) {
    if (! aState) {
	aState = new state.State();
    }

    try {
	// Install the indirects table.
	processIndirects(aState, compilationTop['compiled-indirects']);

	// Process the prefix.
	var prefix = loader.loadPrefix(compilationTop.prefix);
	var prefixValue = control.processPrefix(aState, prefix);

	// Add the default prompt.  Its handler consumes any number
	// of arguments and just returns them.
	aState.cstack.push(new control.PromptControl(
	    aState.vstack.length,
	    types.defaultContinuationPromptTag, 
	    types.defaultContinuationPromptTagHandler));
	// Add the code form to the control stack.
	aState.cstack.push(loader.loadCode(aState, compilationTop.code));
    } catch(e) {
	if (types.isSchemeError(e)) {
	    // scheme exception
	    if ( types.isExn(e.val) &&
		 !types.isContinuationMarkSet( types.exnContMarks(e.val) ) ) {
		types.exnSetContMarks(e.val, 
				      state.captureCurrentContinuationMarks(aState));
	    }
	}
	throw e;
    }

    return aState;

    // TODO: do some processing of the bytecode so that all the
    // constants are native, enable quick dispatching based on
    // bytecode type, rewrite the indirect loops, etc...
};




var processIndirects = function(state, indirects) {
    // First, install the shells
    for (var i = 0 ;i < indirects.length; i++) {
	var anIndirect = indirects[i];
	var lam = anIndirect['lam'];

	var numParams = lam['num-params'];
	var paramTypes = lam['param-types'];
	var isRest = lam['rest?'];
	var closureVals = makeClosureValsFromMap(state,
						 lam['closure-map'], 
						 lam['closure-types']);

	// Subtle: ignore the lam['body'] here: first install the lambdas in the heap.
	var sentinelBody = new control.ConstantControl(false)

	state.heap[anIndirect.id] = 
	    new types.ClosureValue(anIndirect.id,
				   numParams, 
				   paramTypes, 
				   isRest, 
				   closureVals, 
				   sentinelBody);
    }

    // Once the lambdas are there, we can load up the bodies.
    for (var i = 0 ;i < indirects.length; i++) {
	var anIndirect = indirects[i];
	var lam = anIndirect['lam'];

	var lamValue = state.heap[anIndirect.id];
	lamValue.body = loader.loadCode(state, lam['body'])
    }
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




// run: state [string] ->
// consumes a state (that contains success and fail callbacks)
// and an optional string representing where this call came from
// executes whatever computation is currently on the call stack in the given state
var run = function(aState, callSite) {
    // Save the onSuccess and onFail because we're going to use these
    // even if something changes later (such as if an error gets thrown)
    var onSuccess = aState.onSuccess,
    onFail = aState.onFail,
    MAX_STEPS_BEFORE_BOUNCE = aState.MAX_STEPS_BEFORE_BOUNCE,
    gas,
    breakExn,
    stateValues,
    onCall,
    aCompleteError;
    try {
	gas = MAX_STEPS_BEFORE_BOUNCE;
	while( (! (aState.cstack.length === 0)) && (gas > 0)) {
	    step(aState);
	    gas--;
	}

	if (aState.breakRequested) {
	    breakExn = types.exnBreak("user break", 
				      state.captureCurrentContinuationMarks(aState),
				      state.captureContinuationClosure(aState));
	    helpers.raise(breakExn);
	} else if (gas <= 0) {
	    aState.pausedForGas = true;
	    setTimeout(function() { aState.pausedForGas = false;
			    	    run(aState, callSite); },
		       0);
	    return;
	} else {
	    onSuccess(aState.v);
	}
    } catch (e) {
	if (e instanceof control.PauseException) {
	    stateValues = aState.save();
	    aState.clearForEval({preserveBreak: true});

	    aState.onSuccess = function(v, callSite) {
		aState.restore(stateValues);
		aState.v = v;
		run(aState, callSite);
	    };
	    aState.onFail = function(e2) {
		aState.restore(stateValues);
		onFail( completeError(aState, e2) );
	    };
	    onCall = makeOnCall(aState);
	    e.onPause(onCall, aState.onSuccess, aState.onFail);
	}
	else {
	    // Exception handling.
	    // Check to see if we have a continuation mark with the exception
	    // handler.  If so, run it!

	    aCompleteError = completeError(aState, e);
	    if (isExceptionHandlerInContext(aState) && types.isSchemeError(aCompleteError)) {
		applyExceptionHandler(aState, aCompleteError.val, callSite);
	    } else {
		onFail( aCompleteError );
	    }
	}
    }
};

// isExceptionHandlerInContext: state -> boolean
// Produces true if we're in a context that does have an exception handler.
var isExceptionHandlerInContext = function(aState) {
    var contMarks = state.captureCurrentContinuationMarks(aState);
    var exceptionHandlers = contMarks.ref(types.exceptionHandlerKey);
    // FIXME: check that the exception handler is a function.
    return exceptionHandlers.length > 0;
};


var applyExceptionHandler = function(aState, aSchemeError, callSite) {
    var contMarks = state.captureCurrentContinuationMarks(aState);
    var exceptionHandlers = contMarks.ref(types.exceptionHandlerKey);
    aState.cstack.push(
	new control.ApplicationControl(
	    new control.ConstantControl(exceptionHandlers[0]), 
	    helpers.map(function(op) {
			    return new control.ConstantControl(op)},
			[aSchemeError])));
    run(aState, callSite);
};





// completeError: state error -> error
// Take an error and make it complete: have it include the current continuation marks
// if it's missing it.
var completeError = function(aState, error) {
    if ( types.isSchemeError(error) && types.isIncompleteExn(error.val) ) {
	var contMarks = state.captureCurrentContinuationMarks(aState);
	var errorArgs = [error.val.msg, contMarks].concat(error.val.otherArgs);
	return types.schemeError( error.val.constructor.apply(null, errorArgs) );
    }
    return error;
};
    

// call: state scheme-procedure (arrayof scheme-values) (scheme-value -> void) -> void
var call = function(aState, operator, operands, k, onFail, callSite) {
    if ( aState.pausedForGas ) {
	    setTimeout(function() { call(aState, operator, operands, k, onFail, callSite); }, 1);
	    return;
    }
    
    var stateValues = aState.save();
    aState.clearForEval({preserveBreak: true});


    aState.cstack.push(
	new control.ApplicationControl(
	    new control.ConstantControl(operator), 
	    helpers.map(function(op) {
			    return new control.ConstantControl(op)},
			operands)));

    aState.onSuccess = function(v) {
	aState.restore(stateValues);
	k(v);
    };
    aState.onFail = function(e) {
	aState.restore(stateValues);
	onFail(e);
    };
    run(aState, callSite);
};


var makeOnCall = function(state) {
    return function(operator, operands, k, onFail, callSite) {
	call(state, operator, operands, k, onFail, callSite);
    };
};



// step: state -> void
// Takes one step in the abstract machine.
var step = function(aState) {
    var nextCode;
    if (aState.cstack.length === 0) {
	throw types.internalError("cstack empty", 
				  captureCurrentContinuationMarks(aState));
    }
    nextCode = aState.cstack.pop();
    if (nextCode.invoke) {
	nextCode.invoke(aState);
    } else {
	// we should never get here.
	throw types.internalError("I don't know how to handle " + sys.inspect(nextCode),
				  state.captureCurrentContinuationMarks(aState));
    }
};



//////////////////////////////////////////////////////////////////////


interpret.load = load;
interpret.step = step;
interpret.run = run;
interpret.call = call;
//interpret.setDebug = setDebug;

})();

