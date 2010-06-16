/*
// For node.js.
var sys = require('sys');
var types = require('./types');
var primitive = require('./primitive');
var loader = require('./loader');
var assert = require('assert');
var control = require('./control');
var state = require('./state');

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


//////////////////////////////////////////////////////////////////////







//////////////////////////////////////////////////////////////////////

var interpret = {};


(function() {

// load: compilationTop -> state
// Load up the bytecode into a state, ready for evaluation.
var load = function(compilationTop) {
    var aState = new state.State();

    // Install the indirects table.
    processIndirects(aState, compilationTop['compiled-indirects']);

    // Process the prefix.
    var prefix = loader.loadPrefix(compilationTop.prefix);
    control.processPrefix(aState, prefix);


    // Add the code form to the control stack.
    aState.pushControl(loader.loadCode(aState, compilationTop.code));

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
	    new types.ClosureValue(numParams, 
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



// run: state (scheme-value -> void) -> void
var run = function(state, k) {
    try {
	while(! state.isStuck()) {
	    //debug("\n\nstate:\n");
	    // NOTE: sys.inspect is expensive, so we want to 
	    // control the order of evaluation within this tight
	    // loop.
	    //debugF(function() { return sys.inspect(state) });
	    step(state);
	}

	//     debug("\n\nFinished with: \n")
	//     debugF(function() { return sys.inspect(state) });
	//     debug("\n\nfinal value: ")
	//     debugF(function() { return sys.inspect(state.v) });
	k(state.v);
    } catch (e) {
	if (e instanceof control.PauseException) {
	    var onRestart = makeOnRestart(state, k);
	    e.onPause(onRestart);
	    return;
	} else {
	    throw e;
	}
    }
};
    
    

// call: state scheme-procedure (arrayof scheme-values) (scheme-value -> void) -> void
var call = function(state, operator, operands, k) {
    var stateValues = state.save();
    state.clear();
    state.pushControl(new control.ApplicationControl(operator, operands));
    try {
	run(state, 
	    function(v) {
		state.restore(stateValues);
		k(v)});
    } catch (e) {
	state.restore(stateValues);
	throw e;
    }
};





// create function for restarting a run, given the state
// and the continuation k.
var makeOnRestart = function(state, k) {
    return function(v) {
	state.v = v;
	run(state, k);
    };
};



// step: state -> void
// Takes one step in the abstract machine.
var step = function(state) {
    var nextCode = state.popControl();
    debugF(function() { return sys.inspect(nextCode) });
    if (typeof(nextCode) === 'object' && nextCode['invoke']) {
	nextCode.invoke(state);
    } else {
	// we should never get here.
	throw new Error("I don't know how to handle " + sys.inspect(nextCode));
    }
};



//////////////////////////////////////////////////////////////////////

/*
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

				state.v = f;
				state.pushControl(new control.CallControl(numArgs));
			    }));


primitive.addPrimitive('values',
		       new primitive.Primitive(
			   'values', 
			   0, 
			   true, false,
			   function(args) {
			       if (args.length === 1) {
				   return args[0];
			       }
			       return new types.ValuesWrapper(args);
			   }));
*/




primitive.addPrimitive('call/cc', 
		       new primitive.Primitive('call/cc',
					       1,
					       false, true,
					       function(state, f) {
						   var continuationClosure = 
						       captureContinuationClosure(state);
						   state.pushValue(continuationClosure);
						   state.v = f;
						   state.pushControl(new control.CallControl(1));
					       }));


var captureContinuationClosure = function(state) {
    return new types.ContinuationClosureValue(state.vstack,
					      state.cstack);
};



//////////////////////////////////////////////////////////////////////


interpret.load = load;
interpret.step = step;
interpret.run = run;
interpret.call = call;
//interpret.setDebug = setDebug;

})();

