var assert = require('assert');
var runtime = require('./../../lib/test-support');
//var control = require('./../../lib/control');
//var types = require('./../../lib/types');
var sys = require('sys');

var control = runtime.control;
var types = runtime.types;


//////////////////////////////////////////////////////////////////////


var run = runtime.run;
var step = runtime.step;


//////////////////////////////////////////////////////////////////////

var EXIT_ON_FIRST_ERROR = true;


//////////////////////////////////////////////////////////////////////



var makeStateWithConstant = function(c) {
    var s = new runtime.State();
    s.v = c;
    return s;
};


var makePrefix = function(n) {
    var arr = [];    
    for (var i = 0; i < n; i++) {
	arr.push(false);
    }
    return new control.Prefix({numLifts: 0,
			       toplevels: arr });
};

var makeMod = function(prefix, body) {
    return new control.ModControl(prefix, body);
};

var makeConstant = function(c) {
    return new control.ConstantControl(c);
};

var makeBranch = function(x, y, z) { 
    return new control.BranchControl(x, y, z);
};

var makeSeq = function() {
    return new control.SeqControl(arguments);
};

var makeBeg0 = function() {
    return new control.Beg0Control(arguments);
};

var makeToplevel = function(depth, pos) {
    return new control.ToplevelControl(depth, pos);
};


var makeDefValues = function(ids, body) {
    return new control.DefValuesControl(ids, body);
};


var makeLam = function(arity, closureMap, body) {
    var aClosureMap = [];
    var aClosureTypes = [];
    var aParamTypes = [];
    for (var i = 0; i < closureMap.length; i++) {
	aClosureMap.push(closureMap[i]);
	aClosureTypes.push("val/ref");
    }
    for (var i = 0; i < arity; i++) {
	aParamTypes.push("val");
    }

    return new control.LamControl({'numParams': arity,
				   'paramTypes': aParamTypes,
				   'isRest': false,
				   'closureMap' : aClosureMap,
				   'closureTypes' : aClosureTypes,
				   'body': body});    
};


var makeLamWithRest = function(arity, closureMap, body) {
    var aClosureMap = [];
    var aClosureTypes = [];
    var aParamTypes = [];
    for (var i = 0; i < closureMap.length; i++) {
	aClosureMap.push(closureMap[i]);
	aClosureTypes.push("val/ref");
    }
    for (var i = 0; i < arity; i++) {
	aParamTypes.push("val");
    }

    return new control.LamControl({'numParams': arity,
				   'paramTypes': aParamTypes,
				   'isRest': true,
				   'closureMap' : aClosureMap,
				   'closureTypes' : aClosureTypes,
				   'body': body});    
};






var makePrimval = function(name) {
    return new control.PrimvalControl(name);
};


var makeApplication = function(rator, rands) {
    assert.ok(typeof(rands) === 'object' && rands.length !== undefined);
    return new control.ApplicationControl(rator, rands);
};


var makeLocalRef = function(n) {
    return new control.LocalrefControl(n);
};


var makeApplyValues = function(proc, argsExpr) {
    return new control.ApplyValuesControl(proc, argsExpr);
};


var makeLet1 = function(rhs, body) {
    return new control.LetOneControl(rhs, body);
};


var makeLetVoid = function(count, isBoxes, body) {
    return new control.LetVoidControl({count: count,
				       isBoxes : isBoxes,
				       body : body});
};

var makeBoxenv = function(pos, body) {
    return new control.BoxenvControl(pos, body);
};


var makeInstallValue = function(count, pos, isBoxes, rhs, body) {
    return new control.InstallValueControl({count: count,
					    pos: pos,
					    isBoxes: isBoxes,
					    rhs: rhs,
					    body: body});

};


var makeWithContMark = function(key, val, body) {
    return new control.WithContMarkControl(key, val, body);
};


var makeAssign = function(id, rhs, isUndefOk) {
    return new control.AssignControl({id: id,
				      rhs: rhs,
				      isUndefOk: isUndefOk});
};

  
var makeVarref = function(aToplevel) {
    return new control.VarrefControl(aToplevel);
};


var makeClosure = function(genId) {
    return new control.ClosureControl(genId);
};


var makeCaseLam = function(name, clauses) {
    assert.ok(typeof(clauses) === 'object' && clauses.length !== undefined);
    return new control.CaseLamControl(name, clauses);
};


var makeLetrec = function(procs, body) {
    return new control.LetRecControl(procs, body);
};


/////////////////////////////////////////////////////////////////////


var testPrim = function(funName, f, baseArgs, expectedValue) {
	var state = new runtime.State();
	var args = [];
	for (var i = 0; i < baseArgs.length; i++) {
		args.push(makeConstant(f(baseArgs[i])));
	}
	state.pushControl(makeApplication(makePrimval(funName), args));
	assert.deepEqual(run(state), expectedValue);
};

var id = function(x) {return x;};

/*
var testNumPrim = function(funName, baseArgs, expectedValue) {
	var state = new runtime.State();
	var args = [];
	for (var i = 0; i < baseArgs.length; i++) {
		args.push(makeConstant(runtime.rational(baseArgs[i])));
	}
	state.pushControl(makeApplication(makePrimval(funName), args));
	assert.deepEqual(run(state), expectedValue);
}

var testStringPrim = function(funName, baseArgs, expectedValue) {
	var state = new runtime.State();
	var args = [];
	for (var i = 0; i < baseArgs.length; i++) {
		args.push(makeConstant(runtime.string(baseArgs[i])));
	}
	state.pushControl(makeApplication(makePrimval(funName), args));
	assert.deepEqual(run(state), expectedValue);
}
*/



//////////////////////////////////////////////////////////////////////

var runTest = function(name, thunk) {
    sys.print("running " + name + "... ");
    try {
	thunk();
    } catch(e) {
	sys.print(" FAIL\n");
	sys.print(e);
	if (EXIT_ON_FIRST_ERROR) {
	    throw e;
	}
    }
    sys.print(" ok\n")
    
};

//////////////////////////////////////////////////////////////////////


sys.print("START TESTS\n\n");

runTest("simple empty state",
	// Simple running should just terminate, and always be at the "stuck" state.
	function() { 
	    var state = new runtime.State();
	    assert.ok(state.isStuck());
	    run(state);
	    assert.ok(state.isStuck());
	});



// Numeric constants should just evaluate through.
runTest("Numeric constant", 
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant(42));
	    var result = run(state);
	    assert.deepEqual(result, 
			     42);
	    
	    assert.deepEqual(state, makeStateWithConstant(42));
	});



// String constant.
runTest("String constant",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant("hello world"));
	    var result = run(state);
	    assert.deepEqual(result, 
			     "hello world");

	    assert.deepEqual(state, makeStateWithConstant("hello world"));
	});


// boolean constant.
runTest("Boolean constant", 
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant(true));
	    var result = run(state);
	    assert.deepEqual(result, true);

	    assert.deepEqual(state, makeStateWithConstant(true));
	});



// Simple branch to true
runTest("Simple boolean branch to true",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBranch(makeConstant(true),
					 makeConstant(true),
					 makeConstant(false)));
	    var result = run(state);
	    assert.deepEqual(result, true);
	});


// Simple branch to false
runTest("Simple boolean branch to false",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBranch(makeConstant(false),
					 makeConstant(false),
					 makeConstant(true)));
	    var result = run(state);
	    assert.deepEqual(result, 
			     true);

	    assert.deepEqual(state, makeStateWithConstant(true));
	});



// (if (if true false true) "apple" "pie") --> "pie"
runTest("nested booleans",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBranch(makeBranch(makeConstant(true), makeConstant(false), makeConstant(true)),
					 makeConstant("apple"),
					 makeConstant("pie")));
	    var result = run(state);
	    assert.deepEqual(result, "pie");

	    assert.deepEqual(state, makeStateWithConstant("pie"));
	});



// Sequences
runTest("Sequences",
	function() {
	    var state1 = new runtime.State();
	    state1.pushControl(makeSeq(makeConstant(3),
				       makeConstant(4),
				       makeConstant(5)));
	    step(state1);
	    step(state1);
	    assert.ok(!state1.isStuck());
	    assert.deepEqual(state1.v, 3);
	    step(state1);
	    assert.deepEqual(state1.v, 4);
	    var result = run(state1);
	    assert.deepEqual(result, 5);

	    assert.deepEqual(state1, makeStateWithConstant(5));    
	});



// Module prefix
runTest("module prefix",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    run(state);   
	    assert.equal(1, state.vstack.length);
	    assert.ok(state.vstack[0] instanceof types.PrefixValue);
	    assert.equal(state.vstack[0].length(), 3);
	});


runTest("toplevel lookup",
	// toplevel lookup
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    run(state);   

	    state.vstack[0].set(0, "zero");
	    state.vstack[0].set(1, "one");
	    state.vstack[0].set(2, "two");

	    state.pushControl(makeToplevel(0, 0));
	    assert.equal(run(state), "zero");

	    state.pushControl(makeToplevel(0, 1));
	    assert.equal(run(state), "one");

	    state.pushControl(makeToplevel(0, 2));
	    assert.equal(run(state), "two");
	});



runTest("define-values",
	// define-values
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    run(state);   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("try it")));
	    run(state);

	    var expectedState = new runtime.State();
	    expectedState.pushControl(makeMod(makePrefix(3),
					      []));
	    run(expectedState);   
	    expectedState.v = "try it";
	    expectedState.vstack[0].set(0, "try it");
	    assert.deepEqual(state, expectedState);
	});


runTest("lambda",
	// lambda
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    run(state);   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("Some toplevel value")));

	    run(state);
	    state.pushControl(makeLam(3, [0], makeConstant("I'm a body")));

	    var result = run(state);

	    // result should be a lambda.
	    assert.ok(result instanceof runtime.ClosureValue);
	    assert.equal(result.closureVals.length, 1);
	    assert.ok(result.closureVals[0] instanceof types.PrefixValue);
	    assert.deepEqual(result.body, makeConstant("I'm a body"));
	    assert.equal(result.numParams, 3);
	});



runTest("primval (current-print)",
	// primval
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("current-print"));
	    var result = run(state);
	    assert.ok(result instanceof runtime.Primitive);
	});


runTest("primval on bad primitive should throw error",
	// primval on unknowns should throw error
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("foobar"));
	    assert.throws(function() { run(state); });
	});


runTest("Primval on *",
	// primval on *
	// primval
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("*"));
	    var result = run(state);
	    assert.ok(result instanceof runtime.Primitive);
	});


runTest("My own list function",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makeLamWithRest(0, [], makeLocalRef(0)),
					      [makeConstant("one"),
					       makeConstant("two"),
					       makeConstant("three")]))
	    var result = run(state);
	    assert.deepEqual(result,
			     runtime.list(["one", "two", "three"]));
	});


runTest("primitive application",
	// primitive application.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makePrimval("*"),
					      [makeConstant(runtime.rational(3)),
					       makeConstant(runtime.rational(5))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(15));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive application, no arguments",
	// primitive application with no arguments.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makePrimval("*"),
					      []));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(1));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive application, nested application",
	// primitive application, with nesting
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("*"),
		[makeApplication(
		    makePrimval("*"),
		    [makeConstant(runtime.rational(3)),
		     makeConstant(runtime.rational(5))]),
		 makeConstant(runtime.rational(7))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(105));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive appliation, nesting, testing non-commutativity",
	// primitive application, with nesting, testing order
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("string-append"),
		[makeApplication(
		    makePrimval("string-append"),
		    [makeConstant("hello"),
		     makeConstant("world")]),
		 makeConstant("testing")]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.string("helloworldtesting"));
	    assert.equal(state.vstack.length, 0);
	});

runTest("primitive application, subtraction",
	// subtraction
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("-"),
		[makeApplication(
		    makePrimval("-"),
		    [makeConstant(runtime.rational(3)),
		     makeConstant(runtime.rational(4))]),
		 makeConstant(runtime.rational(15))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(-16));
	    assert.equal(state.vstack.length, 0);
	});

runTest("primitive application, unary subtraction (negation)", 
	// Checking negation.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("-"),
		[makeConstant(runtime.rational(1024))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(-1024));
	    assert.equal(state.vstack.length, 0);
	});


runTest("closure application",
	// Closure application
	// lambda will just return a constant value
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeConstant("I'm a body"))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0), [makeConstant("boo")]));
	    var result = run(state);
	    assert.equal(result, "I'm a body");

	    assert.equal(state.vstack.length, 1);
	});


runTest("closure application, defining square",
	// Closure application
	// lambda will square its argument
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeApplication(makePrimval("*"),
								    [makeLocalRef(2),
								     makeLocalRef(2)]))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0), 
					      [makeConstant(runtime.rational(4))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(16));
	    assert.equal(state.vstack.length, 1);
	});



runTest("closure application, testing tail calls",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (f) (f)) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    makeApplication(makeToplevel(0, 0),
								    []))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    var MAXIMUM_BOUND = 5;
	    var ITERATIONS = 1000000;
	    for (var i = 0; i < ITERATIONS; i++) {
		step(state);
		assert.ok(state.cstack.length < MAXIMUM_BOUND);
	    }
	});



runTest("closure application, testing tail calls with even/odd",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (even? x) (if (zero? x) true (odd? (sub1 x))))
	// (define (odd? x) (if (zero? x) false (even? (sub1 x))))
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    state.pushControl(makeDefValues
			      ([makeToplevel(0, 0)],
			       makeLam(1, [0],
				       makeBranch(
					   makeApplication(makePrimval("zero?"),
							   [makeLocalRef(2)]),
					   makeConstant(true),
					   makeApplication(makeToplevel(1, 1),
							   [makeApplication(
							       makePrimval("sub1"),
							       [makeLocalRef(3)])])))));
	    state.pushControl(makeDefValues
			      ([makeToplevel(0, 1)],
			       makeLam(1, [0],
				       makeBranch(
					   makeApplication(makePrimval("zero?"),
							   [makeLocalRef(2)]),
					   makeConstant(false),
					   makeApplication(makeToplevel(1, 0),
							   [makeApplication(
							       makePrimval("sub1"),
							       [makeLocalRef(3)])])))));
	    
	    run(state);

	    var even = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(runtime.rational(n))]));
		var MAXIMUM_BOUND = 10;
		while (!state.isStuck()) {
		    step(state);
		    assert.ok(state.cstack.length < MAXIMUM_BOUND);
		    //sys.print(state.cstack.length + "\n");
		}
		return state.v;
	    }
	    assert.equal(even(0), true);
	    assert.equal(even(1), false);
	    assert.equal(even(50), true);
	    assert.equal(even(51), false);
	    assert.equal(even(501), false);
	    assert.equal(even(1001), false);
	    assert.equal(even(10000), true);
	    assert.equal(even(10001), false);
	});


runTest("factorial",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0)],
		makeLam(1, [0],
			makeBranch(
			    makeApplication(makePrimval("zero?"),
					    [makeLocalRef(2)]),
			    makeConstant(runtime.rational(1)),
			    makeApplication(makePrimval("*"),
					    [makeLocalRef(3),
					     makeApplication(
						 makeToplevel(3, 0),
						 [makeApplication(makePrimval("sub1"),
								  [makeLocalRef(5)])])])))));

	    run(state);

	    var fact = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(runtime.rational(n))]));
		return run(state);
	    }

 	    assert.equal(fact(0), 1);
 	    assert.equal(fact(1), 1);
 	    assert.equal(fact(2), 2);
 	    assert.equal(fact(3), 6);
 	    assert.equal(fact(4), 24);
	    assert.equal(fact(5), 120);
	    assert.equal(fact(6), 720);
	    assert.equal(fact(10), 3628800);
	    assert.equal(fact(11), 39916800);
	    assert.equal(fact(12), 479001600);
	});



runTest("apply on a primitive *",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("*"),
		 makeConstant(
		     runtime.list([runtime.rational(3),
				   runtime.rational(9)]))]));
	    assert.deepEqual(run(state),
			     27);
	    assert.equal(state.vstack.length, 0);
	});



runTest("apply on a primitive -",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("-"),
		 makeConstant(
		     runtime.list([runtime.rational(3),
				   runtime.rational(9)]))]));
	    assert.deepEqual(run(state),
			     -6);
	    assert.equal(state.vstack.length, 0);
	});

runTest("apply on a primitive -, three arguments",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("apply"),
		[makePrimval("-"),
		 makeConstant(
		     runtime.list([runtime.rational(3),
				   runtime.rational(9),
				   runtime.rational(12)]))]));
	    assert.deepEqual(run(state),
			     -18);
	    assert.equal(state.vstack.length, 0);
	});


runTest("values",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("values"),
		[makePrimval("*"),
		 makeConstant(
		     runtime.list([runtime.rational(3),
				   runtime.rational(9),
				   runtime.rational(12)]))]));
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.ok(result instanceof runtime.ValuesWrapper);
	    assert.equal(result.elts.length, 2);
	});



runTest("values with no arguments",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(
		makePrimval("values"),[]));
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.ok(result instanceof runtime.ValuesWrapper);
	    assert.equal(result.elts.length, 0);
	});




runTest("current-inexact-milliseconds",
	function() {
	    var state = new runtime.State();
	    for (var i = 0; i < 2000; i++) {
		state.pushControl(makeApplication(
		    makePrimval("current-inexact-milliseconds"),[]));
		var result1 = run(state);


		state.pushControl(makeApplication(
		    makePrimval("current-inexact-milliseconds"),[]));
		var result2 = run(state);
		assert.ok(runtime.lessThanOrEqual(result1, result2));
	    }
	});




runTest("values with def-values",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant("hello"),
				 makeConstant("world")])));
	    run(state);
	    assert.equal(state.vstack.length, 1);
	    assert.ok(state.vstack[0] instanceof types.PrefixValue);
	    assert.equal(state.vstack[0].ref(0), "hello");
	    assert.equal(state.vstack[0].ref(1), "world");
	});



runTest("apply-values",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant("hello"),
				 makeConstant("world")])));
	    run(state);

	    state.pushControl(makeApplyValues(
		makeLam(2, [], makeApplication(makePrimval("string-append"),
					       [makeLocalRef(2),
						makeLocalRef(3)])),
		makeApplication(makePrimval("values"),
				[makeToplevel(2, 0),
				 makeToplevel(2, 1)])));
	    assert.equal(run(state), "helloworld");
	});



runTest("apply-values, testing no stack usage",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    run(state);   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makePrimval("zero?"),
				 makeConstant(runtime.rational(0))])));
	    run(state);

	    state.pushControl(makeApplyValues(
		makeToplevel(0, 0),
		makeToplevel(0, 1)));
	    assert.equal(run(state), true);
	    assert.equal(state.vstack.length, 1);
	});

runTest("let-one, trivial",
	function() {
	    var state = new runtime.State();
	    assert.equal(state.vstack.length, 0);
	    var body = makeLocalRef(0);
	    state.pushControl(makeLet1(makeConstant("someValue"),
				       body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.deepEqual(result, "someValue");
	});


runTest("let-one, different body",
	function() {
	    var state = new runtime.State();
	    assert.equal(state.vstack.length, 0);
	    var body = makeConstant("something else");
	    state.pushControl(makeLet1(makeConstant("someValue"),
				       body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = run(state);
	    assert.equal(state.vstack.length, 0);
	    assert.deepEqual(result, "something else");
	});


runTest("let-void, no boxes",
	function() {
	    var state = new runtime.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, false, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok(state.vstack[i] === runtime.UNDEFINED);
	    }
	    var result = run(state);
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("let-void, with boxes",
	function() {
	    var state = new runtime.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, true, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok(state.vstack[i] instanceof runtime.Box);
	    }
	    var result = run(state);
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("beg0 with just one argument should immediately reduce to its argument",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBeg0(makeConstant("first post")));
	    step(state);
	    assert.equal(state.cstack.length, 1);
	    assert.deepEqual(state.cstack[0], 
			     makeConstant("first post"));
	    var result = run(state);
	    assert.equal(result, "first post");
	});



runTest("beg0, more general",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBeg0(makeConstant("first post"),
				       makeConstant("second post"),
				       makeConstant("third post"),
				       makeConstant("fourth post")));
	    step(state);

	    // By this point, there should be two elements
	    // in the control stack, the evaluation of the first
	    // argument, and a control to continue the
	    // rest of the sequence evaluation.
	    assert.equal(state.cstack.length, 2); 
	    var result = run(state);
	    assert.equal(result, "first post");
	});



runTest("boxenv",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeLet1(makeConstant("foo"),
				       makeBoxenv(0, 
						  makeLocalRef(0))));
	    var result = run(state);
	    assert.ok(result instanceof runtime.Box);
	    assert.deepEqual(result, new runtime.Box("foo"));
	});


runTest("install-value, without boxes",
	function() {
	    var state = new runtime.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeLetVoid
		 (4,
		  false,
		  makeInstallValue
		  (3, 1, false,
		   makeApplication(makePrimval("values"),
				   [makeConstant("3"),
				    makeConstant("1"),
				    makeConstant("4")]),
		   aBody)));
	    while (state.cstack[state.cstack.length - 1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 4);
	    assert.equal(state.vstack[0], "4");
	    assert.equal(state.vstack[1], "1");
	    assert.equal(state.vstack[2], "3");
	    var result = run(state);
	    assert.equal(result, "peep");
	    assert.equal(state.vstack.length, 0);
	});



runTest("install-value, with boxes",
	function() {
	    var state = new runtime.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeLetVoid
		 (4,
		  true,
		  makeInstallValue
		  (3, 1, true,
		   makeApplication(makePrimval("values"),
				   [makeConstant("3"),
				    makeConstant("1"),
				    makeConstant("4")]),
		   aBody)));
	    while (state.cstack[state.cstack.length - 1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.vstack.length, 4);
	    assert.deepEqual(state.vstack[0], new runtime.Box("4"));
	    assert.deepEqual(state.vstack[1], new runtime.Box("1"));
	    assert.deepEqual(state.vstack[2], new runtime.Box("3"));
	    var result = run(state);
	    assert.equal(result, "peep");
	    assert.equal(state.vstack.length, 0);
	});


runTest("assign",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), 
				    [makeAssign(makeToplevel(0, 0),
						makeConstant("some value"),
						true)]));
	    run(state);
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0].ref(0), "some value");
	});


runTest("varref",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1),
				      [makeSeq(makeAssign(makeToplevel(0, 0),
						makeConstant("a toplevel value"),
							  true),
					       makeVarref(makeToplevel(0, 0)))]));
	    var result = run(state);
	    assert.ok(result instanceof runtime.VariableReference);
	    assert.equal(result.ref(), "a toplevel value");
	    result.set("something else!");
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0].ref(0), "something else!");
	});


runTest("closure",
	function() {
	    var state = new runtime.State();
	    state.heap['some-closure'] = 42;
	    state.pushControl(makeClosure('some-closure'));
	    // The way we process closures in bytecode-compiler
	    // should make this a direct heap lookup.
	    assert.equal(run(state), 42);
	});


runTest("with-cont-mark", 
	function() {
	    var state = new runtime.State();
	    var aBody = makeConstant("peep");
	    state.pushControl
		(makeWithContMark(makeConstant
				  (runtime.symbol("x")),
				  makeConstant("42"),
				  aBody));
	    while (state.cstack[state.cstack.length -1] !== aBody) {
		step(state);
	    }
	    assert.equal(state.cstack.length, 2);
	    assert.ok(state.cstack[0] instanceof 
		      control.ContMarkRecordControl);
	    assert.equal(state.cstack[0].dict['x'],
			 "42");
	    var result = run(state);
	    assert.equal(result, "peep");
	});




runTest("closure application, testing tail calls in the presence of continuation marks",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (f) (call-with-continuation-marks 'x 1 (f))) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    run(state);   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    (makeWithContMark
						     (makeConstant(runtime.symbol("x")),
						      makeConstant(runtime.rational(1)),
						      
						      makeApplication(makeToplevel(0, 0),
								      []))))));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    var MAXIMUM_BOUND = 6;
	    var ITERATIONS = 1000000;
	    for (var i = 0; i < ITERATIONS; i++) {
		step(state);
		assert.ok(state.cstack.length < MAXIMUM_BOUND);
	    }
	});


runTest("case-lambda, with a function that consumes one or two values",
	function() {
	    var state = new runtime.State();
	    state.pushControl
		(makeMod(makePrefix(1), 
			 [makeDefValues
			  ([makeToplevel(0, 0)],
			   makeCaseLam(runtime.symbol("last"),
				       [makeLam(1, [], makeLocalRef(0)),
					makeLam(2, [], makeLocalRef(1))]))]));
	    run(state);
	    state.pushControl(makeApplication(makeToplevel(1, 0),
					      [makeConstant(runtime.rational(5))]));
	    var result = run(state);
	    assert.deepEqual(result, runtime.rational(5));

	    state.pushControl(makeApplication(makeToplevel(2, 0),
					      [makeConstant(runtime.rational(7)),
					       makeConstant(runtime.rational(42))]));
	    result = run(state);
	    assert.deepEqual(result, runtime.rational(42));
	});



// runTest("factorial again, testing the accumulation of continuation marks",
// 	//
// 	// (define marks #f)
// 	// (define (f x)
// 	//   (with-continuation-marks 'x x
// 	//     (if (= x 0)
// 	//         (begin (set! marks (current-continuation-marks))
// 	//                1)
// 	//         (* x (f (sub1 x))))))
// 	function() {

// 	});


runTest("let-rec",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeLetVoid(2,
					  false,
					  makeLetrec([makeLam(1, [0],
							      makeBranch
							      (makeApplication(makePrimval("zero?"),
									       [makeLocalRef(2)]),
							       makeConstant(true),
							       makeApplication(makeLocalRef(1),
									       [makeApplication
										(makePrimval("sub1"),
										 [makeLocalRef(3)])]))),
						      makeLam(1, [1],
							      makeBranch
							      (makeApplication(makePrimval("zero?"),
									       [makeLocalRef(2)]),
							       makeConstant(false),
							       makeApplication(makeLocalRef(1),
									       [makeApplication
										(makePrimval("sub1"),
										 [makeLocalRef(3)])])))],
						     makeLocalRef(1))));
	    var evenValue = run(state);
	    var e = function(x) {
		state.pushControl(makeApplication(makeConstant(evenValue),
						  [makeConstant(runtime.rational(x))]));
		return run(state);
	    }
	    assert.equal(state.vstack.length, 0);

	    assert.equal(e(0), true);
	    assert.equal(e(1), false);
	    assert.equal(e(2), true);
	    assert.equal(e(3), false);
	    assert.equal(e(100), true);
	    assert.equal(e(101), false);
	    assert.equal(e(10000), true);
	    assert.equal(e(10001), false);
	});


/***************************************
 *** Primitive String Function Tests ***
 ***************************************/

runTest('symbol?',
	function() {
		testPrim('symbol?', id, [runtime.symbol('hi')], true);
		testPrim('symbol?', runtime.rational, [1], false);
	});

runTest('string->symbol',
	function() {
		testPrim('string->symbol', id, [runtime.string('hello!')], runtime.symbol('hello!'));
		testPrim('string->symbol', id, [runtime.string(' world')], runtime.symbol(' world'));
	});


runTest('symbol->string',
	function() {
		testPrim('symbol->string', id, [runtime.symbol('hello!')], runtime.string('hello!'));
	});


runTest('string?',
	function() {
		testPrim('string?', id, [runtime.symbol('hello!')], false);
		testPrim('string?', id, ['string'], false);
		testPrim('string?', runtime.string, ['world'], true);
	});


runTest('make-string',
	function() {
		testPrim('make-string', id, [0, runtime.char('A')], runtime.string(""));
		testPrim('make-string', id, [runtime.rational(3), runtime.char('b')], runtime.string('bbb'));
	});


runTest('string',
	function() {
		testPrim('string', id, [], runtime.string(''));
		testPrim('string', runtime.char, ['a', 'b'], runtime.string('ab'));
	});

runTest('string-length',
	function() {
		testPrim('string-length', runtime.string, [''], 0);
		testPrim('string-length', runtime.string, ['antidisestablishmentarianism'], 28);
	});

runTest('string-ref',
	function() {
		testPrim('string-ref', id, [runtime.string('abcd'), 1], runtime.char('b'));
		testPrim('string-ref', id, [runtime.string('asdfasdf'), 4], runtime.char('a'));
	});

runTest('string=?',
	function() {
		testPrim('string=?', runtime.string, ['asdf', 'asdf', 'Asdf'], false);
		testPrim('string=?', runtime.string, ['far', 'fAr'], false);
		testPrim('string=?', runtime.string, ['', ''], true);
		testPrim('string=?', runtime.string, ['as', 'as', 'as'], true);
		testPrim('string=?', runtime.string, ['1', '1', '2'], false);
	});

runTest('string-ci=?',
	function() {
		testPrim('string-ci=?', runtime.string, ['asdf', 'asdf', 'Asdf'], true);
		testPrim('string-ci=?', runtime.string, ['far', 'fAr'], true);
		testPrim('string-ci=?', runtime.string, ['', ''], true);
		testPrim('string-ci=?', runtime.string, ['as', 'as', 'as'], true);
		testPrim('string-ci=?', runtime.string, ['1', '1', '2'], false);
	});

runTest('string<?',
	function() {
		testPrim('string<?', runtime.string, ["", "a"], true);
		testPrim('string<?', runtime.string, ['abc', 'ab'], false);
		testPrim('string<?', runtime.string, ['abc', 'abc'], false);
		testPrim('string<?', runtime.string, ['abc', 'def', 'cde'], false);
		testPrim('string<?', runtime.string, ['a', 'b', 'c', 'd', 'dd', 'e'], true);
	});

runTest('string>?',
	function() {
		testPrim('string>?', runtime.string, ["", "a"], false);
		testPrim('string>?', runtime.string, ['abc', 'ab'], true);
		testPrim('string>?', runtime.string, ['abc', 'abc'], false);
		testPrim('string>?', runtime.string, ['def', 'abc', 'cde'], false);
		testPrim('string>?', runtime.string, ['e', 'd', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string<=?',
	function() {
		testPrim('string<=?', runtime.string, ["", "a"], true);
		testPrim('string<=?', runtime.string, ['abc', 'ab'], false);
		testPrim('string<=?', runtime.string, ['abc', 'abc'], true);
		testPrim('string<=?', runtime.string, ['abc', 'aBc'], false);
		testPrim('string<=?', runtime.string, ['abc', 'def', 'cde'], false);
		testPrim('string<=?', runtime.string, ['a', 'b', 'b', 'd', 'dd', 'e'], true);
	});

runTest('string>=?',
	function() {
		testPrim('string>=?', runtime.string, ["", "a"], false);
		testPrim('string>=?', runtime.string, ['abc', 'ab'], true);
		testPrim('string>=?', runtime.string, ['abc', 'abc'], true);
		testPrim('string>=?', runtime.string, ['aBc', 'abc'], false);
		testPrim('string>=?', runtime.string, ['def', 'abc', 'cde'], false);
		testPrim('string>=?', runtime.string, ['e', 'e', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string-ci<?',
	function() {
		testPrim('string-ci<?', runtime.string, ["", "a"], true);
		testPrim('string-ci<?', runtime.string, ['Abc', 'ab'], false);
		testPrim('string-ci<?', runtime.string, ['abc', 'abc'], false);
		testPrim('string-ci<?', runtime.string, ['abc', 'def', 'cde'], false);
		testPrim('string-ci<?', runtime.string, ['a', 'b', 'C', 'd', 'dd', 'e'], true);
	});

runTest('string-ci>?',
	function() {
		testPrim('string-ci>?', runtime.string, ["", "a"], false);
		testPrim('string-ci>?', runtime.string, ['abc', 'Ab'], true);
		testPrim('string-ci>?', runtime.string, ['abc', 'abc'], false);
		testPrim('string-ci>?', runtime.string, ['def', 'abc', 'cde'], false);
		testPrim('string-ci>?', runtime.string, ['e', 'D', 'cc', 'c', 'b', 'a'], true);
	});

runTest('string-ci<=?',
	function() {
		testPrim('string-ci<=?', runtime.string, ["", "a"], true);
		testPrim('string-ci<=?', runtime.string, ['Abc', 'ab'], false);
		testPrim('string-ci<=?', runtime.string, ['abc', 'abc'], true);
		testPrim('string-ci<=?', runtime.string, ['abc', 'aBc'], true);
		testPrim('string-ci<=?', runtime.string, ['abc', 'def', 'cde'], false);
		testPrim('string-ci<=?', runtime.string, ['a', 'b', 'b', 'D', 'dd', 'e'], true);
	});

runTest('string-ci>=?',
	function() {
		testPrim('string-ci>=?', runtime.string, ["", "a"], false);
		testPrim('string-ci>=?', runtime.string, ['abc', 'ab'], true);
		testPrim('string-ci>=?', runtime.string, ['abc', 'abc'], true);
		testPrim('string-ci>=?', runtime.string, ['aBc', 'abc'], true);
		testPrim('string-ci>=?', runtime.string, ['def', 'abc', 'cde'], false);
		testPrim('string-ci>=?', runtime.string, ['e', 'e', 'cc', 'C', 'b', 'a'], true);
	});


runTest('substring',
	function() {
		testPrim('substring', id, [runtime.string('abc'), 1], runtime.string('bc'));
		testPrim('substring', id, [runtime.string('abc'), 0], runtime.string('abc'));
		testPrim('substring', id, [runtime.string('abcdefgh'), 2, 4], runtime.string('cd'));
		testPrim('substring', id, [runtime.string('abc'), 3], runtime.string(''));
		testPrim('substring', id, [runtime.string('abcd'), 2, 2], runtime.string(''));
	});


runTest('string-append',
	function() {
		testPrim('string-append', runtime.string, [], runtime.string(''));
		testPrim('string-append', runtime.string, ['a', '', 'b', ' world'], runtime.string('ab world'));
	});


runTest('string->list',
	function() {
		testPrim('string->list', runtime.string, [''], runtime.EMPTY);
		testPrim('string->list', runtime.string, ['one'], runtime.list([runtime.char('o'),
										runtime.char('n'),
										runtime.char('e')]));
	});

runTest('list->string',
	function() {
		testPrim('list->string', id, [runtime.EMPTY], runtime.string(''));
		testPrim('list->string', id,
			 [runtime.list([runtime.char('H'),
					runtime.char('e'),
					runtime.char('l'),
					runtime.char('l'),
					runtime.char('o')])],
			 runtime.string('Hello'));
	});


runTest('string-copy',
	function() {
		testPrim('string-copy', runtime.string, [''], runtime.string(''));
		testPrim('string-copy', runtime.string, ['hello'], runtime.string('hello'));

		var state = new runtime.State();
		var str = runtime.string('hello');
		state.pushControl(makeApplication(makePrimval('string-copy'), [makeConstant(str)]));
		var result = run(state);
		assert.deepEqual(result, str);
		assert.deepEqual((result === str), false);
	});


runTest('format',
	function() {
		testPrim('format', runtime.string, ['hello'], runtime.string('hello'));
		testPrim('format', runtime.string, ['hello~n'], runtime.string('hello\n'));
		testPrim('format', id, [runtime.string('Test: ~a~nTest2: ~A~%'),
					runtime.char('A'),
					runtime.list([1, 2, 3])],
			 runtime.string('Test: A\nTest2: (1 2 3)\n'));
		testPrim('format', id, [runtime.string('~s ~S ~a'),
					runtime.char('b'),
					runtime.complex(0, 2),
					runtime.char('b')],
			 runtime.string('#\\b 0+2i b'));
	});



/*************************************
 *** Primitive Math Function Tests ***
 *************************************/


runTest("zero?",
	function() {
		testPrim('zero?', runtime.rational, [0], true);
		testPrim('zero?', runtime.rational, [1], false);
		testPrim('zero?', id, [runtime.complex(0, 1)], false);
	});



runTest("sub1",
	function() {
		testPrim('sub1', runtime.rational, [25], runtime.rational(24));
		testPrim('sub1', id, [runtime.complex(3, 5)], runtime.complex(2, 5));
	});


runTest("add1",
	function() {
		testPrim('add1', runtime.rational, [25], runtime.rational(26));
		testPrim('add1', id, [runtime.complex(3, 5)], runtime.complex(4, 5));
	});


runTest("+",
	function() {
		testPrim('+', runtime.rational, [], runtime.rational(0));
		testPrim('+', runtime.rational, [2], runtime.rational(2));
		testPrim('+', runtime.rational, [1, 2], runtime.rational(3));
		testPrim('+', runtime.rational, [1, 2, 3, 4], runtime.rational(10));
	});


runTest("-",
	function() {
		testPrim('-', runtime.rational, [2], runtime.rational(-2));
		testPrim('-', runtime.rational, [1, 2], runtime.rational(-1));
		testPrim('-', runtime.rational, [1, 2, 3, 4], runtime.rational(-8));
	});


runTest("*",
	function() {
		testPrim('*', runtime.rational, [], runtime.rational(1));
		testPrim('*', runtime.rational, [2], runtime.rational(2));
		testPrim('*', runtime.rational, [1, 2], runtime.rational(2));
		testPrim('*', runtime.rational, [1, 2, 3, 4], runtime.rational(24));
	});


runTest("/",
	function() {
		testPrim('/', runtime.rational, [2], runtime.rational(1, 2));
		testPrim('/', runtime.rational, [1, 3], runtime.rational(1, 3));
		testPrim('/', runtime.rational, [18, 2, 3, 4], runtime.rational(3, 4));
	});


runTest('abs',
	function() {
		testPrim('abs', runtime.rational, [2], runtime.rational(2));
		testPrim('abs', runtime.rational, [0], runtime.rational(0));
		testPrim('abs', runtime.rational, [-2], runtime.rational(2));
	});

/*
runTest('quotient',
	function() {
	    testPrim('quotient', runtime.rational, [5, 3], runtime.rational(1));
	});


runTest('remainder',
	function() {
	    testPrim('remainder', runtime.rational, [5, 3], runtime.rational(2));
	});
*/

runTest('modulo',
	function() {
	    testPrim('modulo', runtime.rational, [-5, 3], runtime.rational(1));
	});


runTest('=',
	function() {
	    testPrim('=', runtime.rational, [2, 3], false);
	    testPrim('=', runtime.rational, [2, 2, 2, 2], true);
	    testPrim('=', runtime.rational, [2, 2, 3, 3], false);
	});


runTest('<',
	function() {
	    testPrim('<', runtime.rational, [1, 2], true);
	    testPrim('<', runtime.rational, [2, 2], false);
	    testPrim('<', runtime.rational, [3, 2], false);
	    testPrim('<', runtime.rational, [1, 2, 3, 4], true);
	    testPrim('<', runtime.rational, [1, 2, 2, 3], false);
	    testPrim('<', runtime.rational, [1, 3, 5, 4], false);
	});


runTest('>',
	function() {
	    testPrim('>', runtime.rational, [1, 2], false);
	    testPrim('>', runtime.rational, [2, 2], false);
	    testPrim('>', runtime.rational, [3, 2], true);
	    testPrim('>', runtime.rational, [4, 3, 2, 1], true);
	    testPrim('>', runtime.rational, [4, 3, 3, 2], false);
	    testPrim('>', runtime.rational, [4, 3, 5, 2], false);
	});


runTest('<=',
	function() {
	    testPrim('<=', runtime.rational, [1, 2], true);
	    testPrim('<=', runtime.rational, [2, 2], true);
	    testPrim('<=', runtime.rational, [3, 2], false);
	    testPrim('<=', runtime.rational, [1, 2, 3, 4], true);
	    testPrim('<=', runtime.rational, [2, 3, 3, 3], true);
	    testPrim('<=', runtime.rational, [1, 3, 5, 4], false);
	});


runTest('>=',
	function() {
	    testPrim('>=', runtime.rational, [1, 2], false);
	    testPrim('>=', runtime.rational, [2, 2], true);
	    testPrim('>=', runtime.rational, [3, 2], true);
	    testPrim('>=', runtime.rational, [4, 3, 2, 1], true);
	    testPrim('>=', runtime.rational, [4, 3, 3, 2], true);
	    testPrim('>=', runtime.rational, [5, 3, 5, 4], false);
	});


runTest('positive?',
	function() {
		testPrim('positive?', runtime.rational, [-1], false);
		testPrim('positive?', runtime.rational, [0], false);
		testPrim('positive?', runtime.rational, [1], true);
	});


runTest('negative?',
	function() {
		testPrim('negative?', runtime.rational, [-1], true);
		testPrim('negative?', runtime.rational, [0], false);
		testPrim('negative?', runtime.rational, [1], false);
	});


runTest('max',
	function() {
		testPrim('max', runtime.rational, [1], runtime.rational(1));
		testPrim('max', runtime.rational, [1, 2], runtime.rational(2));
		testPrim('max', runtime.rational, [2, 1, 4, 3, 6, 2], runtime.rational(6));
	});


runTest('min',
	function() {
		testPrim('min', runtime.rational, [1], runtime.rational(1));
		testPrim('min', runtime.rational, [1, 2], runtime.rational(1));
		testPrim('min', runtime.rational, [2, 1, 4, 3, 6, 2], runtime.rational(1));
	});



/*************************************
 *** Primitive List Function Tests ***
 *************************************/


runTest('cons, car, and cdr',
	function() {
		var state = new runtime.State();
		state.pushControl(makeApplication(makePrimval('car'),
						  [makeApplication(makePrimval('cons'),
							 	   [makeConstant(runtime.rational(1)),
								    makeConstant(runtime.EMPTY)])]));
		assert.deepEqual(run(state), runtime.rational(1));

		state.pushControl(makeApplication(makePrimval('cdr'),
						  [makeApplication(makePrimval('cons'),
							  	   [makeConstant(runtime.rational(1)),
								    makeConstant(runtime.EMPTY)])]));
		assert.deepEqual(run(state), runtime.EMPTY);

		state.pushControl(makeApplication(makePrimval('cdr'),
						  [makeApplication(makePrimval('cons'),
							[makeConstant(runtime.rational(1)),
							 makeApplication(makePrimval('cons'),
								[makeConstant(runtime.rational(2)),
								 makeConstant(runtime.EMPTY)])])]));
		assert.deepEqual(run(state), runtime.pair(2, runtime.EMPTY));
	});


runTest('list?',
	function() {
		testPrim('list?', id, [runtime.EMPTY], true);
		testPrim('list?', id, [runtime.pair(1, runtime.EMPTY)], true);
		testPrim('list?', id, [runtime.list([1, 2, 0, 3, 2])], true);
		testPrim('list?', id, [runtime.complex(0, 2)], false);
	});


runTest('list',
	function() {
		testPrim('list', runtime.rational, [], runtime.EMPTY);
		testPrim('list', runtime.rational, [1], runtime.pair(runtime.rational(1), runtime.EMPTY));
		testPrim('list', runtime.rational, [1, 5, 3], runtime.list([runtime.rational(1),
									    runtime.rational(5),
									    runtime.rational(3)]));
	});


runTest('list*',
	function() {
		testPrim('list*', id, [runtime.EMPTY], runtime.EMPTY);
		testPrim('list*', id, [runtime.rational(1), runtime.pair(runtime.rational(2), runtime.EMPTY)],
			 runtime.list([runtime.rational(1), runtime.rational(2)]));
		testPrim('list*', id, [1, 2, 3, runtime.list([4, 5])], runtime.list([1, 2, 3, 4, 5]));
	});


runTest('length',
	function() {
		testPrim('length', id, [runtime.EMPTY], 0);
		testPrim('length', id, [runtime.list([1])], 1);
		testPrim('length', id, [runtime.list([1, 2, 3, 4])], 4);
	});


runTest('append',
	function() {
		testPrim('append', id, [], runtime.EMPTY);
		testPrim('append', id, [runtime.list([1])], runtime.list([1]));
		testPrim('append', id, [runtime.EMPTY, runtime.list([1, 2, 3]), runtime.list([1, 2])],
			 runtime.list([1, 2, 3, 1, 2]));
	});


runTest('reverse',
	function() {
		testPrim('reverse', id, [runtime.EMPTY], runtime.EMPTY);
		testPrim('reverse', id, [runtime.list([1])], runtime.list([1]));
		testPrim('reverse', id, [runtime.list([1, 2, 3, 4, 5])], runtime.list([5, 4, 3, 2, 1]));
	});


runTest('list-ref',
	function() {
		var testList = runtime.list([runtime.rational(1),
					     runtime.rational(1),
					     runtime.rational(2),
					     runtime.rational(3),
					     runtime.rational(5),
					     runtime.rational(8),
					     runtime.rational(11)]);
		testPrim('list-ref', id, [testList, runtime.rational(0)], runtime.rational(1));
		testPrim('list-ref', id, [testList, runtime.rational(5)], runtime.rational(8));
	});



runTest("topsyntax",
	function() {
	    sys.print("!Not implemented yet!  ");
	});



sys.print("\nEND TESTS\n")
