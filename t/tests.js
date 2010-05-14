var assert = require('assert');
var runtime = require('./../lib');
var sys = require('sys');


//////////////////////////////////////////////////////////////////////

var EXIT_ON_FIRST_ERROR = true;


//////////////////////////////////////////////////////////////////////


var makeConstant = function(c) { return {$:'constant', value:c}; };


var makeBranch = function(x, y, z) { 
    var b = {};
    b['$'] = 'branch';
    b['test'] = x;
    b['then'] = y;
    b['else'] = z;
    return b;
};

var makeSeq = function() {
    return { $: 'seq',
	     forms: arguments };};

var makeBeg0 = function() {
    return { $: 'beg0',
	     seq: arguments };};


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
    return { $: 'prefix',
	     'num-lifts': runtime.rational(0),
	     toplevels: arr };
};


var makeMod = function(prefix, body) {
    return { $: 'mod', 
	     prefix: prefix,
	     body : body };
};


var makeToplevel = function(depth, pos) {
    return {$:'toplevel',
	    depth: runtime.rational(depth),
	    pos:runtime.rational(pos)};
};


var makeDefValues = function(ids, body) {
    return {$:"def-values",
	    ids: ids,
	    body: body};
};

var makeLam = function(arity, closureMap, body) {
    var aClosureMap = [];
    var aClosureTypes = [];
    var aParamTypes = [];
    for (var i = 0; i < closureMap.length; i++) {
	aClosureMap.push(runtime.rational(closureMap[i]));
	aClosureTypes.push(runtime.symbol("val/ref"));
    }
    for (var i = 0; i < arity; i++) {
	aParamTypes.push(runtime.symbol("val"));
    }
    return {'$':"lam",
	    'num-params': runtime.rational(arity),
	    'param-types': aParamTypes,
	    'rest?': false,
	    'closure-map' : aClosureMap,
	    'closure-types' : aClosureTypes,
	    'body': body};	    
};


var makePrimval = function(name) {
    return {$: 'primval',
	    'value': name};
};

var makeApplication = function(rator, rands) {
    assert.ok(typeof(rands) === 'object' && rands.length !== undefined);
    return {$ : 'application',
	    rator: rator,
	    rands: rands};
};


var makeLocalRef = function(n) {
    return {$ : 'localref',
	    pos: runtime.rational(n)};
};


var makeApplyValues = function(proc, argsExpr) {
    return {$:'apply-values',
	    'proc': proc,
	    'args-expr': argsExpr};
};

var makeLet1 = function(rhs, body) {
    return {$: 'let-one',
	    rhs : rhs,
	    body : body};
};

var makeLetVoid = function(count, isBoxes, body) {
    return {$: 'let-void',
	    count: runtime.rational(count),
	    'boxes?' : isBoxes,
	    body : body};
};


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
	    state.run();
	    assert.ok(state.isStuck());
	});



// Numeric constants should just evaluate through.
runTest("Numeric constant", 
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant(42));
	    var result = state.run();
	    assert.deepEqual(result, 
			     42);
	    
	    assert.deepEqual(state, makeStateWithConstant(42));
	});



// String constant.
runTest("String constant",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant("hello world"));
	    var result = state.run();
	    assert.deepEqual(result, 
			     "hello world");

	    assert.deepEqual(state, makeStateWithConstant("hello world"));
	});


// boolean constant.
runTest("Boolean constant", 
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeConstant(true));
	    var result = state.run();
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
	    var result = state.run();
	    assert.deepEqual(result, true);
	});


// Simple branch to false
runTest("Simple boolean branch to false",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBranch(makeConstant(false),
					 makeConstant(false),
					 makeConstant(true)));
	    var result = state.run();
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
	    var result = state.run();
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
	    state1.step();
	    state1.step();
	    assert.ok(!state1.isStuck());
	    assert.deepEqual(state1.v, 3);
	    state1.step();
	    assert.deepEqual(state1.v, 4);
	    var result = state1.run();
	    assert.deepEqual(result, 5);

	    assert.deepEqual(state1, makeStateWithConstant(5));    
	});



// Module prefix
runTest("module prefix",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    state.run();   
	    assert.equal(1, state.vstack.length);
	    assert.ok(state.vstack[0] instanceof runtime.Prefix);
	    assert.equal(state.vstack[0].length(), 3);
	});


runTest("toplevel lookup",
	// toplevel lookup
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3),
				      []));
	    state.run();   

	    state.vstack[0].set(0, "zero");
	    state.vstack[0].set(1, "one");
	    state.vstack[0].set(2, "two");

	    state.pushControl(makeToplevel(0, 0));
	    assert.equal(state.run(), "zero");

	    state.pushControl(makeToplevel(0, 1));
	    assert.equal(state.run(), "one");

	    state.pushControl(makeToplevel(0, 2));
	    assert.equal(state.run(), "two");
	});



runTest("define-values",
	// define-values
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    state.run();   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("try it")));
	    state.run();

	    var expectedState = new runtime.State();
	    expectedState.pushControl(makeMod(makePrefix(3),
					      []));
	    expectedState.run();   
	    expectedState.v = "try it";
	    expectedState.vstack[0].set(0, "try it");
	    assert.deepEqual(state, expectedState);
	});


runTest("lambda",
	// lambda
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(3), []));
	    state.run();   
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeConstant("Some toplevel value")));

	    state.run();
	    state.pushControl(makeLam(3, [0], makeConstant("I'm a body")));

	    var result = state.run();

	    // result should be a lambda.
	    assert.ok(result instanceof runtime.ClosureValue);
	    assert.equal(result.closureVals.length, 1);
	    assert.ok(result.closureVals[0] instanceof runtime.Prefix);
	    assert.deepEqual(result.body, makeConstant("I'm a body"));
	    assert.equal(result.numParams, 3);
	});



runTest("primval (current-print)",
	// primval
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("current-print"));
	    var result = state.run();
	    assert.ok(result instanceof runtime.Primitive);
	});


runTest("primval on bad primitive should throw error",
	// primval on unknowns should throw error
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("foobar"));
	    assert.throws(function() { state.run(); });
	});


runTest("Primval on *",
	// primval on *
	// primval
	function() {
	    var state = new runtime.State();
	    state.pushControl(makePrimval("*"));
	    var result = state.run();
	    assert.ok(result instanceof runtime.Primitive);
	});


runTest("My own list function",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication
			      ( {'$' : "lam",
				      'num-params': runtime.rational(0),
				      'param-types': [],
				      'rest?': true,
				      'closure-map' : [],
				      'closure-types' : [],
				      'body': makeLocalRef(0)},
				  [makeConstant("one"),
				   makeConstant("two"),
				   makeConstant("three")]));
	    var result = state.run();
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
	    var result = state.run();
	    assert.deepEqual(result, runtime.rational(15));
	    assert.equal(state.vstack.length, 0);
	});


runTest("primitive application, no arguments",
	// primitive application with no arguments.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makePrimval("*"),
					      []));
	    var result = state.run();
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
	    var result = state.run();
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
	    var result = state.run();
	    assert.deepEqual(result, "helloworldtesting");
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
	    var result = state.run();
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
	    var result = state.run();
	    assert.deepEqual(result, runtime.rational(-1024));
	    assert.equal(state.vstack.length, 0);
	});


runTest("closure application",
	// Closure application
	// lambda will just return a constant value
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    state.run();   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeConstant("I'm a body"))));
	    state.run();
	    state.pushControl(makeApplication(makeToplevel(1, 0), [makeConstant("boo")]));
	    var result = state.run();
	    assert.equal(result, "I'm a body");

	    assert.equal(state.vstack.length, 1);
	});


runTest("closure application, defining square",
	// Closure application
	// lambda will square its argument
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    state.run();   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(1, [],
						    makeApplication(makePrimval("*"),
								    [makeLocalRef(2),
								     makeLocalRef(2)]))));
	    state.run();
	    state.pushControl(makeApplication(makeToplevel(1, 0), 
					      [makeConstant(runtime.rational(4))]));
	    var result = state.run();
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
	    state.run();   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues([makeToplevel(0, 0)],
					    makeLam(0, [0],
						    makeApplication(makeToplevel(0, 0),
								    []))));
	    state.run();
	    state.pushControl(makeApplication(makeToplevel(0, 0), []));
	    var MAXIMUM_BOUND = 5;
	    var ITERATIONS = 1000000;
	    for (var i = 0; i < ITERATIONS; i++) {
		state.step();
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
	    state.run();   
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
	    
	    state.run();

	    var even = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(runtime.rational(n))]));
		var MAXIMUM_BOUND = 10;
		while (!state.isStuck()) {
		    state.step();
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





runTest("zero?",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makePrimval("zero?"),
					      [makeConstant(runtime.rational(0))]));
	    assert.deepEqual(state.run(), true);

	    state.pushControl(makeApplication(makePrimval("zero?"),
					      [makeConstant(runtime.rational(1))]));
	    assert.deepEqual(state.run(), false);
	    
	});



runTest("sub1",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeApplication(makePrimval("sub1"),
					      [makeConstant(runtime.rational(25))]));
	    assert.deepEqual(state.run(), runtime.rational(24));
	});



runTest("factorial",
	// Checking tail calling behavior
	// The standard infinite loop should consume bounded control stack.
	// (define (f) (f)) (begin (f)) --> infinite loop, but with bounded control stack.
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(1), []));
	    state.run();   
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

	    state.run();

	    var fact = function(n) {
		state.pushControl(makeApplication(makeToplevel(1, 0),
						  [makeConstant(runtime.rational(n))]));
		return state.run().toFixnum();;
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
	    assert.deepEqual(state.run().toFixnum(),
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
	    assert.deepEqual(state.run().toFixnum(),
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
	    assert.deepEqual(state.run().toFixnum(),
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
	    var result = state.run();
	    assert.equal(state.vstack.length, 0);
	    assert.ok(result instanceof runtime.ValuesWrapper);
	    assert.equal(result.elts.length, 2);
	});



runTest("values with def-values",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    state.run();   
	    assert.equal(state.vstack.length, 1);
	    
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant("hello"),
				 makeConstant("world")])));
	    state.run();
	    assert.equal(state.vstack.length, 1);
	    assert.ok(state.vstack[0] instanceof runtime.Prefix);
	    assert.equal(state.vstack[0].ref(0), "hello");
	    assert.equal(state.vstack[0].ref(1), "world");
	});



runTest("apply-values",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    state.run();   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makeConstant("hello"),
				 makeConstant("world")])));
	    state.run();

	    state.pushControl(makeApplyValues(
		makeLam(2, [], makeApplication(makePrimval("string-append"),
					       [makeLocalRef(2),
						makeLocalRef(3)])),
		makeApplication(makePrimval("values"),
				[makeToplevel(2, 0),
				 makeToplevel(2, 1)])));
	    assert.equal(state.run(), "helloworld");
	});



runTest("apply-values, testing no stack usage",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeMod(makePrefix(2), []));
	    state.run();   
	    state.pushControl(makeDefValues(
		[makeToplevel(0, 0),
		 makeToplevel(0, 1)],
		makeApplication(makePrimval("values"),
				[makePrimval("zero?"),
				 makeConstant(runtime.rational(0))])));
	    state.run();

	    state.pushControl(makeApplyValues(
		makeToplevel(0, 0),
		makeToplevel(0, 1)));
	    assert.equal(state.run(), true);
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
		state.step();
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = state.run();
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
		state.step();
	    }
	    assert.equal(state.vstack.length, 1);
	    assert.equal(state.vstack[0], "someValue");
	    var result = state.run();
	    assert.equal(state.vstack.length, 0);
	    assert.deepEqual(result, "something else");
	});


runTest("let-void, no boxes",
	function() {
	    var state = new runtime.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, false, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		state.step();
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok(state.vstack[i] instanceof runtime.UndefinedValue);
	    }
	    var result = state.run();
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("let-void, with boxes",
	function() {
	    var state = new runtime.State();
	    var body = makeConstant("blah");
	    state.pushControl(makeLetVoid(2, true, body));
	    while (state.cstack[state.cstack.length - 1] !== body) {
		state.step();
	    }
	    assert.equal(state.vstack.length, 2);
	    for(var i = 0; i < state.vstack.length; i++) {
		assert.ok(state.vstack[i] instanceof runtime.Box);
	    }
	    var result = state.run();
	    assert.equal(result, "blah");
	    assert.equal(state.vstack.length, 0);
	});


runTest("beg0 with just one argument should immediately reduce to its argument",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBeg0(makeConstant("first post")));
	    state.step();
	    assert.equal(state.cstack.length, 1);
	    assert.deepEqual(state.cstack[0], 
			     makeConstant("first post"));
	    var result = state.run();
	    assert.equal(result, "first post");
	});



runTest("beg0, more general",
	function() {
	    var state = new runtime.State();
	    state.pushControl(makeBeg0(makeConstant("first post"),
				       makeConstant("second post"),
				       makeConstant("third post"),
				       makeConstant("fourth post")));
	    state.step();

	    // By this point, there should be two elements
	    // in the control stack, the evaluation of the first
	    // argument, and a control to continue the
	    // rest of the sequence evaluation.
	    assert.equal(state.cstack.length, 2); 
	    var result = state.run();
	    assert.equal(result, "first post");
	});



runTest("boxenv",
	function() {
	});



// What's left to implement?
//
// closure
// case-lam
// install-value
// let-rec
// boxenv
// topsyntax
// with-cont-mark
// assign
// varref





sys.print("\nEND TESTS\n")