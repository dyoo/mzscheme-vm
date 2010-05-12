var assert = require('assert');
var runtime = require('./../lib');
var sys = require('sys');


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
	     body:body };
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
    return {$ : 'application',
	    rator: rator,
	    rands: rands};
};

//////////////////////////////////////////////////////////////////////



// Simple running should just terminate, and always be at the "stuck" state.
(function() { 
    var state = new runtime.State();
    assert.ok(state.isStuck());
    state.run();
    assert.ok(state.isStuck());
})();



// Numeric constants should just evaluate through.
(function() {
    var state = new runtime.State();
    state.pushControl(makeConstant(42));
    var result = state.run();
    assert.deepEqual(result, 
		     42);

    assert.deepEqual(state, makeStateWithConstant(42));
})();



// String constant.
(function() {
    var state = new runtime.State();
    state.pushControl(makeConstant("hello world"));
    var result = state.run();
    assert.deepEqual(result, 
		     "hello world");

    assert.deepEqual(state, makeStateWithConstant("hello world"));
})();


// boolean constant.
(function() {
    var state = new runtime.State();
    state.pushControl(makeConstant(true));
    var result = state.run();
    assert.deepEqual(result, true);

    assert.deepEqual(state, makeStateWithConstant(true));
})();



// Simple branch to true
(function() {
    var state = new runtime.State();
    state.pushControl(makeBranch(makeConstant(true),
				 makeConstant(true),
				 makeConstant(false)));
    var result = state.run();
    assert.deepEqual(result, true);
})();


// Simple branch to false
(function() {
    var state = new runtime.State();
    state.pushControl(makeBranch(makeConstant(false),
				 makeConstant(false),
				 makeConstant(true)));
    var result = state.run();
    assert.deepEqual(result, 
		     true);

    assert.deepEqual(state, makeStateWithConstant(true));
})();



// (if (if true false true) "apple" "pie") --> "pie"
(function() {
    var state = new runtime.State();
    state.pushControl(makeBranch(makeBranch(makeConstant(true), makeConstant(false), makeConstant(true)),
				 makeConstant("apple"),
				 makeConstant("pie")));
    var result = state.run();
    assert.deepEqual(result, "pie");

    assert.deepEqual(state, makeStateWithConstant("pie"));
})();



// Sequences
(function() {
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
})();



// Module prefix
(function() {
    var state = new runtime.State();
    state.pushControl(makeMod(makePrefix(3),
			      []));
    state.run();   
    assert.equal(1, state.vstack.length);
    assert.ok(state.vstack[0] instanceof runtime.Prefix);
    assert.equal(state.vstack[0].length(), 3);
})();



// toplevel lookup
(function() {
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
})();




// define-values
(function() {
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
})();



// lambda
(function() {
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
})();



// primval
(function() {
    var state = new runtime.State();
    state.pushControl(makePrimval("current-print"));
    var result = state.run();
    assert.ok(result instanceof runtime.Primitive);
})();



// primval on unknowns should throw error
(function() {
    var state = new runtime.State();
    state.pushControl(makePrimval("foobar"));
    assert.throws(function() { state.run(); });
})();


// primval on *
// primval
(function() {
    var state = new runtime.State();
    state.pushControl(makePrimval("*"));
    var result = state.run();
    assert.ok(result instanceof runtime.Primitive);
})();




// primitive application
(function() {
    var state = new runtime.State();
    state.pushControl(makeApplication(makePrimval("*"),
				      [makeConstant(runtime.rational(3)),
				       makeConstant(runtime.rational(5))]));
    var result = state.run();
    assert.deepEqual(result, runtime.rational(15));
})();

