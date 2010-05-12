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
