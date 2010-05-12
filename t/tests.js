var assert = require('assert');
var runtime = require('./../lib');
var sys = require('sys');

var makeConstant = function(c) { return {$:'constant', value:c}; };



// Simple running should just terminate.
(function() { 
    var state = new runtime.State();
    state.run();
})();



// Numeric constant
(function() {
    var state = new runtime.State();
    state.pushControl(makeConstant(42));
    var result = state.run();
    assert.deepEqual(result, 
		     42);
})();



// String constant.
(function() {
    var state = new runtime.State();
    state.pushControl(makeConstant("hello world"));
    var result = state.run();
    assert.deepEqual(result, 
		     "hello world");
})();




// Simple branch to true
(function() {
    var state = new runtime.State();
    state.pushControl({'$' : 'branch', 
		       'test' : makeConstant(true),
		       'then' : makeConstant(true),
		       'else' : makeConstant(false)});
    var result = state.run();
    assert.deepEqual(result, 
		     true);
})();


// Simple branch to false
(function() {
    var state = new runtime.State();
    state.pushControl({'$' : 'branch', 
		       'test' : makeConstant(false),
		       'then' : makeConstant(false),
		       'else' : makeConstant(true)});
    var result = state.run();
    assert.deepEqual(result, 
		     true);
})();



// Simple branch to false
(function() {
    var state1 = new runtime.State();
    state.pushControl({'$' : 'seq', 
		       forms : [makeConstant(3),
				makeConstant(4),
				makeConstant(5)]}),
    var result = state1.run();
    assert.deepEqual(result, 5);

    
    var state2 = new runtime.State();
    state.pushControl(makeConstant(5));

    
    assert.deepEqual(state2, state1);
    
})();
