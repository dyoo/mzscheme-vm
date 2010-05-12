var assert = require('assert');
var runtime = require('./../lib');
var sys = require('sys');





//////////////////////////////////////////////////////////////////////
(function() { 

    var state = new runtime.State();
    state.run();
})();



//////////////////////////////////////////////////////////////////////

// Constants
(function() {
    var state = new runtime.State();
    state.pushControl({$:'constant', value: 42});
    var result = state.run();
    assert.deepEqual(result, 
		     42);
})();



(function() {
    var state = new runtime.State();
    state.pushControl({$:'constant', value:"hello world"});
    var result = state.run();
    assert.deepEqual(result, 
		     "hello world");
})();
//////////////////////////////////////////////////////////////////////