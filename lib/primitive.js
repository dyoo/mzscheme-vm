var types = require("./types");


var PRIMITIVES = {};

var Primitive = function(name, arity, isRest, usesState, impl) {
    this.name = name;
    this.arity = arity;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};



var defaultPrint = 
    new Primitive('print', 1, false, false, function(x) {
	    sys.print(''+x);
	});


PRIMITIVES['current-print'] =
    new Primitive('current-print', 
		  0, 
		  false, false,
		  function() {
		      return defaultPrint;
		  });


PRIMITIVES['for-each'] =
    new Primitive('for-each', 
		  2, 
		  true, true,
		  function(state, f, firstArg, restArgs) {
		      //throw new Error("for-each not implemented yet");
		  });


PRIMITIVES['values'] =
    new Primitive('values', 
		  1, 
		  true, false,
		  function(firstArg, restArgs) {
		      var bundledArgs = [firstArg].concat(restArgs);
		      return new types.ValueWrapper(bundledArgs);
		  });


PRIMITIVES['apply'] =
    new Primitive('apply', 
		  2, 
		  true, true,
		  function(state, f, firstArg, secondArg, restArgs) {
		      state.pushValue(firstArg);
		      state.pushValue(secondArg);
		      for(var i = 0; i < restArgs.length; i++) {
			  state.pushValue(restArgs[i]);
		      }
		      state.setValue(f);
		      state.pushControl(state.CALL(restArgs.length + 2));
		  });







//////////////////////////////////////////////////////////////////////


exports.isPrimitive = function(x) {
    return x instanceof Primitive;
};

exports.getPrimitive = function(name) {
    return PRIMITIVES[name];
}