var types = require("./types");
var sys = require("sys");


var PRIMITIVES = {};

var Primitive = function(name, arity, isRest, usesState, impl) {
    this.name = name;
    this.arity = arity;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};

var UNDEF = new types.UndefinedValue();


var defaultPrint = 
    new Primitive('print', 
		  1, 
		  false, 
		  false, 
		  function(x) {
		      sys.print(''+ x + '\n');
		      return UNDEF;
		  });


PRIMITIVES['current-print'] =
    new Primitive('current-print', 
		  0, 
		  false, false,
		  function() {
		      return defaultPrint;
		  });


PRIMITIVES['printf'] = 
    new Primitive('printf',
		  1,
		  true, false,
		  function(fmtString, vals) {
		      // FIXME
		      sys.print("printf not implemented yet.  " + fmtString);
		      sys.p(vals);
		      return UNDEF;
		  });


PRIMITIVES['for-each'] =
    new Primitive('for-each', 
		  2, 
		  true, true,
		  function(state, f, firstArg, restArgs) {
		      var firstList = firstArg.reverse();
		      var restLists = [];
		      for (var i = 0; i < restArgs.length; i++) { 
			  restLists.push(restArgs[i].reverse());
		      }
		      while (!firstList.isEmpty()) {
			  var aSlice = [firstList.first()];
			  for (var i = 0; i < restLists.length; i++) {
			      aSlice.push(restLists[i].first());
			      restLists[i] = reslLists[i].rest();
			  }
			  PRIMITIVES['apply'].impl(state,
						   f,
						   types.list(aSlice),
						   []);
			  firstList = firstList.rest();
		      }
		  });


PRIMITIVES['values'] =
    new Primitive('values', 
		  1, 
		  true, false,
		  function(firstArg, restArgs) {
		      var bundledArgs = [firstArg].concat(restArgs);
		      if (bundledArgs.length === 1) {
			  return firstArg;
		      }
		      return new types.ValuesWrapper(bundledArgs);
		  });


PRIMITIVES['apply'] =
    new Primitive('apply', 
		  2, 
		  true, true,
		  function(state, f, firstArg, restArgs) {
		      sys.print("apply");

		      var numArgs = 0;
		      restArgs.unshift(firstArg);
		      for(var i = 0; i < restArgs.length - 1; i++) {
			  state.pushValue(restArgs[i]);
			  numArgs++;
		      }
		      if (restArgs.length > 0) {
			  var lastLst = restArgs[restArgs.length - 1];
			  while (! lastLst.isEmpty()) {
			      state.pushValue(lastLst.first());
			      numArgs++;
			      lastLst = lastLst.rest();
			  }
		      }
		      state.setValue(f);
		      state.pushControl(state.CALL(numArgs));
		  });







//////////////////////////////////////////////////////////////////////



exports.getPrimitive = function(name) {
    return PRIMITIVES[name];
}

exports.Primitive = Primitive;