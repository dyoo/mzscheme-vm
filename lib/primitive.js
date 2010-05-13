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


// FIXME: how do we properly write primitives that have to do higher-order
// things?  The code for for-each seems extremely fragile.
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


PRIMITIVES['*'] = 
    new Primitive('*',
		  0,
		  true, false,
		  function(args) {
		      var result = types.rational(1);
		      for(var i = 0; i < args.length; i++) {
			  result = types.NumberTower.multiply(args[i], result);
		      }
		      return result;
		  });


PRIMITIVES['string-append'] = 
    new Primitive("string-append",
		  0,
		  true, false,
		  function(args) {
		      return args.join("");
		  });



PRIMITIVES['-'] = 
    new Primitive("-",
		  1,
		  true, false,
		  function(x, args) {
		      if (args.length == 0) { 
			  return types.NumberTower.subtract(types.rational(0), x);
		      }
		      var result = x;
		      for (var i = 0; i < args.length; i++) {
			  result = types.NumberTower.subtract(result, args[i]);
		      }
		      return result;
		  });


PRIMITIVES['zero?'] =
    new Primitive("zero?",
		  1,
		  false, false,
		  function(x) {
		      return types.NumberTower.equal(types.rational(0), x)
		  });

PRIMITIVES['sub1'] =
    new Primitive("sub1",
		  1,
		  false, false,
		  function(x) {
		      return types.NumberTower.subtract(x, types.rational(1))
		  });



//////////////////////////////////////////////////////////////////////



exports.getPrimitive = function(name) {
    return PRIMITIVES[name];
};

exports.isPrimitive = function(x) {
    return x instanceof Primitive;
};

exports.Primitive = Primitive;