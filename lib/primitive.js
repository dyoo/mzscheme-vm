var types = require("./types");
var sys = require("sys");
var jsnums = require('./js-numbers');
var assert = require('assert');






var CALL;
var setCALL = function(v) {
    CALL = v;
};







var PRIMITIVES = {};

var Primitive = function(name, numParams, isRest, usesState, impl) {
    this.name = name;
    this.numParams = numParams;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};


var CasePrimitive = function(cases) {
    this.cases = cases;
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


PRIMITIVES['display'] = 
    new CasePrimitive(
	[new Primitive('display', 1, false, false, function(x) {
	    sys.print('' + x);
	    return UNDEF;
	}),
	 new Primitive('display', 2, false, false, function(x, port) {
	     // FIXME
	     throw new Error("display to a port not implemented yet.");
	 } )]);



PRIMITIVES['newline'] = 
    new CasePrimitive(
	[new Primitive('newline', 0, false, false, function() {
	    sys.print('\n');
	    return UNDEF;
	}),
	 new Primitive('newline', 1, false, false, function(port) {
	     // FIXME
	     throw new Error("newline to a port not implemented yet.");
	 } )]);



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
		      sys.print(sys.inspect(vals));
		      return UNDEF;
		  });


// FIXME: how do we properly write primitives that have to do higher-order
// things?  The code for for-each seems extremely fragile.
// This is not right for many reasons... it should return UNDEF
// at the end.
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





PRIMITIVES['*'] = 
    new Primitive('*',
		  0,
		  true, false,
		  function(args) {
		      var result = types.rational(1);
		      for(var i = 0; i < args.length; i++) {
			  result = jsnums.multiply(args[i], result);
		      }
		      return result;
		  });



PRIMITIVES['<'] = 
    new Primitive('<',
		  2,
		  true, false,
		  function(x, y, args) {
		      if (! jsnums.lessThan(x, y)) {
			  return false;
		      }
		      for(var i = 0; i < args.length; i++) {
			  if (! (jsnums.lessThan(y, args[i]))) {
			      return false;
			  }
			  y = args[i];
		      }
		      return true;
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
			  return jsnums.subtract(0, x);
		      }
		      var result = x;
		      for (var i = 0; i < args.length; i++) {
			  result = jsnums.subtract(result, args[i]);
		      }
		      return result;
		  });


PRIMITIVES['+'] = 
    new Primitive("+",
		  0,
		  true, false,
		  function(args) {
		      if (args.length == 0) { 
			  return 0;
		      }
		      var result = args[0];
		      for (var i = 1; i < args.length; i++) {
			  result = jsnums.add(result, args[i]);
		      }
		      return result;
		  });


PRIMITIVES['='] = 
    new Primitive("=",
		  2,
		  true, false,
		  function(x, y, args) {
		      if (! jsnums.equals(x, y)) {
			  return false;
		      }
		      for (var i = 0; i < args.length; i++) {
			  if (! jsnums.equals(y, args[i])) {
			      return false;
			  }
		      }
		      return true;
		  });



PRIMITIVES['random'] =
    new Primitive("random",
		  0,
		  true, false,
		  function(args) {
		      if (args.length === 0) {
			  throw new Error("random not implemented yet\n");
		      } else if (args.length === 1) {
			  // FIXME: NOT RIGHT
			  sys.print("FIXME: random not implemented correctly yet\n");
			  return 0;
		      } else if (args.length === 2) {
			  throw new Error("random not implemented yet\n");
		      }
		  });


PRIMITIVES['zero?'] =
    new Primitive("zero?",
		  1,
		  false, false,
		  function(x) {
		      return jsnums.equals(0, x)
		  });


PRIMITIVES['sub1'] =
    new Primitive("sub1",
		  1,
		  false, false,
		  function(x) {
		      return jsnums.subtract(x, 1);
		  });


PRIMITIVES['expt'] = 
    new Primitive("expt",
		  2,
		  false, false,
		  function(x, y) {
		      return jsnums.expt(x, y);
		  }
		 );


PRIMITIVES['make-thread-cell'] = 
    new CasePrimitive([
	new Primitive("make-thread-cell",
		      1, false, false,
		      function(x) {
			  return new types.ThreadCell(x, false);
		      }
		     ),
	new Primitive("make-thread-cell",
		      2, false, false,
		      function(x, y) {
			  return new types.ThreadCell(x, y);
		      }
		     )]);



PRIMITIVES['make-continuation-prompt-tag'] = 
    new CasePrimitive([
	new Primitive("make-continuation-prompt-tag",
		      0, false, false,
		      function() {
			  return new types.ThreadCell();
		      }
		     ),
	new Primitive("make-continuation-prompt-tag",
		      1, false, false,
		      function(x) {
			  return new types.ThreadCell(x);
		      }
		     )]);



var makeOptionPrimitive = function(name,
				   numArgs,
				   defaultVals,
				   bodyF) {
    var makeNthPrimitive = function(n) {
	return new Primitive(name,
			     numArgs + n,
			     false,
			     false,
			     function() {
				 assert.equal(arguments.length,
					      numArgs + n);
				 var args = [];
				 for (var i = 0; i < arguments.length; i++) {
				     args.push(arguments[i]);
				 }
				 return bodyF.apply(
				     bodyF,
				     args.concat(defaultVals.slice(i, defaultVals.length)));
			     });
    };
	
    var cases = [];
    for (var i = 0; i <= defaultVals.length; i++) {
	cases.push(makeNthPrimitive(i));
    }
    return new CasePrimitive(cases);
};




PRIMITIVES['make-struct-type'] =
	makeOptionPrimitive(
	    'make-struct-type',
	    4,
	    [false, 
	     types.EMPTY,
	     types.symbol("prefab"),
	     false,
	     types.EMPTY,
	     false],
	    function(name,
 	 	     superType,	// FIXME: currently ignored
 	 	     initFieldCnt, 
 	 	     autoFieldCnt,
 		     autoV,
 	 	     props,	// FIXME: currently ignored
 	 	     inspector, // FIXME: currently ignored
 	 	     procSpec, 	// FIXME: currently ignored
 	 	     immutables, // FIXME: currently ignored
 	 	     guard) {	 // FIXME: currently ignored
		var aStructType = 
		  types.makeStructureType(name, initFieldCnt, autoFieldCnt, autoV);
		return new types.ValuesWrapper
		([aStructType,
		  (new Primitive('constructor',
				 initFieldCnt,
				 true, 
				 false,
				 function(){ 
				     var args = [];
				     for (var i = 0; i < arguments.length; i++) {
					 args.push(arguments[i]);
				     }
				     for (var i = 0; i < initFieldCnt + autoFieldCnt - arguments.length; i++) {
					 args.push(autoV);
				     }
				     return aStructType.constructor(args);
				 })),
		  (new Primitive('predicate',
				 1,
				 false,
				 false,
				 function(x) {
				     return aStructType.predicate(x);
				 })),
		  (new Primitive('accessor',
				 2,
				 false,
				 false,
				 function(x, i) {
				     return aStructType.accessor(x, jsnums.toFixnum(i));
				 })),
		  (new Primitive('mutator',
				 3,
				 false,
				 false,
				 function(x, i, v) {
				     aStructType.mutator(x, jsnums.toFixnum(i), v)
				 }))]);
	    });
			    
			   
PRIMITIVES['make-struct-field-accessor'] =
	makeOptionPrimitive(
	    'make-struct-field-accessor',
	    2,
	    [false],
	    function(accessor, fieldPos, fieldName) {
		return new Primitive(fieldName, 
				     1, 
				     false,
				     false,
				     function(x) {

					 return accessor.impl(x, fieldPos);
				     });
	    });



PRIMITIVES['make-struct-field-mutator'] =
	makeOptionPrimitive(
	    'make-struct-field-mutator',
	    2,
	    [false],
	    function(mutator, fieldPos, fieldName) {
		return new Primitive(fieldName, 
				     2, 
				     false,
				     false,
				     function(x, v) {
					 return mutator.impl(x, fieldPos, v);
				     });
	    });



PRIMITIVES['current-inexact-milliseconds'] =
    new Primitive(
	'current-inexact-milliseconds',
	0,
	false, false,
	function() {
	    return jsnums.makeFloat((new Date()).valueOf());
	});
	
    



//////////////////////////////////////////////////////////////////////



exports.getPrimitive = function(name) {
    return PRIMITIVES[name];
};

exports.isPrimitive = function(x) {
    return x instanceof Primitive;
};

exports.addPrimitive = function(name, aPrim) {
    PRIMITIVES[name] = aPrim;
};

exports.Primitive = Primitive;
exports.CasePrimitive = CasePrimitive;


exports.setCALL = setCALL;