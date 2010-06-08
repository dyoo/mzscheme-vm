/*
var types = require("./types");
var sys = require("sys");
var jsnums = require('./js-numbers');
var assert = require('assert');
*/

var primitive = {};

(function() {


var CALL;
var setCALL = function(V) {
    CALL = function(op, operands, k) {
	return new V(op, operands, k);
    };
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


var UNDEF = types.UNDEFINED;

//////////////////////////////////////////////////////////////////////

// Helper Functions

var length = function(lst) {
	checkList(lst, 'length', 1);
	var ret = 0;
	for (; !lst.isEmpty(); lst = lst.rest()) {
		ret = ret+1;
	}
	return ret;
}

var append = function(args) {
	var i;
	arrayEach(args, function(x, i) {checkList(x, 'append', i+1);});

	if (args.length == 0) {
		return types.EMPTY;
	}
	var ret = args[0];
	for (i = 1; i < args.length; i++) {
		ret = ret.append(args[i]);
	}
	return ret;
}

var foldHelp = function(f, acc, args) {
	if ( args[0].isEmpty() ) {
		return acc;
	}

	var fArgs = [];
	var argsRest = [];
	for (var i = 0; i < args.length; i++) {
		fArgs.push(args[i].first());
		argsRest.push(args[i].rest());
	}
	fArgs.push(acc);
	return CALL(f, fArgs,
		function(result) {
			foldHelp(f, result, argsRest);
		});
}

var schemeListToList = function(lst) {
	var result = [];
	while ( !lst.isEmpty() ) {
		result.push(lst.first());
		lst = lst.rest();
	}
}

var compare = function(args, comp) {
	var curArg = args[0];
	for (var i = 1; i < args.length; i++) {
		if ( !comp(curArg, args[i]) ) {
			return false;
		}
		curArg = args[i];
	}
	return true;
}

// isAlphabeticString: string -> boolean
var isAlphabeticString = function(s) {
	for(var i = 0; i < s.length; i++) {
		if (! ((s[i] >= "a" && s[i] <= "z") ||
		       (s[i] >= "A" && s[i] <= "Z"))) {
			return false;
		}
	}
	return true;
}

// isWhitespaceString: string -> boolean
var isWhitespaceString = (function() {
	var pat = new RegExp("^\\s*$");
	return function(s) {
		return (s.match(pat) ? true : false);
	}
}());


//////////////////////////////////////////////////////////////////////

// Returns true if x is a number.
var isNumber = jsnums.isSchemeNumber;

var isSymbol = function(x) {
	return (x !== null && x !== undefined && x instanceof types.Symbol);
}

var isChar = function(x) {
	return x !== null && x !== undefined && x instanceof types.Char;
}


var isString = function(x) {
	return typeof(x) == 'string';
	//return x != null && x != undefined && x instanceof types.String;
}

var isBoolean = function(x) {
	return (x === true || x === false);
}

var isPair = function(x) {
	return x !== null && x !== undefined && x instanceof types.Cons;
}

var isEmpty = function(x) {
	return x === types.EMPTY;
}

var isReal = jsnums.isReal;
var isRational = jsnums.isRational;
var isComplex = isNumber;

/*
var isReal = function(x) {
	return (isNumber(x) && x.isReal());
}

var isRational = function(x) {
	return isNumber(x) && x.isRational();
}

var isComplex = function(x) {
	return isNumber(x);
}
*/

var isFunction = function(x) {
	return typeof(x) == 'function';
}

// Returns true if x is an integer.
var isInteger = function(x) {
	return (isNumber(x) && jsnums.isInteger(x));
}

var isNatural = function(x) {
	return isNumber(x) && jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 0);
}

var isBox = function(x) {
	return x !== null && x !== undefined && x instanceof types.Box;
}

var isHash = function(x) {
	return (x !== null &&
		x !== undefined &&
		(x instanceof types.EqHashTable ||
		 x instanceof types.EqualHashTable));
}


var sub1 = function(x) {
	check(x, isNumber, 'sub1', 'number', 1);
	return jsnums.subtract(x, 1);
}

var add1 = function(x) {
	check(x, isNumber, 'add1', 'number', 1);
	return jsnums.add(x, 1);
}

var isList = function(x) {
        return ((x instanceof types.Cons) || (x === types.EMPTY));
}


var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.apply(arr[i], [arr[i], i]);
	}
}

var check = function(x, f, functionName, typeName, position) {
//	sys.print("Function " + functionName + " checking " + typeName + "\n");
	
	if (! f(x)) {
// TODO Add some sort of error throw here!
	}
}

var checkList = function(x, functionName, position) {
	if (! isList(x)) {
//		sys.print("Non-list given as argment " + position + " in " + functionName + ", given: " + sys.inspect(x) + "\n");
// TODO Add some sort of exception throw in here!
	}
}

var checkListOf = function(lst, f, functionName, typeName, position) {
	if ( !isList(lst) ) {
		// TODO Add some sort of error throw here!
	}
	else {
		while( !lst.isEmpty() ) {
			if ( !f(lst.first()) ) {
				// TODO Throw some sort of error here!
			}
			lst = lst.rest();
		}
	}
}

var checkAllSameLength = function(lists, functionName) {
	if (lists.length == 0)
		return;
	
	var len = length(lists[0]);
	arrayEach(lists,
		function(lst) {
			if (length(lst) != len) {
				// TODO Throw an error saying lists need to be the same length
			}
		});
}

//////////////////////////////////////////////////////////////////////

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


PRIMITIVES['for-each'] =
    new Primitive('for-each', 
		  2, 
		  true, false,
		  function(f, firstArg, arglists) {
		  	arglists.unshift(firstArg);
			check(f, isFunction, 'for-each', 'function', 1);
			arrayEach(arglists, function(lst, i) {checkList(lst, 'for-each', i+2);});
			checkAllSameLength(arglists, 'for-each');

			var forEachHelp = function(args) {
				if (args[0].isEmpty()) {
					return types.VOID;
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length; i++) {
					argsFirst.push(args[i].first());
					argsRest.push(args[i].rest());
				}

				return CALL(f, argsFirst,
					function(result) {return forEachHelp(argsRest);});
			}

			return forEachHelp(arglists);
		  });



PRIMITIVES['string-append'] = 
    new Primitive("string-append",
		  0,
		  true, false,
		  function(args) {
		      return args.join("");
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



/*****************************
 *** Arithmetic Primitives ***
 *****************************/


PRIMITIVES['*'] = 
    new Primitive('*',
		  0,
		  true, false,
		  function(args) {
		      arrayEach(args, function(x) {check(x, isNumber, '*', 'number', i+1);});

		      var result = types.rational(1);
		      for(var i = 0; i < args.length; i++) {
			  result = jsnums.multiply(args[i], result);
		      }
		      return result;
		  });



PRIMITIVES['-'] = 
    new Primitive("-",
		  1,
		  true, false,
		  function(x, args) {
		      check(x, isNumber, '-', 'number', 1);
		      arrayEach(args, function(y) {check(y, isNumber, '-', 'number', i+2);});

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
		      arrayEach(args, function(x) {check(x, isNumber, '+', 'number', i+2);});

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
		  	args.unshift(y);
		  	args.unshift(x);
		  	arrayEach(args, function(z, i) {check(z, isNumber, '=', 'number', i+1);});

		  	return compare(args, jsnums.equals);
		  });
/*
		      check(x, isNumber, '=', 'number', 1);
		      arrayEach(args, function(y) {check(y, isNumber, '=', 'number', i+2);});

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
*/


PRIMITIVES['/'] =
    new Primitive('/',
		  1,
		  true, false,
		  function(x, args) {
		  	check(x, isNumber, '/', 'number', 1);
		  	arrayEach(args, function(y, i) {check(y, isNumber, '/', 'number', i+2);});
			
			if (args.length == 0) {
				return jsnums.divide(1, x);
			}

		  	var res = x;
		  	for (var i = 0; i < args.length; i++) {
				res = jsnums.divide(res, args[i]);
		  	}
		  	return res;
		  });



PRIMITIVES['sub1'] =
    new Primitive("sub1",
		  1,
		  false, false,
		  sub1);

PRIMITIVES['add1'] =
    new Primitive("add1",
		  1,
		  false, false,
		  add1);


PRIMITIVES['<'] = 
    new Primitive('<',
		  2,
		  true, false,
		  function(x, y, args) {
		  	args.unshift(y);
		  	args.unshift(x);
		  	arrayEach(args, function(z, i) {check(z, isNumber, '<', 'number', i+1);});

		  	return compare(args, jsnums.lessThan);
		  });
/*
		  	check(x, isReal, '<', 'real', 1);
			check(y, isReal, '<', 'real', 2);
			arrayEach(args, function(z, i) {check(z, isReal, '<', 'real', i+3);});

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
*/


PRIMITIVES['>'] =
    new Primitive('>',
		  2,
		  true, false,
		  function(x, y, args) {
		  	args.unshift(y);
		  	args.unshift(x);
		  	arrayEach(args, function(z, i) {check(z, isNumber, '>', 'number', i+1);});

		  	return compare(args, jsnums.greaterThan);
		  });
/*
		  	check(x, isReal, '>', 'real', 1);
			check(y, isReal, '>', 'real', 2);
			arrayEach(args, function(z, i) {check(z, isReal, '>', 'real', i+3);});

			if ( ! jsnums.greaterThan(x, y) ) {
				return false;
			}
			for (var i = 0; i < args.length; i++) {
				if ( !jsnums.greaterThan(y, args[i]) ) {
					return false;
				}
				y = args[i];
			}
			return true;
		  });
*/


PRIMITIVES['<='] = 
    new Primitive('<=',
		  2,
		  true, false,
		  function(x, y, args) {
		  	args.unshift(y);
		  	args.unshift(x);
		  	arrayEach(args, function(z, i) {check(z, isNumber, '<=', 'number', i+1);});

		  	return compare(args, jsnums.lessThanOrEqual);
		  });
/*
		  	check(x, isReal, '<=', 'real', 1);
			check(y, isReal, '<=', 'real', 2);
			arrayEach(args, function(z, i) {check(z, isReal, '<=', 'real', i+3);});

		  	if (! jsnums.lessThanOrEqual(x, y)) {
		  		return false;
		  	}
		  	for(var i = 0; i < args.length; i++) {
				if (! (jsnums.lessThanOrEqual(y, args[i]))) {
				return false;
				}
				y = args[i];
			}
			return true;
		  });
*/


PRIMITIVES['>='] =
    new Primitive('>=',
		  2,
		  true, false,
		  function(x, y, args) {
		  	args.unshift(y);
		  	args.unshift(x);
		  	arrayEach(args, function(z, i) {check(z, isNumber, '>=', 'number', i+1);});

		  	return compare(args, jsnums.greaterThanOrEqual);
		  });
/*
		  	check(x, isReal, '>=', 'real', 1);
			check(y, isReal, '>=', 'real', 2);
			arrayEach(args, function(z, i) {check(z, isReal, '>=', 'real', i+3);});

			if ( ! jsnums.greaterThanOrEqual(x, y) ) {
				return false;
			}
			for (var i = 0; i < args.length; i++) {
				if ( !jsnums.greaterThanOrEqual(y, args[i]) ) {
					return false;
				}
				y = args[i];
			}
			return true;
		  });
*/




PRIMITIVES['abs'] =
    new Primitive('abs',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'abs', 'real', 1);
			return jsnums.abs(x);
		  });

/*
PRIMITIVES['quotient'] =
    new Primitive('quotient',
		  2,
		  false, false,
		  function(x, y) {
		  	sys.print("typeof x and y: " + typeof x + " " + typeof y + "\n");
		  	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.quotient(x, y);
		  });


PRIMITIVES['remainder'] =
    new Primitive('remainder',
		  2,
		  false, false,
		  function(x, y) {
		  	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.remainder(x, y);
		  });
*/

PRIMITIVES['modulo'] =
    new Primitive('modulo',
		  2,
		  false, false,
		  function(x, y) {
		  	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.modulo(x, y);
		  });


PRIMITIVES['max'] =
    new Primitive('max',
		  1,
		  true, false,
		  function(x, args) {
		  	check(x, isReal, 'max', 'real', 1);
			arrayEach(args, function(y, i) {check(y, isReal, 'max', 'real', i+2);});

			var curMax = x;
			for (var i = 0; i < args.length; i++) {
				if ( jsnums.greaterThan(args[i], curMax) ) {
					curMax = args[i];
				}
			}
			return curMax;
		  });


PRIMITIVES['min'] =
    new Primitive('min',
		  1,
		  true, false,
		  function(x, args) {
		  	check(x, isReal, 'min', 'real', 1);
			arrayEach(args, function(y, i) {check(y, isReal, 'min', 'real', i+2);});

			var curMin = x;
			for (var i = 0; i < args.length; i++) {
				if ( jsnums.lessThan(args[i], curMin) ) {
					curMin = args[i];
				}
			}
			return curMin;
		  });


PRIMITIVES['gcd'] =
    new Primitive('gcd',
		  1,
		  true, false,
		  function(x, args) {
		  	args.unshift(x);
		 	arrayEach(args, function(y, i) {check(y, isInteger, 'gcd', 'integer', i+1);});

		 	return jsnums.gcd.apply(null, args);
		  });

PRIMITIVES['lcm'] =
    new Primitive('lcm',
		  1,
		  true, false,
		  function(x, args) {
		  	args.unshift(x);
		 	arrayEach(args, function(y, i) {check(y, isInteger, 'lcm', 'integer', i+1);});

		 	return jsnums.gcd.apply(null, args);
		  });


PRIMITIVES['floor'] =
    new Primitive('floor',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'floor', 'real', 1);
			return jsnums.floor(x);
		  });


PRIMITIVES['ceiling'] =
    new Primitive('ceiling',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'ceiling', 'real', 1);
			return jsnums.ceiling(x);
		  });


PRIMITIVES['round'] =
    new Primitive('round',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'round', 'real', 1);
			return jsnums.round(x);
		  });


PRIMITIVES['numerator'] =
    new Primitive('numerator',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isRational, 'numerator', 'rational', 1);
			return jsnums.numerator(x);
		  });


PRIMITIVES['denominator'] =
    new Primitive('denominator',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isRational, 'denominator', 'rational', 1);
			return jsnums.denominator(x);
		  });


PRIMITIVES['expt'] = 
    new Primitive("expt",
		  2,
		  false, false,
		  function(x, y) {
		  	check(x, isNumber, 'expt', 'number', 1);
			check(y, isNumber, 'expt', 'number', 2);
		  	return jsnums.expt(x, y);
		  });


PRIMITIVES['exp'] =
    new Primitive('exp',
		  1,
		  false, false,
		  function(x, y) {
		  	check(x, isNumber, 'exp', 'number', 1);
			return jsnums.exp(x);
		  });


PRIMITIVES['log'] =
    new Primitive('log',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'log', 'number', 1);
			return jsnums.log(x);
		  });


PRIMITIVES['sin'] =
    new Primitive('sin',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'sin', 'number', 1);
			return jsnums.sin(x);
		  });


PRIMITIVES['cos'] =
    new Primitive('cos',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'cos', 'number', 1);
			return jsnums.cos(x);
		  });


PRIMITIVES['tan'] =
    new Primitive('tan',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'tan', 'number', 1);
			return jsnums.tan(x);
		  });


PRIMITIVES['asin'] =
    new Primitive('asin',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'asin', 'number', 1);
			return jsnums.asin(x);
		  });


PRIMITIVES['acos'] =
    new Primitive('acos',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'acos', 'number', 1);
			return jsnums.acos(x);
		  });


PRIMITIVES['atan'] =
    new Primitive('atan',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'atan', 'number', 1);
			return jsnums.atan(x);
		  });


PRIMITIVES['sqrt'] =
    new Primitive('sqrt',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'sqrt', 'number', 1);
			return jsnums.sqrt(x);
		  });


PRIMITIVES['integer-sqrt'] =
    new Primitive('integer-sqrt',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isInteger, 'integer-sqrt', 'integer', 1);
			return jsnums.integerSqrt(x);
		  });


PRIMITIVES['make-rectangular'] =
    new Primitive('make-rectangular',
		  2,
		  false, false,
		  function(x, y) {
		  	check(x, isReal, 'make-rectangular', 'real', 1);
			check(y, isReal, 'make-rectangular', 'real', 2);
			return jsnums.makeRectangular(x, y);
		  });

PRIMITIVES['make-polar'] =
    new Primitive('make-polar',
		  2,
		  false, false,
		  function(x, y) {
		  	check(x, isReal, 'make-polar', 'real', 1);
			check(x, isReal, 'make-polar', 'real', 2);
			return jsnums.makePolar(x, y);
		  });


PRIMITIVES['real-part'] =
    new Primitive('real-part',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'real-part', 'number', 1);
			return jsnums.realPart(x);
		  });


PRIMITIVES['imag-part'] =
    new Primitive('imag-part',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'imag-part', 'number', 1);
			return jsnums.imaginaryPart(x);
		  });


PRIMITIVES['angle'] =
    new Primitive('angle',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'angle', 'number', 1);
			return jsnums.angle(x);
		  });


PRIMITIVES['magnitude'] =
    new Primitive('magnitude',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'magnitude', 'number', 1);
			return jsnums.magnitude(x);
		  });


PRIMITIVES['inexact->exact'] =
    new Primitive('inexact->exact',
		  1,
		  false, false,
		  function (x) {
		  	check(x, isNumber, 'inexact->exact', 'number', 1);
			return jsnums.toExact(x);
		  });


PRIMITIVES['number->string'] =
    new Primitive('number->string',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isNumber, 'number->string', 'number', 1);
			return x.toString();
		  });


PRIMITIVES['string->number'] =
    new Primitive('string->number',
		  1,
		  false, false,
		  function(str) {
		  	check(str, isString, 'string->number', 'string', 1);
			return jsnums.fromString(str);
		  });



/******************
 *** Predicates ***
 ******************/

PRIMITIVES['pair?'] = new Primitive('pair?', 1, false, false, isPair);
PRIMITIVES['cons?'] = new Primitive('cons?', 1, false, false, isPair);
PRIMITIVES['empty?'] = new Primitive('empty?', 1, false, false, isEmpty);

PRIMITIVES['symbol?'] = new Primitive('symbol?', 1, false, false, isSymbol);
PRIMITIVES['string?'] = new Primitive('string?', 1, false, false, isString);
PRIMITIVES['char?'] = new Primitive('char?', 1, false, false, isChar);
PRIMITIVES['boolean?'] = new Primitive('boolean?', 1, false, false, isBoolean);

PRIMITIVES['number?'] = new Primitive('number?', 1, false, false, isNumber);
PRIMITIVES['complex?'] = new Primitive('complex?', 1, false, false, isComplex);
PRIMITIVES['real?'] = new Primitive('real?', 1, false, false, isReal);
PRIMITIVES['rational?'] = new Primitive('rational?', 1, false, false, isRational);
PRIMITIVES['integer?'] = new Primitive('integer?', 1, false, false, isInteger);

PRIMITIVES['exact?'] =
    new Primitive('exact?', 1, false, false,
		  function(x) {
		  	check(x, isNumber, 'exact?', 'number', 1);
			return jsnums.isExact(x);
		  });
PRIMITIVES['inexact?'] =
    new Primitive('inexact?', 1, false, false,
		  function(x) {
		  	check(x, isNumber, 'inexact?', 'number', 1);
			return !jsnums.isExact(x);
		  });

PRIMITIVES['odd?'] =
    new Primitive('odd?',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isInteger, 'odd?', 'integer', 1);
			return jsnums.modulo(x, 2).equals(1);
		  });
PRIMITIVES['even?'] =
    new Primitive('even?',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isInteger, 'even?', 'integer', 1);
			return jsnums.modulo(x, 2).equals(0);
		  });

PRIMITIVES['zero?'] =
    new Primitive("zero?",
		  1,
		  false, false,
		  function(x) {
		      return jsnums.equals(0, x)
		  });

PRIMITIVES['positive?'] =
    new Primitive('positive?',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'positive?', 'real', 1);
			return jsnums.greaterThan(x, 0);
		  });
PRIMITIVES['negative?'] =
    new Primitive('negative?',
		  1,
		  false, false,
		  function(x) {
		  	check(x, isReal, 'negative?', 'real', 1);
			return jsnums.lessThan(x, 0);
		  });

PRIMITIVES['box?'] = new Primitive('box?', 1, false, false, isBox);

PRIMITIVES['hash?'] = new Primitive('hash?', 1, false, false, isHash);


/***********************
 *** List Primitives ***
 ***********************/

PRIMITIVES['cons'] =
    new Primitive('cons',
		  2,
		  false, false,
		  function(f, r) {
		  	checkList(r, "cons", 2);
		  	return types.cons(f, r);
		  });


var carFirst = function(name) {
	return function (lst) {
		checkList(lst, name, 1);
		return lst.first();
	};
}
PRIMITIVES['first'] = new Primitive('first', 1, false, false, carFirst('first'));
PRIMITIVES['car'] = new Primitive('car', 1, false, false, carFirst('car'));


var cdrRest = function(name) {
	return function (lst) {
		checkList(lst, name, 1);
		return lst.rest();
	};
}
PRIMITIVES['rest'] = new Primitive('rest', 1, false, false, cdrRest('rest'));
PRIMITIVES['cdr'] = new Primitive('cdr', 1, false, false, cdrRest('cdr'));


PRIMITIVES['caar'] =
    new Primitive('caar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'caar', 1);
		  	lst.first().first();
		  });

var cadrSecond = function(name) {
	return function(lst) {
		checkList(lst, 'cadr', 1);
		lst.rest().first();
	};
}
PRIMITIVES['cadr'] = new Primitive('cadr', 1, false, false, cadrSecond('cadr'));
PRIMITIVES['second'] = new Primitive('second', 1, false, false, cadrSecond('second'));

PRIMITIVES['cdar'] =
    new Primitive('cdar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cdar', 1);
		  	lst.first().rest();
		  });

PRIMITIVES['cddr'] =
    new Primitive('cddr',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cddr', 1);
		  	lst.rest().rest();
		  });

PRIMITIVES['caaar'] =
    new Primitive('caaar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'caaar', 1);
		  	lst.first().first().first();
		  });

PRIMITIVES['caadr'] =
    new Primitive('caadr',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'caadr', 1);
		  	lst.rest().first().first();
		  });

PRIMITIVES['cadar'] =
    new Primitive('cadar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cadar', 1);
		  	lst.first().rest().first();
		  });

PRIMITIVES['cdaar'] =
    new Primitive('cdaar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cdaar', 1);
		  	lst.first().first().rest();
		  });

PRIMITIVES['cdadr'] =
    new Primitive('cdadr',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cdadr', 1);
		  	lst.rest().first().rest();
		  });

PRIMITIVES['cddar'] =
    new Primitive('cddar',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cddar', 1);
		  	lst.first().rest().rest();
		  });

var caddrThird = function(name) {
	return function(lst) {
		checkList(lst, name, 1);
		lst.rest().rest().first();
	};
}
PRIMITIVES['caddr'] = new Primitive('caddr', 1, false, false, caddrThird('caddr'));
PRIMITIVES['third'] = new Primitive('third', 1, false, false, caddrThird('third'));

PRIMITIVES['cdddr'] =
    new Primitive('cdddr',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'cdddr', 1);
		  	lst.rest().rest().rest();
		  });

var cadddrFourth = function(name) {
	return function (lst) {
		checkList(lst, name, 1);
		lst.rest().rest().rest().first();
	};
}
PRIMITIVES['cadddr'] = new Primitive('cadddr', 1, false, false, cadddrFourth('cadddr'));
PRIMITIVES['fourth'] = new Primitive('fourth', 1, false, false, cadddrFourth('fourth'));

PRIMITIVES['fifth'] =
    new Primitive('fifth',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'fifth', 1);
		  	lst.rest().rest().rest().rest().first();
		  });

PRIMITIVES['sixth'] =
    new Primitive('sixth',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'sixth', 1);
		  	lst.rest().rest().rest().rest().rest().first();
		  });

PRIMITIVES['seventh'] =
    new Primitive(
		  'seventh',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'seventh', 1);
		  	lst.rest().rest().rest().rest().rest().rest().first();
		  });

PRIMITIVES['eighth'] =
    new Primitive('eighth',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'eighth', 1);
		  	lst.rest().rest().rest().rest().rest().rest().rest().first();
		  });


PRIMITIVES['length'] =
    new Primitive('length',
		  1,
		  false, false,
		  function(lst) {
		   	return jsnums.makeRational(length(lst));
		  });


PRIMITIVES['list?'] = new Primitive('list?', 1, false, false, isList);


PRIMITIVES['list'] =
    new Primitive('list',
		  0,
		  true, false,
		  types.list);


PRIMITIVES['list*'] =
    new Primitive('list*',
		  1,
		  true, false,
		  function(items, otherItems) {
		  	if (otherItems.length == 0) {
				return items;
			}
		  
		  	var lastListItem = otherItems.pop();
		  	checkList(lastListItem, 'list*', otherItems.length+2);

		  	otherItems.unshift(items);
		  	return append([types.list(otherItems), lastListItem]);
		  });


PRIMITIVES['list-ref'] =
    new Primitive('list-ref',
		  2,
		  false, false,
		  function(lst, x) {
		  	checkList(lst, 'list-ref', 1);
		  	check(x, isNatural, 'list-ref', 'natural', 2);
		  	var i = 0;
		  	var len = 0;
		  	for (; jsnums.lessThan(i, x); i++) {
		  		if (lst.isEmpty()) {
		  			// TODO Throw some sort of error here
		  		}
		  		else {
		  			len++;
	  				lst = lst.rest();
		  		}
		  	}
		  	return lst.first();
		  });


PRIMITIVES['append'] =
    new Primitive('append',
		  0,
		  true, false,
		  append);


PRIMITIVES['reverse'] =
    new Primitive('reverse',
		  1,
		  false, false,
		  function(lst) {
		  	checkList(lst, 'reverse', 1);
		  	return lst.reverse();
		  });


PRIMITIVES['map'] =
    new Primitive('map',
		  2,
		  true, false,
		  function(f, lst, arglists) {
		  	arglists.unshift(lst);
		  	check(f, isFunction, 'map', 'function', 1);
		  	arrayEach(arglists, function(x, i) {checkList(x, 'map', i+2);});
			checkAllSameLength(arglists, 'map');
			
			var mapHelp = function(f, args, acc) {
				if (args[0].isEmpty()) {
				    return acc.reverse();
				}
				
				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length; i++) {
					argsFirst.push(args[i].first());
					argsRest.push(args[i].rest());
				}
				//sys.print(sys.inspect(argsFirst));
				//sys.print(sys.inspect(argsRest));
				var result = CALL(f, argsFirst,
					function(result) {
						return mapHelp(f, argsRest, types.cons(result, acc));
					});
				return result;
			}
			return mapHelp(f, arglists, types.EMPTY);
		});


PRIMITIVES['andmap'] =
    new Primitive('andmap',
		  2,
		  true, false,
		  function(f, lst, arglists) {
		  	arglists.unshift(lst);
		   	check(f, isFunction, 'andmap', 'function', 1);
		   	arrayEach(arglists, function(x, i) {checkList(x, 'andmap', i+2);});
			checkAllSameLength(arglists, 'andmap');
  
			var andmapHelp = function(f, args) {
				if ( args[0].isEmpty() ) {
					return true;
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length; i++) {
					argsFirst.push(args[i].first());
					argsRest.push(args[i].rest());
				}

				return CALL(f, argsFirst,
					function(result) {
						return result && andmapHelp(f, argsRest);
					});
			}
			return andmapHelp(f, arglists);
		  });


PRIMITIVES['ormap'] =
    new Primitive('ormap',
		  2,
		  true, false,
		  function(f, lst, arglists) {
		  	arglists.unshift(lst);
		   	check(f, isFunction, 'ormap', 'function', 1);
		   	arrayEach(arglists, function(x, i) {checkList(x, 'ormap', i+2);});
			checkAllSameLength(arglists, 'ormap');

			var ormapHelp = function(f, args) {
				if ( args[0].isEmpty() ) {
					return false;
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length; i++) {
					argsFirst.push(args[i].first());
					argsRest.push(args[i].rest());
				}

				return CALL(f, argsFirst,
					function(result) {
						return result || ormapHelp(f, argsRest);
					});
			}
			return ormapHelp(f, arglists);
		});


PRIMITIVES['member'] =
    new Primitive('member',
		  2,
		  false, false,
		  function(item, lst) {
		  	checkList(lst, 'member', 2);
		  	var aUnionFind = new types.UnionFind();
		  	while ( !lst.isEmpty() ) {
		  		if ( types.isEqual(item, lst.first(), aUnionFind).valueOf() ) {
		  			return true;
		  		}
		  		lst = lst.rest();
		  	}
		  	return false;
		  });

PRIMITIVES['remove'] =
    new Primitive('remove',
		  2,
		  false, false,
		  function(item, lst) {
		  	checkList(lst, 'remove', 2);
		  	var originalLst = lst;
		  	var aUnionFind = new types.UnionFind();
		  	var result = types.EMPTY;
		  	while ( !lst.isEmpty() ) {
		  		if (types.isEqual(item, lst.first(), aUnionFind).valueOf()) {
		  			return append([result.reverse(), lst.rest()]);
		  		} else {
		  			result = types.cons(lst.first(), result);
		  			lst = lst.rest();
		  		}
		  	}
		  	return originalLst;
		  });


PRIMITIVES['filter'] =
    new Primitive('filter',
		  2,
		  false, false,
		  function(f, lst) {
		  	check(f, isFunction, 'filter', 'function', 1);
			checkList(lst, 'filter', 2);

			var filterHelp = function(f, lst, acc) {
				if ( lst.isEmpty() ) {
					return acc.reverse();
				}

				return CALL(f, [lst.first()],
					function(result) {
						if (result) {
							return filterHelp(f, lst.rest(),
								types.cons(lst.first(), acc));
						}
						else {
							return filterHelp(f, lst.rest(), acc);
						}
					});
			}
			return filterHelp(f, lst, types.EMPTY);
		});


PRIMITIVES['foldl'] =
    new Primitive('foldl',
		  3,
		  true, false,
		  function(f, initAcc, lst, arglists) {
		  	arglists.unshift(lst);
		  	check(f, isFunction, 'foldl', 'function', 1);
			arrayEach(arglists, function(x, i) {checkList(x, 'foldl', i+3);});
			checkAllSameLength(arglists, 'foldl');
	
			return foldHelp(f, initAcc, arglists);
		});

PRIMITIVES['foldr'] =
    new Primitive('foldr',
		  3,
		  true, false,
		  function(f, initAcc, lst, arglists) {
		  	arglists.unshift(lst);
		  	check(f, isFunction, 'foldr', 'function', 1);
			arrayEach(arglists, function(x, i) {checkList(x, 'foldr', i+3);});
			checkAllSameLength(arglists, 'foldr');

			for (var i = 0; i < arglists.length; i++) {
				arglists[i] = arglists[i].reverse();
			}
			
			return foldHelp(f, initAcc, arglists);
		});


PRIMITIVES['build-list'] =
    new Primitive('build-list',
		  2,
		  false, false,
		  function(num, f) {
		  	check(num, isNatural, 'build-list', 'natural', 1);
			check(f, isFunction, 'build-list', 'function', 2);

			var buildListHelp = function(n, acc) {
				if ( jsnums.greaterThanOrEqual(n, num) ) {
					return acc.reverse();
				}

				return CALL(f, n,
					function (result) {
						return buildListHelp(n+1, types.cons(result, acc));
					});
			}
			return buildListHelp(0, types.EMPTY);
		  });


/**********************
 *** Box Primitives ***
 **********************/


PRIMITIVES['box'] = new Primitive('box', 1, false, false, types.Box);

PRIMITIVES['unbox'] =
    new Primitive('unbox',
		  1,
		  false, false,
		  function(box) {
		  	check(box, isBox, 'unbox', 'box', 1);
			return box.unbox();
		  });


PRIMITIVES['set-box!'] =
    new Primitive('set-box!',
		  2,
		  false, false,
		  function(box, newVal) {
		  	check(box, isBox, 'set-box!', 'box', 1);
			box.set(newVal);
			return types.VOID;
		  });

/****************************
 *** Hashtable Primitives ***
 ****************************/


PRIMITIVES['make-hash'] = new Primitive('make-hash', 0, false, false, types.EqualHashTable);

PRIMITIVES['make-hasheq'] = new Primitive('make-hasheq', 0, false, false, types.EqHashTable);

PRIMITIVES['hash-set!'] =
    new Primitive('hash-set!',
		  3,
		  false, false,
		  function(obj, key, val) {
		  	check(obj, isHash, 'hash-set!', 'hash', 1);
			obj.hash.put(key, val);
			return types.VOID;
		  });

PRIMITIVES['hash-ref'] =
    new Primitive('hash-ref',
		  3,
		  false, false,
		  function(obj, key, defaultVal) {
		  	check(obj, isHash, 'hash-ref', 'hash', 1);

			if (obj.hash.containsKey(key)) {
				return obj.hash.get(key);
			}
			else {
				if (isFunction(defaultVal)) {
					return defaultVal();
				}
				return defaultVal;
			}
		  });

PRIMITIVES['hash-remove!'] =
    new Primitive('hash-remove',
		  2,
		  false, false,
		  function(obj, key) {
		  	check(obj, isHash, 'hash-remove!', 'hash', 1);
			obj.hash.remove(key);
			return types.VOID;
		  });

PRIMITIVES['hash-map'] =
    new Primitive('hash-map',
		  2,
		  false, false,
		  function(ht, f) {
		  	check(ht, isHash, 'hash-map', 'hash', 1);
			check(f, isFunction, 'hash-map', 'function', 2);
			
			var keys = ht.hash.keys();
			var hashMapHelp = function(i, acc) {
				if (i >= keys.length) {
					return acc;
				}

				var val = ht.hash.get(keys[i]);
				return CALL(f, [keys[i], val],
					function(result) {
						hashMapHelp(i+1, types.cons(result, acc));
					});
			}
			return hashMapHelp(0, types.EMPTY);
		  });


PRIMITIVES['hash-for-each'] =
    new Primitive('hash-for-each',
		  2,
		  false, false,
		  function(ht, f) {
		  	check(ht, isHash, 'hash-for-each', 'hash', 1);
			check(f, isFunction, 'hash-for-each', 'function', 2);
		  	
		  	var keys = ht.hash.keys();
		  	var hashForEachHelp = function(i) {
		  		if (i >= keys.length) {
					return types.VOID;
				}

				var val = ht.hash.get(keys[i]);
				return CALL(f, [keys[i], val],
					function(result) {
						hashForEachHelp(i+1);
					});
			}
			return hashForEachHelp(0);
		  });



/*************************
 *** String Primitives ***
 *************************/


PRIMITIVES['make-string'] =
    new Primitive('make-string',
		  2,
		  false, false,
		  function(n, c) {
		  	check(n, isNatural, 'make-string', 'natural', 1);
			check(c, isChar, 'make-string', 'char', 2);

			var val = "";
			for (var i = 0; jsnums.lessThan(i, n); i++) {
				val += c;
			}
			return types.string(val);
		  });


PRIMITIVES['string'] =
    new Primitive('string',
		  1,
		  true, false,
		  function(firstChar, chars) {
		  	chars.unshift(firstChar);
			arrayEach(chars, function(c, i) {check(c, isChar, 'string', 'char', i+1);});

			var ret = "";
			for (var i = 0; i < chars.length; i++) {
				ret += chars[i].val;
			}
			return types.string(ret);
		  });


PRIMITIVES['string-length'] =
    new Primitive('string-length', 1, false, false,
		  function(str) {
		  	check(str, isString, 'string-length', 'string', 1);
			return str.length;
		  });


PRIMITIVES['string-ref'] =
    new Primitive('string-ref',
		  2,
		  false, false,
		  function(str, n) {
		  	check(str, isString, 'string-ref', 'string', 1);
			check(n, isNatural, 'string-ref', 'natural', 2);
			if (jsnums.greaterThanOrEqual(n, str.length)) {
				// TODO Throw some sort of error
			}
			return types.char(str.charAt(jsnums.toFixnum(n)));
		  });


PRIMITIVES['string=?'] =
    new Primitive('string=?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
		  	strs.unshift(str1);
		 	arrayEach(strs, function(str, i) {check(str, isString, 'string=?', 'string', i+1);});
		  	
			var str = str1;
			for (var i = 0; i < strs.length; i++) {
				if (str !== strs[i]) {
					return false;
				}
				str = strs[i];
			}
			return true;
		  });


PRIMITIVES['string-ci=?'] =
    new Primitive('string-ci=?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);

			for(var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA === strB;});
		  });


PRIMITIVES['string<?'] =
    new Primitive('string<?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string<?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA < strB;});
		  });


PRIMITIVES['string>?'] =
    new Primitive('string>?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string>?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA > strB;});
		  });


PRIMITIVES['string<=?'] =
    new Primitive('string<=?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string<=?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA <= strB;});
		  });


PRIMITIVES['string>=?'] =
    new Primitive('string>=?',
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string>=?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA >= strB;});
		  });


PRIMITIVES['string-ci<?'] =
    new Primitive('string-ci<?'
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci<?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA < strB;});
		  });


PRIMITIVES['string-ci>?'] =
    new Primitive('string-ci>?'
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci>?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA > strB;});
		  });


PRIMITIVES['string-ci<=?'] =
    new Primitive('string-ci<=?'
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci<=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA <= strB;});
		  });


PRIMITIVES['string-ci>=?'] =
    new Primitive('string-ci>=?'
		  2,
		  true, false,
		  function(str1, str2, strs) {
		  	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci>=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA >= strB;});
		  });


PRIMITIVES['substring'] =
    new CasePrimitive(
	[new Primitive('substring',
		       2,
		       false, false,
		       function(str, theStart) {
		           check(str, isString, 'substring', 'string', 1);
			   check(theStart, isNatural, 'substring', 'natural', 2);
			  
			   var start = jsnums.toFixnum(theStart);
			   if (start > str.length) {
			  	// TODO Throw some sort of error here!
			   }
			   else {
			  	return str.substring(jsnums.toFixnum(start));
			   }
		       }),
	 new Primitive('substring',
		       3,
		       false, false,
		       function(str, theStart, theEnd) {
		           check(str, isString, 'substring', 'string', 1);
			   check(theStart, isNatural, 'substring', 'natural', 2);
			   check(theEnd, isNatural, 'substring', 'natural', 3);

			   var start = jsnums.toFixnum(theStart);
			   var end = jsnums.toFixnum(theEnd);
			   if (start > str.length || end > str.length || end <= start) {
			  	// TODO Throw some sort of error here!
			   }
			   else {
			  	return str.substring(start, end);
			   }
		       }) ]);


PRIMITIVES['string->list'] =
    new Primitive('string->list',
		  1,
		  false, false,
		  function(str) {
		  	check(str, isString, 'string->list', 'string', 1);

			var lst = types.EMPTY;
			for (var i = 0; i < str.length; i++) {
				types.cons(types.char(str.charAt(i)), lst);
			}
			return lst.reverse();
		  });


PRIMITIVES['list->string'] =
    new Primitive('list->string',
		  1,
		  false, false,
		  function(lst) {
		  	checkListOf(lst, isChar, 'list->string', 'char', 1);

			var ret = "";
			while( !lst.isEmpty() ) {
				ret += lst.first().val;
				lst = lst.rest();
			}
			return types.string(ret);
		  });


PRIMITIVES['string-copy'] =
    new Primitive('string-copy',
		  1,
		  false, false,
		  function(str) {
		  	check(str, isString, 'string-copy', 'string', 1);
			return types.string(str);
		  });



/***********************
 *** Char Primitives ***
 ***********************/


PRIMITIVES['char=?'] =
    new Primitive('char=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val === c2.val;});
		  });


PRIMITIVES['char<?'] =
    new Primitive('char<?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char<?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val < c2.val;});
		  });


PRIMITIVES['char>?'] =
    new Primitive('char>?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char>?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val > c2.val;});
		  });


PRIMITIVES['char<=?'] =
    new Primitive('char<=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char<=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val <= c2.val;});
		  });


PRIMITIVES['char>=?'] =
    new Primitive('char>=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char>=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val >= c2.val;});
		  });


PRIMITIVES['char-ci=?'] =
    new Primitive('char-ci=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci=?', 'char', i+1);});

			return compare(chars,
				function(c1, c2) {
					return c1.val.toLowerCase() === c2.val.toLowerCase();
				});
		  });


PRIMITIVES['char-ci<?'] =
    new Primitive('char-ci<?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci<?', 'char', i+1);});

			return compare(chars,
				function(c1, c2) {
					return c1.val.toLowerCase() < c2.val.toLowerCase();
				});
		  });


PRIMITIVES['char-ci>?'] =
    new Primitive('char-ci>?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci>?', 'char', i+1);});

			return compare(chars,
				function(c1, c2) {
					return c1.val.toLowerCase() > c2.val.toLowerCase();
				});
		  });


PRIMITIVES['char-ci<=?'] =
    new Primitive('char-ci<=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci<=?', 'char', i+1);});

			return compare(chars,
				function(c1, c2) {
					return c1.val.toLowerCase() <= c2.val.toLowerCase();
				});
		  });


PRIMITIVES['char-ci>=?'] =
    new Primitive('char-ci>=?',
		  2,
		  true, false,
		  function(char1, char2, chars) {
		  	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char-ci>=?', 'char', i+1);});

			return compare(chars,
				function(c1, c2) {
					return c1.val.toLowerCase() >= c2.val.toLowerCase();
				});
		  });


PRIMITIVES['char-alphabetic?'] =
    new Primitive('char-alphabetic?',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-alphabetic?', 'char', 1);
			return isAlphabeticString(c.val);
		  });


PRIMITIVES['char-numeric?'] =
    new Primitive('char-numeric?',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-numeric?', 'char', 1);
			return (c.val >= '0' && c.val <= '9');
		  });


PRIMITIVES['char-whitespace?'] =
    new Primitive('char-whitespace?',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-whitespace?', 'char', 1);
			return isWhitespaceString(c.val);
		  });


PRIMITIVES['char-upper-case?'] =
    new Primitive('char-upper-case?',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-upper-case?', 'char', 1);
			return (isAlphabeticString(c.val) && c.val.toUpperCase() === c.val);
		  });


PRIMITIVES['char-lower-case?'] =
    new Primitive('char-lower-case?',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-lower-case?', 'char', 1);
			return (isAlphabeticString(c.val) && c.val.toLowerCase() === c.val);
		  });


PRIMITIVES['char->integer'] =
    new Primitive('char->integer',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char->integer', 'char', 1);
			return c.val.charCodeAt(0);
		  });


PRIMITIVES['integer->char'] =
    new Primitive('integer->char',
		  1,
		  false, false,
		  function(num) {
		  	check(num, function(x) {
					if ( !isInteger(x) ) {
						return false;
					}
					var n = jsnums.toFixnum(x);
					return ((n >= 0 && n < 55296) ||
						(n > 57343 && n <= 1114111));
				},
				'integer->char',
				'exact integer in [0,#x10FFFF], not in [#xD800,#xDFFF]',
				1);

			return types.char( String.fromCharCode(jsnums.toFixnum(num)) );
		  });


PRIMITIVES['char-upcase'] =
    new Primitive('char-upcase',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-upcase', 'char', 1);
			return types.char( c.val.toUpperCase() );
		  });


PRIMITIVES['char-downcase'] =
    new Primitive('char-downcase',
		  1,
		  false, false,
		  function(c) {
		  	check(c, isChar, 'char-downcase', 'char', 1);
			return types.char( c.val.toLowerCase() );
		  });





/////////////////////////////////////////////////////////////////////////////////////////////

primitive.getPrimitive = function(name) {
    return PRIMITIVES[name];
};

primitive.isPrimitive = function(x) {
    return x instanceof Primitive;
};

primitive.addPrimitive = function(name, aPrim) {
    PRIMITIVES[name] = aPrim;
};

primitive.Primitive = Primitive;
primitive.CasePrimitive = CasePrimitive;


primitive.setCALL = setCALL;

})();

