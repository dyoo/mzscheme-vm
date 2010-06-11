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

var PrimProc = function(name, numParams, isRest, usesState, impl) {
    this.name = name;
    this.numParams = numParams;
    this.isRest = isRest;
    this.usesState = usesState;
    this.impl = impl;
};


var CasePrimitive = function(cases) {
    this.cases = cases;
};

var PrimConst = function(name, val) {
	this.name = name;
	this.val = val;
}


var UNDEF = types.UNDEFINED;

//////////////////////////////////////////////////////////////////////

// Helper Functions

var id = function(x) { return x; };

var callWithValues = function(f, vals) {
	if (vals instanceof types.ValuesWrapper) {
		return CALL(f, vals.elts, id);
	}
	else {
		return CALL(f, [vals], id);
	}
};

var procedureArity = function(proc) {
	check(proc, isFunction, 'procedure-arity', 'function', 1);
			
	var singleCaseArity = function(aCase) {
		if (aCase instanceof types.ContinuationClosureValue) {
			return 1;
		}
		else if (aCase.isRest) {
			return types.arityAtLeast(aCase.numParams);
		}
		else {
			return aCase.numParams;
		}
	}
	
	if ( proc instanceof PrimProc ||
	     proc instanceof types.ClosureValue ||
	     proc instanceof types.ContinuationClosureValue ) {
		return singleCaseArity(proc);
	}
	else if ( proc instanceof CasePrimitive ) {
		var ret = types.EMPTY;
		for (var i = 0; i < proc.cases.length; i++) {
			ret = types.cons(singleCaseArity(proc.cases[i]), ret);
		}
		return ret.reverse();
	}
	else if ( proc instanceof types.CaseLambdaValue ) {
		var ret = types.EMPTY;
		for (var i = 0; i < proc.closures.length; i++) {
			ret = types.cons( singleCaseArity(proc.closures[i]), ret );
		}
		return ret.reverse();
	}
	else {
		throw new Error('procedure-arity given wrong type that passed isFunction!');
	}
};

var funArityContains = function(n) {
	return function(proc) {
		if ( !isFunction(proc) ) {
			return false;
		}

		var arity = procedureArity(proc);
		if ( !isList(arity) ) {
			arity = types.list([arity]);
		}

		while ( !arity.isEmpty() ) {
			if ( isNumber(arity.first()) ) {
				return n === arity.first();
			}
			else if ( types.isArityAtLeast(arity.first()) ) {
				return arity.first().val <= n;
			}
			arity = arity.rest();
		}
		throw new Error('something went wrong in funArityContains');
	}
}

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
	var ret = args[args.length-1];
	for (i = args.length-2; i >= 0; i--) {
		ret = args[i].append(ret);
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
			return foldHelp(f, result, argsRest);
		});
}

var quicksort = function(functionName) {
	return function(initList, comp) {
		checkList(initList, functionName, 1);
		check(comp, funArityContains(2), functionName, 'function', 2);
	
		var quicksortHelp = function(lst) {
			if ( lst.isEmpty() ) {
				return types.EMPTY;
			}
	
			var compYes = new PrimProc('compYes', 1, false, false,
					function(x) { return CALL(comp, [x, lst.first()], id); });
			var compNo = new PrimProc('compNo', 1, false, false,
					function(x) { return CALL(comp, [x, lst.first()],
								  function(res) { return !res; });
					});
			var recCallProc = new PrimProc('quicksort', 1, false, false, quicksortHelp);
			return CALL(PRIMITIVES['filter'], [compYes, lst.rest()],
				function(half1) {
					return CALL(recCallProc, [half1],
						function(sorted1) {
							return CALL(PRIMITIVES['filter'], [compNo, lst.rest()],
								function(half2) {
									return CALL(recCallProc, [half2],
										function(sorted2) {
											return append([sorted1,
												       types.list([lst.first()]),
												       sorted2]);
										});
								});
						});
				});
		}
		return quicksortHelp(initList);
	};
}

var schemeListToArray = function(lst) {
	var result = [];
	while ( !lst.isEmpty() ) {
		result.push(lst.first());
		lst = lst.rest();
	}
	return result;
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
		if (! ((s.charAt(i) >= "a" && s.charAt(i) <= "z") ||
		       (s.charAt(i) >= "A" && s.charAt(i) <= "Z"))) {
			return false;
		}
	}
	return true;
}

var isNumericString = function(s) {
	for (var i = 0; i < s.length; i++) {
		if ( ! (s.charAt(i) >= '0' && s.charAt(i) <= '9') ) {
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


var format = function(functionName) {
	return function(formatStr, args) {
		check(formatStr, isString, functionName, 'string', 1);
		var pattern = new RegExp("~[sSaAn%~]", "g");
		var buffer = args;
		function f(s) {
			if (s == "~~") {
				return "~";
			} else if (s == '~n' || s == '~%') {
				return "\n";
			} else if (s == '~s' || s == "~S") {
				if (buffer.length == 0) {
					//TODO Throw some sort of error here!!
				}
				return types.toWrittenString(buffer.shift());
			} else if (s == '~a' || s == "~A") {
				if (buffer.length == 0) {
					//TODO Throw some sort of error here!!
				}
				return types.toDisplayedString(buffer.shift());
			} else {
				//TODO Throw some sort of error here!!
			}
		}
		var result = formatStr.replace(pattern, f);
		if (buffer.length > 0) {
			//TODO Throw some sort of error here!!
		}
		return result;
	}
};

var printHook = function(str) {
	// TODO Implement actual printing here!
}



//////////////////////////////////////////////////////////////////////

// Returns true if x is a number.
var isNumber = jsnums.isSchemeNumber;

var isReal = jsnums.isReal;
var isRational = jsnums.isRational;
var isComplex = isNumber;

// Returns true if x is an integer.
var isInteger = function(x) {
	return (isNumber(x) && jsnums.isInteger(x));
}

var isNatural = function(x) {
	return isNumber(x) && jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 0);
}

var isSymbol = types.isSymbol;
var isChar = types.isChar;
var isString = types.isString;
var isPair = types.isPair;
var isEmpty = function(x) { return x === types.EMPTY; };
var isList = function(x) { return (isPair(x) || isEmpty(x)); };
var isVector = types.isVector;
var isBox = types.isBox;
var isHash = types.isHash;

var isBoolean = function(x) {
	return (x === true || x === false);
}

var isFunction = function(x) {
	return (x instanceof PrimProc ||
		x instanceof CasePrimitive ||
		x instanceof types.ClosureValue ||
		x instanceof types.CaseLambdaValue ||
		x instanceof types.ContinuationClosureValue);
}


var sub1 = function(x) {
	check(x, isNumber, 'sub1', 'number', 1);
	return jsnums.subtract(x, 1);
}

var add1 = function(x) {
	check(x, isNumber, 'add1', 'number', 1);
	return jsnums.add(x, 1);
}

var isEqual = function(x, y) {
	return types.isEqual(x, y, new types.UnionFind());
}

var isEq = function(x, y) {
	return x === y;
}

var isEqv = function(x, y) {
	if (isNumber(x) && isNumber(y)) {
		return jsnums.eqv(x, y);
	}
	else if (isChar(x) && isChar(y)) {
		return x.val === y.val;
	}
	return x === y;
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
//		sys.print("Check failed: " + functionName + ' expected argument ' +
//				position + ' of type <' + typeName + '>, got ' + x + '\n');
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
    new PrimProc('print', 
		 1, 
		 false, 
		 false, 
		 function(x) {
		     sys.print(''+ x + '\n');
		     return UNDEF;
		 });


PRIMITIVES['display'] = 
    new CasePrimitive(
	[new PrimProc('display', 1, false, false, function(x) {
	    sys.print('' + x);
	    return UNDEF;
	}),
	 new PrimProc('display', 2, false, false, function(x, port) {
	     // FIXME
	     throw new Error("display to a port not implemented yet.");
	 } )]);



PRIMITIVES['newline'] = 
    new CasePrimitive(
	[new PrimProc('newline', 0, false, false, function() {
	    sys.print('\n');
	    return UNDEF;
	}),
	 new PrimProc('newline', 1, false, false, function(port) {
	     // FIXME
	     throw new Error("newline to a port not implemented yet.");
	 } )]);



PRIMITIVES['current-print'] =
    new PrimProc('current-print', 
		 0, 
		 false, false,
		 function() {
		     return defaultPrint;
		 });


/*
PRIMITIVES['printf'] = 
    new PrimProc('printf',
		 1,
		 true, false,
		 function(fmtString, vals) {
		     // FIXME
		     sys.print("printf not implemented yet.  " + fmtString);
		     sys.print(sys.inspect(vals));
		     return UNDEF;
		 });
*/


PRIMITIVES['for-each'] =
    new PrimProc('for-each', 
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


/*
PRIMITIVES['random'] =
    new PrimProc("random",
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
*/


PRIMITIVES['make-thread-cell'] = 
    new CasePrimitive([
	new PrimProc("make-thread-cell",
		     1, false, false,
		     function(x) {
			  return new types.ThreadCell(x, false);
		     }
		    ),
	new PrimProc("make-thread-cell",
		     2, false, false,
		     function(x, y) {
			  return new types.ThreadCell(x, y);
		     }
		    )]);



PRIMITIVES['make-continuation-prompt-tag'] = 
    new CasePrimitive([
	new PrimProc("make-continuation-prompt-tag",
		     0, false, false,
		     function() {
			  return new types.ThreadCell();
		     }
		    ),
	new PrimProc("make-continuation-prompt-tag",
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
	return new PrimProc(name,
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
		 (new PrimProc('constructor',
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
		 (new PrimProc('predicate',
				 1,
				 false,
				 false,
				 function(x) {
				     return aStructType.predicate(x);
				 })),
		 (new PrimProc('accessor',
				 2,
				 false,
				 false,
				 function(x, i) {
				     return aStructType.accessor(x, jsnums.toFixnum(i));
				 })),
		 (new PrimProc('mutator',
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
		return new PrimProc(fieldName, 
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
		return new PrimProc(fieldName, 
				     2, 
				     false,
				     false,
				     function(x, v) {
					 return mutator.impl(x, fieldPos, v);
				     });
	    });



PRIMITIVES['current-inexact-milliseconds'] =
    new PrimProc(
	'current-inexact-milliseconds',
	0,
	false, false,
	function() {
	    return jsnums.makeFloat((new Date()).valueOf());
	});


PRIMITIVES['procedure-arity'] = new PrimProc('procedure-arity', 1, false, false, procedureArity);


PRIMITIVES['apply'] =
    new PrimProc('apply',
		 2,
		 true, false,
		 function(f, firstArg, args) {
		 	check(f, isFunction, 'apply', 'function', 1);
		 	args.unshift(firstArg);

			var lastArg = args.pop();
			checkList(lastArg, 'apply', args.length+2);
			var args = args.concat(schemeListToArray(lastArg));

			return CALL(f, args, id);
		 });


PRIMITIVES['values'] =
    new PrimProc('values',
		 0,
		 true, false,
		 function(args) {
		 	if (args.length === 1) {
				return args[0];
			}
		 	return new types.ValuesWrapper(args);
		 });


PRIMITIVES['call-with-values'] =
    new PrimProc('call-with-values',
		 2,
		 false, false,
		 function(g, r) {
		 	check(g, funArityContains(0), 'call-with-values', 'function', 1);
			check(r, isFunction, 'call-with-values', 'function', 2);

			return CALL(g, [],
				function(res) {
					return callWithValues(r, res);
				});
		 });


PRIMITIVES['compose'] =
    new PrimProc('compose',
		 0,
		 true, false,
		 function(procs) {
		 	arrayEach(procs, function(p, i) {check(p, isFunction, 'compose', 'function', i+1);});

			if (procs.length == 0) {
				return PRIMITIVES['values'];
			}
			var funList = types.list(procs).reverse();
			
			var composeHelp = function(x, fList) {
				if ( fList.isEmpty() ) {
					return x;
				}

				return CALL(new PrimProc('', 1, false, false,
						         function(args) {
							     return callWithValues(fList.first(), args);
							 }),
					    [x],
					    function(result) {
						return composeHelp(result, fList.rest());
					    });
			}
			return new PrimProc('', 0, true, false,
					    function(args) {
						if (args.length === 1) {
							return composeHelp(args[0], funList);
						}
					        return composeHelp(new types.ValuesWrapper(args), funList);
					    });
		 });


PRIMITIVES['current-seconds'] =
    new PrimProc('current-seconds',
		 0,
		 false, false,
		 function() {
		 	return Math.floor( (new Date()).getTime() / 1000 );
		 });


PRIMITIVES['not'] =
    new PrimProc('not',
		 1,
		 false, false,
		 function(x) {
		 	return x === false;
		 });


PRIMITIVES['void'] =
    new PrimProc('void', 0, true, false,
		 function(args) {
		 	return types.VOID;
		 });


PRIMITIVES['random'] =
    new CasePrimitive(
	[new PrimProc('random', 0, false, false,
		      function() {return jsnums.float(Math.random());}),
	 new PrimProc('random', 1, false, false,
		      function(n) {
			  check(n, isNatural, 'random', 'natural', 1);
			  return Math.floor(Math.random() * jsnums.toFixnum(n));
		      }) ]);


PRIMITIVES['identity'] =
    new PrimProc('identity', 1, false, false, id);



/***********************
 *** Math Primitives ***
 ***********************/


PRIMITIVES['*'] = 
    new PrimProc('*',
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
    new PrimProc("-",
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
    new PrimProc("+",
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
    new PrimProc("=",
		 2,
		 true, false,
		 function(x, y, args) {
		 	args.unshift(y);
		 	args.unshift(x);
		 	arrayEach(args, function(z, i) {check(z, isNumber, '=', 'number', i+1);});

		 	return compare(args, jsnums.equals);
		 });


PRIMITIVES['=~'] =
    new PrimProc('=~',
		 3,
		 false, false,
		 function(x, y, range) {
		 	check(x, isReal, '=~', 'real', 1);
			check(y, isReal, '=~', 'real', 2);
			check(range, function(n) {return isReal(n) && jsnums.greaterThanOrEqual(n, 0);},
			      '=~', 'non-negative real', 3);

			return jsnums.lessThanOrEqual(jsnums.abs(jsnums.subtract(x, y)), range);
		 });


PRIMITIVES['/'] =
    new PrimProc('/',
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
    new PrimProc("sub1",
		 1,
		 false, false,
		 sub1);

PRIMITIVES['add1'] =
    new PrimProc("add1",
		 1,
		 false, false,
		 add1);


PRIMITIVES['<'] = 
    new PrimProc('<',
		 2,
		 true, false,
		 function(x, y, args) {
		 	args.unshift(y);
		 	args.unshift(x);
		 	arrayEach(args, function(z, i) {check(z, isNumber, '<', 'number', i+1);});

		 	return compare(args, jsnums.lessThan);
		 });


PRIMITIVES['>'] =
    new PrimProc('>',
		 2,
		 true, false,
		 function(x, y, args) {
		 	args.unshift(y);
		 	args.unshift(x);
		 	arrayEach(args, function(z, i) {check(z, isNumber, '>', 'number', i+1);});

		 	return compare(args, jsnums.greaterThan);
		 });


PRIMITIVES['<='] = 
    new PrimProc('<=',
		 2,
		 true, false,
		 function(x, y, args) {
		 	args.unshift(y);
		 	args.unshift(x);
		 	arrayEach(args, function(z, i) {check(z, isNumber, '<=', 'number', i+1);});

		 	return compare(args, jsnums.lessThanOrEqual);
		 });


PRIMITIVES['>='] =
    new PrimProc('>=',
		 2,
		 true, false,
		 function(x, y, args) {
		 	args.unshift(y);
		 	args.unshift(x);
		 	arrayEach(args, function(z, i) {check(z, isNumber, '>=', 'number', i+1);});

		 	return compare(args, jsnums.greaterThanOrEqual);
		 });




PRIMITIVES['abs'] =
    new PrimProc('abs',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'abs', 'real', 1);
			return jsnums.abs(x);
		 });

/*
PRIMITIVES['quotient'] =
    new PrimProc('quotient',
		 2,
		 false, false,
		 function(x, y) {
		 	sys.print("typeof x and y: " + typeof x + " " + typeof y + "\n");
		 	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.quotient(x, y);
		 });


PRIMITIVES['remainder'] =
    new PrimProc('remainder',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.remainder(x, y);
		 });
*/

PRIMITIVES['modulo'] =
    new PrimProc('modulo',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isInteger, 'quotient', 'integer', 1);
			check(y, isInteger, 'quotient', 'integer', 2);

			return jsnums.modulo(x, y);
		 });


PRIMITIVES['max'] =
    new PrimProc('max',
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
    new PrimProc('min',
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
    new PrimProc('gcd',
		 1,
		 true, false,
		 function(x, args) {
		 	args.unshift(x);
		 	arrayEach(args, function(y, i) {check(y, isInteger, 'gcd', 'integer', i+1);});

		 	return jsnums.gcd.apply(null, args);
		 });

PRIMITIVES['lcm'] =
    new PrimProc('lcm',
		 1,
		 true, false,
		 function(x, args) {
		 	args.unshift(x);
		 	arrayEach(args, function(y, i) {check(y, isInteger, 'lcm', 'integer', i+1);});

		 	return jsnums.gcd.apply(null, args);
		 });


PRIMITIVES['floor'] =
    new PrimProc('floor',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'floor', 'real', 1);
			return jsnums.floor(x);
		 });


PRIMITIVES['ceiling'] =
    new PrimProc('ceiling',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'ceiling', 'real', 1);
			return jsnums.ceiling(x);
		 });


PRIMITIVES['round'] =
    new PrimProc('round',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'round', 'real', 1);
			return jsnums.round(x);
		 });


PRIMITIVES['numerator'] =
    new PrimProc('numerator',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isRational, 'numerator', 'rational', 1);
			return jsnums.numerator(x);
		 });


PRIMITIVES['denominator'] =
    new PrimProc('denominator',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isRational, 'denominator', 'rational', 1);
			return jsnums.denominator(x);
		 });


PRIMITIVES['expt'] = 
    new PrimProc("expt",
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isNumber, 'expt', 'number', 1);
			check(y, isNumber, 'expt', 'number', 2);
		 	return jsnums.expt(x, y);
		 });


PRIMITIVES['exp'] =
    new PrimProc('exp',
		 1,
		 false, false,
		 function(x, y) {
		 	check(x, isNumber, 'exp', 'number', 1);
			return jsnums.exp(x);
		 });


PRIMITIVES['log'] =
    new PrimProc('log',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'log', 'number', 1);
			return jsnums.log(x);
		 });


PRIMITIVES['sin'] =
    new PrimProc('sin',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'sin', 'number', 1);
			return jsnums.sin(x);
		 });


PRIMITIVES['cos'] =
    new PrimProc('cos',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'cos', 'number', 1);
			return jsnums.cos(x);
		 });


PRIMITIVES['tan'] =
    new PrimProc('tan',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'tan', 'number', 1);
			return jsnums.tan(x);
		 });


PRIMITIVES['asin'] =
    new PrimProc('asin',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'asin', 'number', 1);
			return jsnums.asin(x);
		 });


PRIMITIVES['acos'] =
    new PrimProc('acos',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'acos', 'number', 1);
			return jsnums.acos(x);
		 });


PRIMITIVES['atan'] =
    new PrimProc('atan',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'atan', 'number', 1);
			return jsnums.atan(x);
		 });


PRIMITIVES['sinh'] =
    new PrimProc('sinh',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'sinh', 'number', 1);
			return jsnums.sinh(x);
		 });


PRIMITIVES['cosh'] =
    new PrimProc('cosh',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'cosh', 'number', 1);
			return jsnums.cosh(x);
		 });


PRIMITIVES['sqr'] =
    new PrimProc('sqr',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'sqr', 'number', 1);
			return jsnums.sqr(x);
		 });


PRIMITIVES['sqrt'] =
    new PrimProc('sqrt',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'sqrt', 'number', 1);
			return jsnums.sqrt(x);
		 });


PRIMITIVES['integer-sqrt'] =
    new PrimProc('integer-sqrt',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isInteger, 'integer-sqrt', 'integer', 1);
			return jsnums.integerSqrt(x);
		 });


PRIMITIVES['make-rectangular'] =
    new PrimProc('make-rectangular',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isReal, 'make-rectangular', 'real', 1);
			check(y, isReal, 'make-rectangular', 'real', 2);
			return jsnums.makeRectangular(x, y);
		 });

PRIMITIVES['make-polar'] =
    new PrimProc('make-polar',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isReal, 'make-polar', 'real', 1);
			check(x, isReal, 'make-polar', 'real', 2);
			return jsnums.makePolar(x, y);
		 });


PRIMITIVES['real-part'] =
    new PrimProc('real-part',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'real-part', 'number', 1);
			return jsnums.realPart(x);
		 });


PRIMITIVES['imag-part'] =
    new PrimProc('imag-part',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'imag-part', 'number', 1);
			return jsnums.imaginaryPart(x);
		 });


PRIMITIVES['angle'] =
    new PrimProc('angle',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'angle', 'number', 1);
			return jsnums.angle(x);
		 });


PRIMITIVES['magnitude'] =
    new PrimProc('magnitude',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'magnitude', 'number', 1);
			return jsnums.magnitude(x);
		 });


PRIMITIVES['conjugate'] =
    new PrimProc('conjugate',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'conjugate', 'number', 1);
			return jsnums.conjugate(x);
		 });


PRIMITIVES['sgn'] =
    new PrimProc('sgn',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'sgn', 'real number', 1);
			if ( jsnums.greaterThan(x, 0) ) {
				return 1;
			}
			else if ( jsnums.lessThan(x, 0) ) {
				return -1;
			}
			else {
				return 0;
			}
		 });


PRIMITIVES['inexact->exact'] =
    new PrimProc('inexact->exact',
		 1,
		 false, false,
		 function (x) {
		 	check(x, isNumber, 'inexact->exact', 'number', 1);
			return jsnums.toExact(x);
		 });


PRIMITIVES['number->string'] =
    new PrimProc('number->string',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isNumber, 'number->string', 'number', 1);
			return types.string(x+'');
		 });


PRIMITIVES['string->number'] =
    new PrimProc('string->number',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string->number', 'string', 1);
			return jsnums.fromString(str);
		 });


PRIMITIVES['xml->s-exp'] =
    new PrimProc('xml->s-exp',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'xml->s-exp', 'string', 1);
			if (str.length == 0) {
				return types.string('');
			}

			var xmlDoc;
			try {
				//Internet Explorer
				xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
				xmlDoc.async = "false";
				xmlDoc.loadXML(s);
				// FIXME: check parse errors
			}
			catch(e) {
				var parser = new DOMParser();
				xmlDoc = parser.parseFromString(s, "text/xml");
				// FIXME: check parse errors
			}

			var parseAttributes = function(attrs) {
				var result = types.EMPTY;
				for (var i = 0; i < attrs.length; i++) {
					var keyValue = types.cons(types.symbol(attrs.item(i).nodeName),
								  types.cons(attrs.item(i).nodeValue,
									     types.EMPTY));
					result = types.cons(keyValue, result);
				}
				return types.cons(types.symbol("@"), result).reverse();
			};

			var parse = function(node) {
				if (node.nodeType == Node.ELEMENT_NODE) {
					var result = types.EMPTY;
					var child = node.firstChild;
					while (child != null) {
						var nextResult = parse(child);
						if (isString(nextResult) && 
						    !result.isEmpty() &&
						    isString(result.first())) {
							result = types.cons(result.first() + nextResult,
									    result.rest());
						} else {
							result = types.cons(nextResult, result);
						}
						child = child.nextSibling;
					}
					result = result.reverse();
					result = types.cons(parseAttributes(node.attributes),
							    result);
					result = types.cons(
						types.symbol(node.nodeName),
						result);
					return result;
				} else if (node.nodeType == Node.TEXT_NODE) {
					return node.textContent;
				} else if (node.nodeType == Node.CDATA_SECTION_NODE) {
					return node.data;
				} else {
					return types.EMPTY;
				}
			};
			var result = parse(xmlDoc.firstChild);
			return result;
		 });




/******************
 *** Predicates ***
 ******************/

PRIMITIVES['procedure?'] = new PrimProc('procedure?', 1, false, false, isFunction);

PRIMITIVES['pair?'] = new PrimProc('pair?', 1, false, false, isPair);
PRIMITIVES['cons?'] = new PrimProc('cons?', 1, false, false, isPair);
PRIMITIVES['empty?'] = new PrimProc('empty?', 1, false, false, isEmpty);
PRIMITIVES['null?'] = new PrimProc('null?', 1, false, false, isEmpty);

PRIMITIVES['undefined?'] = new PrimProc('undefined?', 1, false, false, function(x) { return x === UNDEF; });
PRIMITIVES['void?'] = new PrimProc('void?', 1, false, false, function(x) { return x === types.VOID; });

PRIMITIVES['symbol?'] = new PrimProc('symbol?', 1, false, false, isSymbol);
PRIMITIVES['string?'] = new PrimProc('string?', 1, false, false, isString);
PRIMITIVES['char?'] = new PrimProc('char?', 1, false, false, isChar);
PRIMITIVES['boolean?'] = new PrimProc('boolean?', 1, false, false, isBoolean);
PRIMITIVES['vector?'] = new PrimProc('vector?', 1, false, false, isVector);
PRIMITIVES['struct?'] = new PrimProc('struct?', 1, false, false, types.isStruct);
PRIMITIVES['eof-object?'] = new PrimProc('eof-object?', 1, false, false, function(x) { return x === types.EOF; });
PRIMITIVES['posn?'] = new PrimProc('posn?', 1, false, false, types.isPosn);

PRIMITIVES['number?'] = new PrimProc('number?', 1, false, false, isNumber);
PRIMITIVES['complex?'] = new PrimProc('complex?', 1, false, false, isComplex);
PRIMITIVES['real?'] = new PrimProc('real?', 1, false, false, isReal);
PRIMITIVES['rational?'] = new PrimProc('rational?', 1, false, false, isRational);
PRIMITIVES['integer?'] = new PrimProc('integer?', 1, false, false, isInteger);

PRIMITIVES['exact?'] =
    new PrimProc('exact?', 1, false, false,
		 function(x) {
			check(x, isNumber, 'exact?', 'number', 1);
			return jsnums.isExact(x);
		 });
PRIMITIVES['inexact?'] =
    new PrimProc('inexact?', 1, false, false,
		 function(x) {
			check(x, isNumber, 'inexact?', 'number', 1);
			return !jsnums.isExact(x);
		 });

PRIMITIVES['odd?'] =
    new PrimProc('odd?',
		 1,
		 false, false,
		 function(x) {
			check(x, isInteger, 'odd?', 'integer', 1);
			return jsnums.equals(jsnums.modulo(x, 2), 1);
		 });
PRIMITIVES['even?'] =
    new PrimProc('even?',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isInteger, 'even?', 'integer', 1);
			return jsnums.equals(jsnums.modulo(x, 2), 0);
		 });

PRIMITIVES['zero?'] =
    new PrimProc("zero?",
		 1,
		 false, false,
		 function(x) {
		     return jsnums.equals(0, x)
		 });

PRIMITIVES['positive?'] =
    new PrimProc('positive?',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'positive?', 'real', 1);
			return jsnums.greaterThan(x, 0);
		 });
PRIMITIVES['negative?'] =
    new PrimProc('negative?',
		 1,
		 false, false,
		 function(x) {
		 	check(x, isReal, 'negative?', 'real', 1);
			return jsnums.lessThan(x, 0);
		 });

PRIMITIVES['box?'] = new PrimProc('box?', 1, false, false, isBox);

PRIMITIVES['hash?'] = new PrimProc('hash?', 1, false, false, isHash);


PRIMITIVES['eq?'] = new PrimProc('eq?', 2, false, false, isEq);
PRIMITIVES['eqv?'] = new PrimProc('eqv?', 2, false, false, isEqv);
PRIMITIVES['equal?'] = new PrimProc('equal?', 2, false, false, isEqual);
PRIMITIVES['equal~?'] =
    new PrimProc('equal~?',
		 3,
		 false, false,
		 function(x, y, range) {
		 	check(range, function(n) {return isReal(n) && jsnums.greaterThanOrEqual(n, 0);},
			      'equal~?', 'non-negative real', 3);

			return (isEqual(x, y) ||
				(isReal(x) && isReal(y) &&
				 jsnums.lessThanOrEqual(jsnums.abs(jsnums.subtract(x, y)), range)));
		 });


PRIMITIVES['false?'] = new PrimProc('false?', 1, false, false, function(x) { return x === false; });
PRIMITIVES['boolean=?'] =
    new PrimProc('boolean=?',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isBoolean, 'boolean=?', 'boolean', 1);
			check(y, isBoolean, 'boolean=?', 'boolean', 2);
			return x === y;
		 });

PRIMITIVES['symbol=?'] =
    new PrimProc('symbol=?',
		 2,
		 false, false,
		 function(x, y) {
		 	check(x, isSymbol, 'symbol=?', 'symbol', 1);
			check(y, isSymbol, 'symbol=?', 'symbol', 2);
			return isEqual(x, y);
		 });


/***********************
 *** List Primitives ***
 ***********************/

PRIMITIVES['cons'] =
    new PrimProc('cons',
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
PRIMITIVES['first'] = new PrimProc('first', 1, false, false, carFirst('first'));
PRIMITIVES['car'] = new PrimProc('car', 1, false, false, carFirst('car'));


var cdrRest = function(name) {
	return function (lst) {
		checkList(lst, name, 1);
		return lst.rest();
	};
}
PRIMITIVES['rest'] = new PrimProc('rest', 1, false, false, cdrRest('rest'));
PRIMITIVES['cdr'] = new PrimProc('cdr', 1, false, false, cdrRest('cdr'));


PRIMITIVES['caar'] =
    new PrimProc('caar',
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
PRIMITIVES['cadr'] = new PrimProc('cadr', 1, false, false, cadrSecond('cadr'));
PRIMITIVES['second'] = new PrimProc('second', 1, false, false, cadrSecond('second'));

PRIMITIVES['cdar'] =
    new PrimProc('cdar',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'cdar', 1);
		 	lst.first().rest();
		 });

PRIMITIVES['cddr'] =
    new PrimProc('cddr',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'cddr', 1);
		 	lst.rest().rest();
		 });

PRIMITIVES['caaar'] =
    new PrimProc('caaar',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'caaar', 1);
		 	lst.first().first().first();
		 });

PRIMITIVES['caadr'] =
    new PrimProc('caadr',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'caadr', 1);
		 	lst.rest().first().first();
		 });

PRIMITIVES['cadar'] =
    new PrimProc('cadar',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'cadar', 1);
		 	lst.first().rest().first();
		 });

PRIMITIVES['cdaar'] =
    new PrimProc('cdaar',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'cdaar', 1);
		 	lst.first().first().rest();
		 });

PRIMITIVES['cdadr'] =
    new PrimProc('cdadr',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'cdadr', 1);
		 	lst.rest().first().rest();
		 });

PRIMITIVES['cddar'] =
    new PrimProc('cddar',
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
PRIMITIVES['caddr'] = new PrimProc('caddr', 1, false, false, caddrThird('caddr'));
PRIMITIVES['third'] = new PrimProc('third', 1, false, false, caddrThird('third'));

PRIMITIVES['cdddr'] =
    new PrimProc('cdddr',
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
PRIMITIVES['cadddr'] = new PrimProc('cadddr', 1, false, false, cadddrFourth('cadddr'));
PRIMITIVES['fourth'] = new PrimProc('fourth', 1, false, false, cadddrFourth('fourth'));

PRIMITIVES['fifth'] =
    new PrimProc('fifth',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'fifth', 1);
		 	lst.rest().rest().rest().rest().first();
		 });

PRIMITIVES['sixth'] =
    new PrimProc('sixth',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'sixth', 1);
		 	lst.rest().rest().rest().rest().rest().first();
		 });

PRIMITIVES['seventh'] =
    new PrimProc(
		 'seventh',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'seventh', 1);
		 	lst.rest().rest().rest().rest().rest().rest().first();
		 });

PRIMITIVES['eighth'] =
    new PrimProc('eighth',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'eighth', 1);
		 	lst.rest().rest().rest().rest().rest().rest().rest().first();
		 });


PRIMITIVES['length'] =
    new PrimProc('length',
		 1,
		 false, false,
		 function(lst) {
		  	return jsnums.makeRational(length(lst));
		 });


PRIMITIVES['list?'] = new PrimProc('list?', 1, false, false, isList);


PRIMITIVES['list'] =
    new PrimProc('list',
		 0,
		 true, false,
		 types.list);


PRIMITIVES['list*'] =
    new PrimProc('list*',
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
    new PrimProc('list-ref',
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
    new PrimProc('append',
		 0,
		 true, false,
		 append);


PRIMITIVES['reverse'] =
    new PrimProc('reverse',
		 1,
		 false, false,
		 function(lst) {
		 	checkList(lst, 'reverse', 1);
		 	return lst.reverse();
		 });


PRIMITIVES['map'] =
    new PrimProc('map',
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
    new PrimProc('andmap',
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
    new PrimProc('ormap',
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


PRIMITIVES['memq'] =
    new PrimProc('memq',
		 2,
		 false, false,
		 function(item, lst) {
		 	checkList(lst, 'memq', 2);
			while ( !lst.isEmpty() ) {
				if ( isEq(item, lst.first()) ) {
					return lst;
				}
				lst = lst.rest();
			}
			return false;
		 });


PRIMITIVES['memv'] =
    new PrimProc('memv',
		 2,
		 false, false,
		 function(item, lst) {
		 	checkList(lst, 'memv', 2);
			while ( !lst.isEmpty() ) {
				if ( isEqv(item, lst.first()) ) {
					return lst;
				}
				lst = lst.rest();
			}
			return false;
		 });


PRIMITIVES['member'] =
    new PrimProc('member',
		 2,
		 false, false,
		 function(item, lst) {
		 	checkList(lst, 'member', 2);
		 	while ( !lst.isEmpty() ) {
		 		if ( isEqual(item, lst.first()) ) {
		 			return lst;
		 		}
		 		lst = lst.rest();
		 	}
		 	return false;
		 });


PRIMITIVES['memf'] =
    new PrimProc('memf',
		 2,
		 false, false,
		 function(f, initList) {
		 	check(f, isFunction, 'memf', 'function', 1);
			checkList(initList, 'memf', 2);

			var memfHelp = function(lst) {
				if ( lst.isEmpty() ) {
					return false;
				}

				return CALL(f, [lst.first()],
					function(result) {
						if (result) {
							return lst;
						}
						return memfHelp(lst.rest());
					});
			}
			return memfHelp(initList);
		 });


PRIMITIVES['assq'] =
    new PrimProc('assq',
		 2,
		 false, false,
		 function(item, lst) {
		 	checkListOf(lst, isPair, 'assq', 'pair', 2);
			while ( !lst.isEmpty() ) {
				if ( isEq(item, lst.first().first()) ) {
					return lst.first();
				}
				lst = lst.rest();
			}
			return false;
		 });


PRIMITIVES['remove'] =
    new PrimProc('remove',
		 2,
		 false, false,
		 function(item, lst) {
		 	checkList(lst, 'remove', 2);
		 	var originalLst = lst;
		 	var result = types.EMPTY;
		 	while ( !lst.isEmpty() ) {
		 		if ( isEqual(item, lst.first()) ) {
		 			return append([result.reverse(), lst.rest()]);
		 		} else {
		 			result = types.cons(lst.first(), result);
		 			lst = lst.rest();
		 		}
		 	}
		 	return originalLst;
		 });


PRIMITIVES['filter'] =
    new PrimProc('filter',
		 2,
		 false, false,
		 function(f, lst) {
		 	check(f, funArityContains(1), 'filter', 'procedure (arity 1)', 1);
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
    new PrimProc('foldl',
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
    new PrimProc('foldr',
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


PRIMITIVES['quicksort'] = new PrimProc('quicksort', 2, false, false, quicksort('quicksort'));
PRIMITIVES['sort'] = new PrimProc('sort', 2, false, false, quicksort('sort'));



PRIMITIVES['argmax'] =
    new PrimProc('argmax',
		 2,
		 false, false,
		 function(f, initList) {
		 	check(f, isFunction, 'argmax', 'function', 1);
			check(initList, isPair, 'argmax', 'non-empty list', 2);

			var argmaxHelp = function(lst, curMaxVal, curMaxElt) {
				if ( lst.isEmpty() ) {
					return curMaxElt;
				}

				return CALL(f, [lst.first()],
					function(result) {
						check(result, isReal, 'argmax',
						      'procedure that returns real numbers', 1);
						if (jsnums.greaterThan(result, curMaxVal)) {
							return argmaxHelp(lst.rest(), result, lst.first());
						}
						else {
							return argmaxHelp(lst.rest(), curMaxVal, curMaxElt);
						}
					});
			}
			return CALL(f, [initList.first()],
				function(result) {
					check(result, isReal, 'argmax', 'procedure that returns real numbers', 1);
					return argmaxHelp(initList.rest(), result, initList.first());
				});
		 });


PRIMITIVES['argmin'] =
    new PrimProc('argmin',
		 2,
		 false, false,
		 function(f, initList) {
		 	check(f, isFunction, 'argmin', 'function', 1);
			check(initList, isPair, 'argmin', 'non-empty list', 2);

			var argminHelp = function(lst, curMaxVal, curMaxElt) {
				if ( lst.isEmpty() ) {
					return curMaxElt;
				}

				return CALL(f, [lst.first()],
					function(result) {
						check(result, isReal, 'argmin',
						      'procedure that returns real numbers', 1);
						if (jsnums.lessThan(result, curMaxVal)) {
							return argminHelp(lst.rest(), result, lst.first());
						}
						else {
							return argminHelp(lst.rest(), curMaxVal, curMaxElt);
						}
					});
			}
			return CALL(f, [initList.first()],
				function(result) {
					check(result, isReal, 'argmin', 'procedure that returns real numbers', 1);
					return argminHelp(initList.rest(), result, initList.first());
				});
		 });


PRIMITIVES['build-list'] =
    new PrimProc('build-list',
		 2,
		 false, false,
		 function(num, f) {
		 	check(num, isNatural, 'build-list', 'natural', 1);
			check(f, isFunction, 'build-list', 'function', 2);

			var buildListHelp = function(n, acc) {
				if ( jsnums.greaterThanOrEqual(n, num) ) {
					return acc.reverse();
				}

				return CALL(f, [n],
					function (result) {
						return buildListHelp(n+1, types.cons(result, acc));
					});
			}
			return buildListHelp(0, types.EMPTY);
		 });


/**********************
 *** Box Primitives ***
 **********************/


PRIMITIVES['box'] = new PrimProc('box', 1, false, false, types.box);

PRIMITIVES['unbox'] =
    new PrimProc('unbox',
		 1,
		 false, false,
		 function(box) {
		 	check(box, isBox, 'unbox', 'box', 1);
			return box.unbox();
		 });


PRIMITIVES['set-box!'] =
    new PrimProc('set-box!',
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


PRIMITIVES['make-hash'] =
    new CasePrimitive(
	[new PrimProc('make-hash', 0, false, false, function() { return types.hash(types.EMPTY); }),
	 new PrimProc('make-hash',
		      1,
		      false, false,
		      function(lst) {
			  checkListOf(lst, isPair, 'make-hash', 'list of pairs', 1);
			  return types.hash(lst);
		      }) ]);

PRIMITIVES['make-hasheq'] =
    new CasePrimitive(
	[new PrimProc('make-hasheq', 0, false, false, function() { return types.hashEq(types.EMPTY); }),
	 new PrimProc('make-hasheq',
		      1,
		      false, false,
		      function(lst) {
			  checkListOf(lst, isPair, 'make-hasheq', 'list of pairs', 1);
			  return types.hashEq(lst);
		      }) ]);

PRIMITIVES['hash-set!'] =
    new PrimProc('hash-set!',
		 3,
		 false, false,
		 function(obj, key, val) {
		 	check(obj, isHash, 'hash-set!', 'hash', 1);
			obj.hash.put(key, val);
			return types.VOID;
		 });

PRIMITIVES['hash-ref'] =
    new CasePrimitive(
	[new PrimProc('hash-ref',
		      2,
		      false, false,
		      function(obj, key) {
			  check(obj, isHash, 'hash-ref', 'hash', 1);

			  if ( !obj.hash.containsKey(key) ) {
			  	// TODO Throw some sort of exception here!
			  	throw new Error('hash-ref: object not in hash, no default value');
			  }
			  return obj.hash.get(key);
		      }),
	 new PrimProc('hash-ref',
		      3,
		      false, false,
		      function(obj, key, defaultVal) {
			  check(obj, isHash, 'hash-ref', 'hash', 1);

			  if (obj.hash.containsKey(key)) {
				return obj.hash.get(key);
			  }
			  else {
				if (isFunction(defaultVal)) {
					return CALL(defaultVal, [], id);
				}
				return defaultVal;
			  }
		      }) ]);

PRIMITIVES['hash-remove!'] =
    new PrimProc('hash-remove',
		 2,
		 false, false,
		 function(obj, key) {
		 	check(obj, isHash, 'hash-remove!', 'hash', 1);
			obj.hash.remove(key);
			return types.VOID;
		 });

PRIMITIVES['hash-map'] =
    new PrimProc('hash-map',
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
						return hashMapHelp(i+1, types.cons(result, acc));
					});
			}
			return hashMapHelp(0, types.EMPTY);
		 });


PRIMITIVES['hash-for-each'] =
    new PrimProc('hash-for-each',
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
    new PrimProc('make-string',
		 2,
		 false, false,
		 function(n, c) {
		 	check(n, isNatural, 'make-string', 'natural', 1);
			check(c, isChar, 'make-string', 'char', 2);

			var ret = [];
			for (var i = 0; jsnums.lessThan(i, n); i++) {
				ret.push(c.val);
			}
			return types.string(ret);
		 });


PRIMITIVES['replicate'] =
    new PrimProc('replicate',
		 2,
		 false, false,
		 function(n, str) {
		 	check(n, isNatural, 'replicate', 'natural', 1);
			check(str, isString, 'replicate', 'string', 2);

			var ret = "";
			var primStr = str.toString();
			for (var i = 0; jsnums.lessThan(i, n); i++) {
				ret += primStr;
			}
			return types.string(ret);
		 });


PRIMITIVES['string'] =
    new PrimProc('string',
		 0,
		 true, false,
		 function(chars) {
			arrayEach(chars, function(c, i) {check(c, isChar, 'string', 'char', i+1);});

			var ret = [];
			for (var i = 0; i < chars.length; i++) {
				ret.push(chars[i].val);
			}
			return types.string(ret);
		 });


PRIMITIVES['string-length'] =
    new PrimProc('string-length', 1, false, false,
		 function(str) {
		 	check(str, isString, 'string-length', 'string', 1);
			return str.length;
		 });


PRIMITIVES['string-ref'] =
    new PrimProc('string-ref',
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
    new PrimProc('string=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
		 	strs.unshift(str1);
		 	arrayEach(strs, function(str, i) {check(str, isString, 'string=?', 'string', i+1);});
		 	
			return compare(strs, function(strA, strB) {return strA.isEqual(strB, new types.UnionFind());});
		 });


PRIMITIVES['string-ci=?'] =
    new PrimProc('string-ci=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);

			for(var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA.isEqual(strB, new types.UnionFind());});
		 });


PRIMITIVES['string<?'] =
    new PrimProc('string<?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string<?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA.lessThan(strB);});
		 });


PRIMITIVES['string>?'] =
    new PrimProc('string>?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string>?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA.greaterThan(strB);});
		 });


PRIMITIVES['string<=?'] =
    new PrimProc('string<=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string<=?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA.lessThanOrEqual(strB);});
		 });


PRIMITIVES['string>=?'] =
    new PrimProc('string>=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);
			arrayEach(strs, function(str, i) {check(str, isString, 'string>=?', 'string', i+1);});

			return compare(strs, function(strA, strB) {return strA.greaterThanOrEqual(strB);});
		 });


PRIMITIVES['string-ci<?'] =
    new PrimProc('string-ci<?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci<?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA.lessThan(strB);});
		 });


PRIMITIVES['string-ci>?'] =
    new PrimProc('string-ci>?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci>?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA.greaterThan(strB);});
		 });


PRIMITIVES['string-ci<=?'] =
    new PrimProc('string-ci<=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci<=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA.lessThanOrEqual(strB);});
		 });


PRIMITIVES['string-ci>=?'] =
    new PrimProc('string-ci>=?',
		 2,
		 true, false,
		 function(str1, str2, strs) {
		 	strs.unshift(str2);
			strs.unshift(str1);

			for (var i = 0; i < strs.length; i++) {
				check(strs[i], isString, 'string-ci>=?', 'string', i+1);
				strs[i] = strs[i].toLowerCase();
			}

			return compare(strs, function(strA, strB) {return strA.greaterThanOrEqual(strB);});
		 });


PRIMITIVES['substring'] =
    new CasePrimitive(
	[new PrimProc('substring',
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
	 new PrimProc('substring',
		      3,
		      false, false,
		      function(str, theStart, theEnd) {
		          check(str, isString, 'substring', 'string', 1);
			   check(theStart, isNatural, 'substring', 'natural', 2);
			   check(theEnd, isNatural, 'substring', 'natural', 3);

			   var start = jsnums.toFixnum(theStart);
			   var end = jsnums.toFixnum(theEnd);
			   if (start > str.length || end > str.length || end < start) {
			  	// TODO Throw some sort of error here!
			   }
			   else {
			  	return str.substring(start, end);
			   }
		      }) ]);


PRIMITIVES['string-append'] = 
    new PrimProc("string-append",
		 0,
		 true, false,
		 function(args) {
		 	arrayEach(args,
				function(str, i) {
					check(str, isString, 'string-append', 'string', i+1);
				});
			
			for (var i = 0; i < args.length; i++) {
				args[i] = args[i].toString();
			}
			return types.string(args.join(""));
		 });


PRIMITIVES['string->list'] =
    new PrimProc('string->list',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string->list', 'string', 1);

			var lst = types.EMPTY;
			for (var i = str.length-1; i >= 0; i--) {
				lst = types.cons(types.char(str.charAt(i)), lst);
			}
			return lst;
		 });


PRIMITIVES['list->string'] =
    new PrimProc('list->string',
		 1,
		 false, false,
		 function(lst) {
		 	checkListOf(lst, isChar, 'list->string', 'char', 1);

			var ret = [];
			while( !lst.isEmpty() ) {
				ret.push(lst.first().val);
				lst = lst.rest();
			}
			return types.string(ret);
		 });


PRIMITIVES['string-copy'] =
    new PrimProc('string-copy',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-copy', 'string', 1);
			return str.copy();
		 });



PRIMITIVES['string->symbol'] =
    new PrimProc('string->symbol',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string->symbol', 'string', 1);
			return types.symbol(str);
		 });


PRIMITIVES['symbol->string'] =
    new PrimProc('symbol->string',
		 1,
		 false, false,
		 function(symb) {
		 	check(symb, isSymbol, 'symbol->string', 'symbol', 1);
			return types.string(symb.toString());
		 });


PRIMITIVES['format'] = new PrimProc('format', 1, true, false, format('format'));
    new PrimProc('format', 1, true, false,
		 function(formatStr, args) {
			return format(formatStr, args);
		 });


PRIMITIVES['printf'] =
    new PrimProc('printf', 1, true, false,
		 function(formatStr, args) {
			var msg = (format('printf'))(formatStr, args);
			printHook(msg);
			return types.VOID;
		 });


PRIMITIVES['string->int'] =
    new PrimProc('string->int',
		 1,
		 false, false,
		 function(str) {
		 	check(str, function(s) {return isString(s) && s.length == 1;},
			      'string->int', '1-letter string', 1);
			return str.charCodeAt(0);
		 });


PRIMITIVES['int->string'] =
    new PrimProc('int->string',
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
				'int->string',
				'exact integer in [0,55295] or [57344,1114111]',
				1);

			return types.string( String.fromCharCode(jsnums.toFixnum(num)) );
		 });


PRIMITIVES['explode'] =
    new PrimProc('explode',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'explode', 'string', 1);
			var ret = types.EMPTY;
			for (var i = str.length-1; i >= 0; i--) {
				ret = types.cons( types.string(str.charAt(i)), ret );
			}
			return ret;
		 });

PRIMITIVES['implode'] =
    new PrimProc('implode',
		 1,
		 false, false,
		 function(lst) {
		 	checkListOf(lst, function(x) { return isString(x) && x.length == 1; },
				    'implode', 'list of 1-letter strings', 1);
			var ret = [];
			while ( !lst.isEmpty() ) {
				ret.push( lst.first().toString() );
				lst = lst.rest();
			}
			return types.string(ret);
		 });


PRIMITIVES['string-alphabetic?'] =
    new PrimProc('string-alphabetic?',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-alphabetic?', 'string', 1);
			return isAlphabeticString(str);
		 });


PRIMITIVES['string-ith'] =
    new PrimProc('string-ith',
		 2,
		 false, false,
		 function(str, num) {
		 	check(str, isString, 'string-ith', 'string', 1);
			check(num, isNatural, 'string-ith', 'natural', 2);
			var n = jsnums.toFixnum(num);

			if (n >= str.length) {
				// TODO Throw some sort of error here!
			}
			return types.string(str.charAt(n));
		 });


PRIMITIVES['string-lower-case?'] =
    new PrimProc('string-lower-case?',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-lower-case?', 'string', 1);
			var primStr = str.toString();
			return isAlphabeticString(str) && primStr.toLowerCase() === primStr;
		 });


PRIMITIVES['string-numeric?'] =
    new PrimProc('string-numeric?',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-numeric?', 'string', 1);
			return isNumericString(str);
		 });


PRIMITIVES['string-upper-case?'] =
    new PrimProc('string-upper-case?',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-upper-case?', 'string', 1);
			var primStr = str.toString();
			return isAlphabeticString(str) && primStr.toUpperCase() === primStr;
		 });


PRIMITIVES['string-whitespace?'] =
    new PrimProc('string-whitespace?',
		 1,
		 false, false,
		 function(str) {
		 	check(str, isString, 'string-whitespace?', 'string', 1);
			return isWhitespaceString(str);
		 });


PRIMITIVES['build-string'] =
    new PrimProc('build-string',
		 2,
		 false, false,
		 function(num, f) {
		 	check(num, isNatural, 'build-string', 'natural', 1);
			check(f, isFunction, 'build-string', 'function', 2);

			var buildStringHelp = function(n, acc) {
				if ( jsnums.greaterThanOrEqual(n, num) ) {
					return types.string(acc);
				}

				return CALL(f, [n],
					function(res) {
						check(res, isChar, 'build-string',
						      'procedure that returns a char', 2);
						return buildStringHelp(n+1, acc.push(res.val));
					});
			}
			return buildStringHelp(0, []);
		 });





/*************************
 *** Vector Primitives ***
 *************************/


PRIMITIVES['make-vector'] =
    new PrimProc('make-vector',
		 2,
		 false, false,
		 function(size, content) {
		 	check(size, isNatural, 'make-vector', 'natural', 1);
			var s = jsnums.toFixnum(size);
			var ret = [];
			for (var i = 0; i < s; i++) {
				ret.push(content);
			}
			return types.vector(ret);
		 });


PRIMITIVES['vector'] =
    new PrimProc('vector',
		 0,
		 true, false,
		 function(args) {
		 	return types.vector(args);
		 });


PRIMITIVES['vector-length'] =
    new PrimProc('vector-length',
		 1,
		 false, false,
		 function(vec) {
		 	check(vec, isVector, 'vector-length', 'vector', 1);
			return vec.length();
		 });


PRIMITIVES['vector-ref'] =
    new PrimProc('vector-ref',
		 2,
		 false, false,
		 function(vec, index) {
		 	check(vec, isVector, 'vector-ref', 'vector', 1);
			check(index, isNatural, 'vector-ref', 'natural', 2);
			var i = jsnums.toFixnum(index);
			if (i <= vec.length()) {
				// TODO Throw some sort of error here!
			}
			return vec.ref(i);
		 });


PRIMITIVES['vector-set!'] =
    new PrimProc('vector-set!',
		 3,
		 false, false,
		 function(vec, index, val) {
		 	check(vec, isVector, 'vector-set!', 'vector', 1);
			check(index, isNatural, 'vector-set!', 'natural', 2);
			var i = jsnums.toFixnum(index);
			if (i <= vec.length()) {
				// TODO Throw some sort of error here!
			}
			vec.set(i, val);
			return types.VOID;
		 });


PRIMITIVES['vector->list'] =
    new PrimProc('vector->list',
		 1,
		 false, false,
		 function(vec) {
		 	check(vec, isVector, 'vector->list', 'vector', 1);
			return vec.toList();
		 });


PRIMITIVES['build-vector'] =
    new PrimProc('build-vector',
		 2,
		 false, false,
		 function(num, f) {
		 	check(num, isNatural, 'build-vector', 'natural', 1);
			check(f, isFunction, 'build-vector', 'function', 2);

			var buildVectorHelp = function(n, acc) {
				if ( jsnums.greaterThanOrEqual(n, num) ) {
					return types.vector(acc);
				}

				return CALL(f, [n],
					function (result) {
						return buildVectorHelp(n+1, acc.push(result));
					});
			}
			return buildVectorHelp(0, []);
		 });



/***********************
 *** Char Primitives ***
 ***********************/


PRIMITIVES['char=?'] =
    new PrimProc('char=?',
		 2,
		 true, false,
		 function(char1, char2, chars) {
		 	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val === c2.val;});
		 });


PRIMITIVES['char<?'] =
    new PrimProc('char<?',
		 2,
		 true, false,
		 function(char1, char2, chars) {
		 	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char<?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val < c2.val;});
		 });


PRIMITIVES['char>?'] =
    new PrimProc('char>?',
		 2,
		 true, false,
		 function(char1, char2, chars) {
		 	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char>?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val > c2.val;});
		 });


PRIMITIVES['char<=?'] =
    new PrimProc('char<=?',
		 2,
		 true, false,
		 function(char1, char2, chars) {
		 	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char<=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val <= c2.val;});
		 });


PRIMITIVES['char>=?'] =
    new PrimProc('char>=?',
		 2,
		 true, false,
		 function(char1, char2, chars) {
		 	chars.unshift(char2);
			chars.unshift(char1);
			arrayEach(chars, function(c, i) {check(c, isChar, 'char>=?', 'char', i+1);});

			return compare(chars, function(c1, c2) {return c1.val >= c2.val;});
		 });


PRIMITIVES['char-ci=?'] =
    new PrimProc('char-ci=?',
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
    new PrimProc('char-ci<?',
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
    new PrimProc('char-ci>?',
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
    new PrimProc('char-ci<=?',
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
    new PrimProc('char-ci>=?',
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
    new PrimProc('char-alphabetic?',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-alphabetic?', 'char', 1);
			return isAlphabeticString(c.val);
		 });


PRIMITIVES['char-numeric?'] =
    new PrimProc('char-numeric?',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-numeric?', 'char', 1);
			return (c.val >= '0' && c.val <= '9');
		 });


PRIMITIVES['char-whitespace?'] =
    new PrimProc('char-whitespace?',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-whitespace?', 'char', 1);
			return isWhitespaceString(c.val);
		 });


PRIMITIVES['char-upper-case?'] =
    new PrimProc('char-upper-case?',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-upper-case?', 'char', 1);
			return (isAlphabeticString(c.val) && c.val.toUpperCase() === c.val);
		 });


PRIMITIVES['char-lower-case?'] =
    new PrimProc('char-lower-case?',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-lower-case?', 'char', 1);
			return (isAlphabeticString(c.val) && c.val.toLowerCase() === c.val);
		 });


PRIMITIVES['char->integer'] =
    new PrimProc('char->integer',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char->integer', 'char', 1);
			return c.val.charCodeAt(0);
		 });


PRIMITIVES['integer->char'] =
    new PrimProc('integer->char',
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
    new PrimProc('char-upcase',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-upcase', 'char', 1);
			return types.char( c.val.toUpperCase() );
		 });


PRIMITIVES['char-downcase'] =
    new PrimProc('char-downcase',
		 1,
		 false, false,
		 function(c) {
		 	check(c, isChar, 'char-downcase', 'char', 1);
			return types.char( c.val.toLowerCase() );
		 });



/***********************
 *** Posn Primitives ***
 ***********************/


PRIMITIVES['make-posn'] =
    new PrimProc('make-posn',
		 2,
		 false, false,
		 function(x, y) {
		 	return types.posn(x, y);
		 });


PRIMITIVES['posn-x'] =
    new PrimProc('posn-x',
		 1,
		 false, false,
		 function(p) {
		 	check(p, types.isPosn, 'posn-x', 'posn', 1);
			return types.posnX(p);
		 });


PRIMITIVES['posn-y'] =
    new PrimProc('posn-y',
		 1,
		 false, false,
		 function(p) {
		 	check(p, types.isPosn, 'posn-y', 'posn', 1);
			return types.posnY(p);
		 });



/***************************
 *** Primitive Constants ***
 ***************************/


PRIMITIVES['eof'] = new PrimConst('eof', types.EOF);
PRIMITIVES['e'] = new PrimConst('e', jsnums.e);
PRIMITIVES['empty'] = new PrimConst('empty', types.EMPTY);
PRIMITIVES['false'] = new PrimConst('false', false);
PRIMITIVES['true'] = new PrimConst('true', true);
PRIMITIVES['pi'] = new PrimConst('pi', jsnums.pi);
PRIMITIVES['null'] = new PrimConst('null', types.EMPTY);




/////////////////////////////////////////////////////////////////////////////////////////////

primitive.getPrimitive = function(name) {
    return PRIMITIVES[name];
};

primitive.isPrimitive = function(x) {
    return x instanceof PrimProc;
};

primitive.addPrimitive = function(name, aPrim) {
    PRIMITIVES[name] = aPrim;
};

primitive.Primitive = PrimProc;
primitive.CasePrimitive = CasePrimitive;
primitive.PrimConst = PrimConst;


primitive.setCALL = setCALL;

})();

