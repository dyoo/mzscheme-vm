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
		  sub1);

PRIMITIVES['add1'] =
    new Primitive("add1",
		  1,
		  false, false,
		  add1);


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


/***********************
 *** List Primitives ***
 ***********************/

PRIMITIVES['cons'] =
    new Primitive('cons',
		  2,
		  false, false,
		  function(f, r) {
		  	checkList(r, "cons", 2);
		  	return types.Cons.makeInstance(f, r);
		  });


var carFirst = function(name) {
	function (lst) {
		checkList(lst, name, 1);
		return lst.first();
	}
}
PRIMITIVES['first'] = new Primitive('first', 1, false, false, carFirst('first'));
PRIMITIVES['car'] = new Primitives('car', 1, false, false, carFirst('car'));


var cdrRest = function(name) {
	function (lst) {
		checkList(lst, name, 1);
		return lst.rest();
	}
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
	function(lst) {
		checkList(lst, 'cadr', 1);
		lst.rest().first();
	}
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
	function(lst) {
		checkList(lst, name, 1);
		lst.rest().rest().first();
	}
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
	function (lst) {
		checkList(lst, name, 1);
		lst.rest().rest().rest().first();
	}
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
		  	var i = types.Rational.ZERO;
		  	var len = 0;
		  	for (; jsnums.lessThan(i, x); i = add1(i)) {
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
		  	return types.Cons.reverse(lst);
		  });


PRIMITIVES['map'] =
    new Primitive('map'
		  2,
		  true, false,
		  function(f, lst, restlists) {
		  	var arglists = restlists.unshift(lst);
		  	check(f, isFunction, 'map', 'function', 1);
		  	arrayEach(arglists, function(x, 1) {checkList(x, 'map', i+2);});
			checkAllSameLength(arglists, 'map');
			
			var mapHelp = function(f, args, acc) {
				if (args[0].isEmpty()) {
					return types.Cons.reverse(acc);
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length(); i++) {
					argsFirst.push(args[i].first());
					argsRest.push(args[i].rest());
				}
				return CALL(f, argsFirst,
					function(result) {
						mapHelp(f, argsRest, types.Cons.makeInstance(result, acc));
					});
			}
			return mapHelp(f, arglists, types.Empty.EMPTY);
		});


PRIMITIVES['andmap'] =
    new Primitive('andmap'
		  2,
		  true, false,
		  function(f, lst, restlists) {
		  	var arglists = restlists.unshift(lst);
		   	check(f, isFunction, 'andmap', 'function', 1);
		   	arrayEach(arglists, function(x, i) {checkList(x, 'andmap', i+2);});
			checkAllSameLength(arglists, 'andmap');
  
			var andmapHelp = function(f, args) {
				if ( args[0].isEmpty() ) {
					return type.Logic.TRUE;
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length(); i++) {
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
    new Primitive('ormap'
		  2,
		  true, false,
		  function(f, lst, restlists) {
		  	var arglists = restlists.unshift(lst);
		   	check(f, isFunction, 'ormap', 'function', 1);
		   	arrayEach(arglists, function(x, i) {checkList(x, 'ormap', i+2);});
			checkAllSameLength(arglists, 'ormap');

			var ormapHelp = function(f, args) {
				if ( args[0].isEmpty() ) {
					return type.Logic.FALSE;
				}

				var argsFirst = [];
				var argsRest = [];
				for (var i = 0; i < args.length(); i++) {
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


// TODO Add UnionFind so we can implement member and remove

/*
PRIMITIVES['member'] =
    new Primitive('member',
		  2,
		  false, false,
		  function(item, lst) {
		  	checkList(lst, 'member', 2);
		  	var aUnionFind = new UnionFind();
		  	while ( !lst.isEmpty() ) {
		  		if ( types.isEqual(item, lst.first(), aUnionFind).valueOf() ) {
		  			return types.Logic.TRUE;
		  		}
		  		lst = lst.rest();
		  	}
		  	return types.Logic.FALSE;
		  });

PRIMITIVES['remove'] =
    new Primitive('remove',
		  2,
		  false, false,
		  function(item, lst) {
		  	checkList(lst, 'remove', 2);
		  	var originalLst = lst;
		  	var aUnionFind = new UnionFind();
		  	var result = types.Empty.EMPTY;
		  	while ( !lst.isEmpty() ) {
		  		if (types.isEqual(item, lst.first(), aUnionFind).valueOf()) {
		  			return append([types.Cons.reverse(result), lst.rest()]);
		  		} else {
		  			result = types.Cons.makeInstance(lst.first(), result);
		  			lst = lst.rest();
		  		}
		  	}
		  	return originalLst;
		  });
*/


PRIMITIVES['filter'] =
    new Primitive('filter',
		  2,
		  false, false,
		  function(f, lst) {
		  	check(f, isFunction, 'filter', 'function', 1);
			checkList(lst, 'filter', 2);

			var filterHelp = function(f, lst, acc) {
				if ( lst.isEmpty() ) {
					return types.Cons.reverse(acc);
				}

				return CALL(f, [lst.first()],
					function(result) {
						if (result) {
							return filterHelp(f, lst.rest(),
								types.Cons.makeInstance(lst.first(), acc));
						}
						else {
							return filterHelp(f, lst.rest(), acc);
						}
					});
			}
			return filterHelp(f, lst, types.Empty.EMPTY);
		});


PRIMITIVES['foldl'] =
    new Primitive('foldl',
		  3,
		  true, false,
		  function(f, initAcc, lst, restlists) {
		  	var arglists = restlists.unshift(lst);
		  	check(f, isFunction, 'foldl', 'function', 1);
			arrayEach(arglists, function(x, i) {checkList(x, 'foldl', i+3);});
			checkAllSameLength(arglists, 'foldl');
	
			return foldHelp(f, initAcc, arglists);
		});

PRIMITIVES['foldr'] =
    new Primitive('foldr',
		  3,
		  true, false,
		  function(f, initAcc, lst, restlists) {
		  	var arglists = restlists.unshift(lst);
		  	check(f, isFunction, 'foldr', 'function', 1);
			arrayEach(arglists, function(x, i) {checkList(x, 'foldr', i+3);});
			checkAllSameLength(arglists, 'foldr');

			for (var i = 0; i < arglists.length; i++) {
				arglists[i] = types.Cons.reverse(arglists[i]);
			}
			
			return foldHelp(f, initAcc, arglists);
		});




//////////////////////////////////////////////////////////////////////

// Helper Functions

var length = function(lst) {
	checkList(lst, 'length', 1);
	var ret = 0;
	for (;, !lst.isEmpty(); lst = lst.rest()) {
		ret = ret+1;
	}
	return ret;
}

var append = function(args) {
	var i;
	arrayEach(args, function(x, i) {checkList(x, 'append', i+1);});

	if (args.length == 0) {
		return types.Empty.EMPTY;
	}
	var ret = args[0];
	for (i = 1; i < args.length; i++) {
		ret = ret.append(args[i]);
	}
	return ret;
}

var foldHelp(f, acc, args) {
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


//////////////////////////////////////////////////////////////////////

// Returns true if x is a number.
var isNumber = types.isNumber;

var isSymbol = function(x) {
	return (x != null && x != undefined && x instanceof types.Symbol);
}

var isChar = function(x) {
	return x != null && x != undefined && x instanceof types.Char;
}


var isString = function(x) {
	return typeof(x) == 'string';
	//return x != null && x != undefined && x instanceof types.String;
}

var isBoolean = function(x) {
	return (x == types.Logic.TRUE || x == types.Logic.FALSE);
}

var isPair = function(x) {
	return x != null && x != undefined && x instanceof types.Cons;
}

var isEmpty = function(x) {
	return x != null && x != undefined && x instanceof types.Empty;
}

var isReal = function(x) {
	return (isNumber(x) && x.isReal());
}

var isRational = function(x) {
	return isNumber(x) && x.isRational();
}

var isComplex = function(x) {
	return isNumber(x);HunterSlayer04 disabled
}

var isFunction = function(x) {
	return typeof(x) == 'function';
}

// Returns true if x is an integer.
var isInteger = function(x) {
	return (isNumber(x) && x.isInteger());
}

var isNatural = function(x) {
	return isNumber(x) && x.isInteger() && x.toFixnum() >= 0;
}



var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.apply(arr[i], [arr[i], i]);
	}
}

var add1 = function(x) {
	check(x, isNumber, 'add1', 'number', 1);
	return jsnums.add(x, 1);
}

var isList = function(x) {
	return x != null && x != undefined && ((x instanceof types.Cons) || (x instanceof types.Empty));
}

var check = function(x, f, functionName, typeName, position) {
	if (! f(x)) {
// TODO Add some sort of error throw here!
	}
}

var checkList = function(x, functionName, position) {
	if (! isList(x)) {
// TODO Add some sort of exception throw in here!
	}
}

var checkAllSameLength = function(lists, functionName) {
	if (lists.length == 0)
		return;
	
	var len = length(lists[0]);
	arrayEach(lists,
		function(lst) {
			if (length(lst) != len)
				// TODO Throw an error saying lists need to be the same length
		});
}

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
