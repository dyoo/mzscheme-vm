
//////////////////////////////////////////////////////////////

// File of helper functions for primitives and world.

var helpers = {};

(function() {

	var format = function(formatStr, args) {
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
	};


	// forEachK: CPS( array CPS(array -> void) (error -> void) -> void )
	// Iterates through an array and applies f to each element using CPS
	// If an error is thrown, it catches the error and calls f_error on it
	var forEachK = function(a, f, f_error, k) {
		var forEachHelp = function(i) {
			if( i >= a.length ) {
				if (k) { k(); }
				return;
			}

			try {
				f(a[i], function() { forEachHelp(i+1); });
			} catch (e) {
				f_error(e);
			}
		};
		forEachHelp(0);
	};


	// reportError: (or exception string) -> void
	// Reports an error to the user, either at the console
	// if the console exists, or as alerts otherwise.
	var reportError = function(e) {
		var reporter;
		if (typeof(console) != 'undefined' && 
			typeof(console.log) != 'undefined') {
			reporter = (function(x) { console.log(x); });
		} else {
			reporter = (function(x) { alert(x); });
		}
		if (typeof e == 'string') {
			reporter(e);
		} else if ( types.isSchemeError(e) ) {
			if ( types.isExn(e.val) ) {
				reporter( types.exnMessage(e.val) );
			}
			else {
				reporter(e.val);
			}
		} else if (e.message) {
			reporter(e.message);
		} else {
			reporter(e.toString());
		}
//		if (plt.Kernel.lastLoc) {
//			var loc = plt.Kernel.lastLoc;
//			if (typeof(loc) === 'string') {
//			reporter("Error was raised around " + loc);
//			} else if (typeof(loc) !== 'undefined' &&
//				   typeof(loc.line) !== 'undefined') {
//			reporter("Error was raised around: "
//				 + plt.Kernel.locToString(loc));
//			}
//		}
	};


	var raise = function(v) {
		throw types.schemeError(v);
	};


	var procArityContains = function(n) {
		return function(proc) {
			var singleCase = function(aCase) {
				if ( aCase instanceof types.ContinuationClosureValue ) {
					return true;
				}
				return (aCase.numParams == n ||
					(aCase.isRest && aCase.numParams <= n));
			};

			var cases = [];
			if ( proc instanceof types.ContinuationClosureValue ||
			     proc instanceof types.ClosureValue ||
			     proc instanceof types.PrimProc ) {
				return singleCase(proc);
			}
			else if (proc instanceof types.CasePrimitive) {
				cases = proc.cases;
			}
			else if (proc instanceof types.CaseLambdaValue) {
				cases = proc.closures;
			}

			for (var i = 0; i < cases.length; i++) {
				if ( singleCase(cases[i]) )
					return true;
			}
			return false;
		}
	};

	var throwCheckError = function(details, otherArgs) {
		var otherArgsStr = '';
		if (otherArgs && otherArgs.length > 0) {
			otherArgsStr = '; other arguments were:';
			for (var i = 0; i < otherArgs.length; i++) {
				otherArgsStr += ' ' + types.toWrittenString(otherArgs[i]);
			}
		}
		details.push(otherArgsStr);

		var errorFormatStr = "~a: expects type <~a> as ~a argument, given: ~s~a";
		raise( types.incompleteExn(types.exnFailContract,
					   helpers.format(errorFormatStr, details),
					   []) );
	};

	var check = function(x, f, functionName, typeName, position, otherArgs) {
		if ( !f(x) ) {
			throwCheckError([functionName,
					 typeName,
					 helpers.ordinalize(position),
					 x],
					otherArgs);
		}
	};


//	// remove: array any -> array
//	// removes the first instance of v in a
//	// or returns a copy of a if v does not exist
//	var remove = function(a, v) {
//		for (var i = 0; i < a.length; i++) {
//			if (a[i] === v) {
//				return a.slice(0, i).concat( a.slice(i+1, a.length) );
//			}
//		}
//		return a.slice(0);
//	};

	// map: array (any -> any) -> array
	// applies f to each element of a and returns the result
	// as a new array
	var map = function(a, f) {
		var b = new Array(a.length);
		for (var i = 0; i < a.length; i++) {
			b[i] = f(a[i]);
		}
		return b;
	};


	// deepListToArray: any -> any
	// Converts list structure to array structure.
	var deepListToArray = function(x) {
		var thing = x;
		if (thing === types.EMPTY) {
			return [];
		} else if (types.isPair(thing)) {
			var result = [];
			while (!thing.isEmpty()) {
				result.push(deepListToArray(thing.first()));
				thing = thing.rest();
			}
			return result;
		} else {
			return x;
		}
	}


	// assocListToHash: (listof (list X Y)) -> (hashof X Y)
	var assocListToHash = function(lst) {
		var result = {};
		while ( !lst.isEmpty() ) {
			var key = lst.first().first();
			var val = lst.first().rest().first();
			result[key] = val;
			lst = lst.rest();
		}
		return result;
	};


	var ordinalize = function(n) {
		var res = n;
		switch( n % 10 ) {
			case 1: res += 'st'; break;
			case 2: res += 'nd'; break;
			case 3: res += 'rd'; break;
			default: res += 'th'; break;
		}
		return res;
	}


	var wrapJsObject = function(x) {
		if (x === undefined || x === null) {
			return types.jsObject('undefined', x);
		}
		else if (typeof(x) == 'function') {
			return types.jsObject('function', x);
		}
		else if ( x instanceof Array ) {
			return types.jsObject('array', x);
		}
		else if ( typeof(x) == 'string' ) {
			return types.jsObject("'" + x.toString() + "'", x);
		}
		else {
			return types.jsObject(x.toString(), x);
		}
	};





	////////////////////////////////////////////////

	helpers.format = format;
	helpers.forEachK = forEachK;
	helpers.reportError = reportError;
	helpers.raise = raise;

	helpers.procArityContains = procArityContains;
	helpers.throwCheckError = throwCheckError;
	helpers.check = check;
	
//	helpers.remove = remove;
	helpers.map = map;
	helpers.deepListToArray = deepListToArray;
	helpers.assocListToHash = assocListToHash;

	helpers.ordinalize = ordinalize;
	helpers.wrapJsObject = wrapJsObject;

})();

/////////////////////////////////////////////////////////////////

