/********************************
 *** Racket -> Javascript FFI ***
 ********************************/

/*
Javascript values exposed that shouldn't be accessible from Racket:

     JsValue
     isJsValue
     isJsObject
     isJsFunction
     wrapJsValue

     wrappedSchemeValue
     isWrappedSchemeValue
*/



var arrayEach = function(arr, f) {
    for (var i = 0; i < arr.length; i++) {
	f.call(null, arr[i], i);
    }
}






var JsValue = function(name, val) {
	this.name = name;
	this.val = val;
};
EXPORTS['JsValue'] = JsValue;




JsValue.prototype.toString = function() {
	return '#<js-value:' + typeof(this.val) + ':' + this.name + '>';
};

JsValue.prototype.toDomNode = function(cache) {
	return toDomNode(this.val, cache);
};

JsValue.prototype.isEqual = function(other, aUnionFind) {
	return (this.val === other.val);
};


// unbox: jsvalue -> any
// Unwraps the value out of the JsValue box.
JsValue.prototype.unbox = function()  {
    return this.val;
};


var isJsValue = function(x) { 
    return x instanceof JsValue; 
};
EXPORTS['isJsValue'] = isJsValue;


var isJsObject = function(x) {
    return isJsValue(x) && typeof(x.val) == 'object';
};
EXPORTS['isJsObject'] = isJsObject;



var isJsFunction = function(x) {
    return isJsValue(x) && typeof(x.val) == 'function';
};
EXPORTS['isJsFunction'] = isJsFunction;




var wrapJsValue = function(x) {
    if (x === undefined) {
	return new JsValue('undefined', x);
    }
    else if (x === null) {
	return new JsValue('null', x);
    }
    else if (typeof(x) == 'function') {
	return new JsValue('function', x);
    }
    else if ( x instanceof Array ) {
	return new JsValue('array', x);
    }
    else if ( typeof(x) == 'string' ) {
	return new JsValue("'" + x.toString() + "'", x);
    }
    else {
	return new JsValue(x.toString(), x);
    }
};
EXPORTS['wrapJsValue'] = wrapJsValue;














var WrappedSchemeValue = function(val) {
	this.val = val;
};

WrappedSchemeValue.prototype.toString = function() { return toString(this.val); };
WrappedSchemeValue.prototype.toWrittenString = function(cache) { return toWrittenString(this.val, cache); };
WrappedSchemeValue.prototype.toDisplayedString = function(cache) { return toDisplayedString(this.val, cache); };


// unbox: jsvalue -> any
// Unwraps the value out of the WrappedSchemeValue box.
WrappedSchemeValue.prototype.unbox = function() {
    return this.val;
};


var wrappedSchemeValue =
    function(val) { return new WrappedSchemeValue(val); };
EXPORTS['wrappedSchemeValue'] = wrappedSchemeValue;



var isWrappedSchemeValue = 
    function(x) { return x instanceof WrappedSchemeValue; };
EXPORTS['isWrappedSchemeValue'] = isWrappedSchemeValue;












var isAssocList = function(x) {
    return types.isPair(x) && types.isPair(x.rest) && types.isEmpty(x.rest.rest);
};


var check = helpers.check;



var makeCaller = function(aState) {
    return function(operator, operands, onSuccess, callSite) {
	interpret.call(aState, operator, operands, onSuccess,
		       aState.onFail, callSite);
    };
};

var MIN_FIXNUM = jsnums.fromFixnum(-9e15);
var MAX_FIXNUM = jsnums.fromFixnum(9e15);












EXPORTS['minimum-js-fixnum'] = MIN_FIXNUM;
EXPORTS['maximum-js-fixnum'] = MAX_FIXNUM;



EXPORTS['racket->prim-js'] =
    new types.PrimProc('racket->prim-js',
		       1,
		       false, false,
		       function(x) {
		 	   check(x, function(y) { return ( types.isReal(y) ||
							   types.isString(y) ||
							   types.isSymbol(y) ||
							   types.isChar(y) ||
							   types.isBoolean(y) ) ||
						  types.isVector(y); },
				 'racket->prim-js', 'real number, string, symbol, char, boolean, or vector', 1);

			   var returnVal;
		 	   if ( types.isReal(x) ) {
			       if ( !( jsnums.equals(x, jsnums.nan) ||
				       jsnums.equals(x, jsnums.inf) ||
				       jsnums.equals(x, jsnums.negative_inf) ) &&
				    ( jsnums.greaterThan(x, MAX_FIXNUM) ||
				      jsnums.lessThan(x, MIN_FIXNUM) ) ) {
				   helpers.raise(types.incompleteExn(
				       types.exnFailContract,
				       helpers.format('racket->primitive-js: only numbers in [~a, ~a] '
						      + 'are accurately representable in javascript; given: ~s',
						      [MIN_FIXNUM, 
						       MAX_FIXNUM, 
						       x]),
				       []));
			       }
			       returnVal = jsnums.toFixnum(x);
			   }
			   else if ( types.isString(x) ) {
			       returnVal = x.toString();
			   }
			   else if ( types.isSymbol(x) || types.isChar(x) ) {
			       returnVal = x.val;
			   }
			   else if ( types.isBoolean(x) ) {
			       returnVal = x;
			   }
			   else if ( types.isVector(x) ) {
			       returnVal = helpers.map(function(y) { return (isJsValue(y) ? y.val : y); },
						       x.elts);
			   }
			   return wrapJsValue(returnVal);
		       });

EXPORTS['scheme->prim-js'] = EXPORTS['racket->prim-js'];


EXPORTS['prim-js->racket'] =
    new types.PrimProc('prim-js->racket',
		       1,
		       false, false,
		       function(x) {
		 	   check(x, function(y) { return isJsValue(y) &&
						  ( typeof(y.val) == 'number' ||
						    typeof(y.val) == 'string' ||
						    typeof(y.val) == 'boolean' ||
						    typeof(y.val) == 'function' ||
						    y.val instanceof Array ); },
				 'prim-js->racket', 'javascript number, string, boolean, function, or array', 1);

		 	   if ( typeof(x.val) === 'number' ) {
			       return types.float(x.val);
			   }
			   else if ( typeof(x.val) === 'string' || typeof(x.val) === 'boolean' ) {
			       return x.val;
			   }
			   else if ( typeof(x.val) === 'function' ) {
			       return new types.PrimProc('', 0, true, false, function(args) { return x.val.apply(null, args); });
			   }
			   else if ( x.val instanceof Array ) {
			       return types.vector( helpers.map(wrapJsValue, x.val) );
			   }
		       });

EXPORTS['prim-js->scheme'] = EXPORTS['prim-js->racket'];



EXPORTS['procedure->cps-js-fun'] =
    new types.PrimProc(
	'procedure->cps-js-fun',
	1,
	false, true,
	function(aState, proc) {
	    check(proc, types.isFunction, 
		  'procedure->cps-js-fun', 'procedure', 1);
	    
	    var caller = makeCaller(aState);
	    aState.v = new JsValue(
		proc.name + ' (cps)',
		function() {
		    var args = helpers.map(wrapJsValue, arguments);
		    var k = (args.length == 0 ? 
			     function() {} :
			     args.shift().val);
		    
		    caller(proc, args, k, 'proc->cps-js: ' + proc.name);
		});
	});



var makeWrappedRacketFunction = function(aState, proc) {
    var caller = makeCaller(aState);
    var closure = function() {
	var args = helpers.map(wrapJsValue,
			       arguments);
	caller(proc, args, 
	       function(v) {},
	       'proc->void-js: ' + proc.name);
    };
    // Magic: the closure has an additional field that cooperates
    // with js-call if js-call see the application.
    closure.__isRacketFunction = true;
    closure.__racketFunction = proc;
    return closure;
};



EXPORTS['procedure->void-js-fun'] =
    new types.PrimProc(
	'procedure->void-js-fun',
	1,
	false, true,
	function(aState, proc) {
	    check(proc, types.isFunction,
		  'procedure->void-js-fun', 'procedure', 1);
	    aState.v = new JsValue(
		proc.name + ' (void)',
		makeWrappedRacketFunction(aState, proc));
	});


EXPORTS['js-==='] =
    new types.PrimProc('js-===',
		       2,
		       false, false,
		       function(v1, v2) {
		 	   check(v1, isJsValue, 'js-===', 'javascript value', 1);
			   check(v2, isJsValue, 'js-===', 'javascript value', 2);

			   return v1.val === v2.val;
		       });


EXPORTS['js-get-global-value'] =
    new types.PrimProc('js-get-global-value',
		       1,
		       false, false,
		       function(name) {
		 	   check(name, types.isString, 'js-get-global-value', 'string', 1);

			   var nameStr = name.toString();
			   var obj = (nameStr === 'window') ? window : window[nameStr];
			   return new JsValue(nameStr, obj);
		       });



EXPORTS['js-get-field'] =
    new types.PrimProc('js-get-field',
		       2,
		       true, false,
		       function(root, firstSelector, selectors) {
		 	   selectors.unshift(firstSelector);
			   var allArgs = [root].concat(selectors);
		 	   check(root, isJsValue, 'js-get-field', 'js-value', 1, allArgs);
			   arrayEach(selectors, function(x, i) { 
			       check(x, types.isString, 'js-get-field', 'string', i+2, allArgs); });

			   var name = [root.name];
			   var obj = root.val;

			   var fail = function(reason) {
			       var joinedName = name.join('');
			       helpers.raise(types.incompleteExn(
				   types.exnFailContract,
				   helpers.format('js-get-field: tried to access field ~a of ~a, but the latter was ~a',
						  [selectors[i], joinedName, reason]),
				   []));
			   };

			   for (var i = 0; i < selectors.length; i++) {
			       if ( obj === undefined ) {
				   fail('undefined');
			       }
			       else if ( isWrappedSchemeValue(obj) ) {
				   fail( helpers.format('the racket value ~s', [obj.val]) );
			       }

			       name.push( '["' + selectors[i].toString() + '"]' );
			       obj = obj[selectors[i].toString()];
			   }

			   if ( isWrappedSchemeValue(obj) ) {
			       return obj.val;
			   }
			   else {
			       return new JsValue(name.join(''), obj);
			   }
		       });


EXPORTS['js-set-field!'] =
    new types.PrimProc('js-set-field!',
		       3,
		       false, false,
		       function(obj, field, v) {
		 	   check(obj, function(x) { return isJsValue(x) && (typeof(x) == 'object' || typeof(x) == 'function'); },
				 'js-set-field!', 'javascript object or function', 1, arguments);
			   check(field, types.isString, 'js-set-field!', 'string', 2, arguments);

			   obj.val[field.toString()] = (isJsValue(v) ? v.val : wrappedSchemeValue(v));
			   return types.VOID;
		       });


EXPORTS['js-typeof'] =
    new types.PrimProc('js-typeof',
		       1,
		       false, false,
		       function(v) {
		 	   check(v, isJsValue, 'js-typeof', 'js-value', 1);
			   return typeof(v.val);
		       });


EXPORTS['js-instanceof'] =
    new types.PrimProc('js-instanceof',
		       2,
		       false, false,
		       function(v, type) {
		 	   check(v, isJsValue, 'js-instanceof', 'js-value', 1, arguments);
			   check(type, isJsFunction, 'js-instanceof', 'javascript function', 2, arguments);

			   return (v.val instanceof type.val);
		       });


EXPORTS['js-call'] =
    new types.PrimProc(
	'js-call',
	2,
	true, false,
	function(fun, parent, initArgs) {
	    var allArgs = [fun, parent].concat(initArgs);
	    check(fun, isJsFunction, 
		  'js-call', 'javascript function', 1, allArgs);
	    check(parent, 
		  function(x) {
		      return (x === false || isJsObject(x)); 
		  },
		  'js-call', 'javascript object or false', 2, allArgs);
	    
// 	    if (fun.__isRacketFunction) {
// 		//console.log('here');
// 		var racketOperator = fun.__racketFunction;
// 		var args = helpers.map(
// 		    function(x) { 
// 			return (isJsValue(x) ? 
// 				x : wrapJsValue(x));
// 		    },
// 		    initArgs);
// 		return types.internalPause(
// 		    function(caller, success, fail)  {
// 			caller(proc,
// 			       args,
// 			       function(v) {
// 				   success(wrapJsValue(v));
// 			       }, 
// 			       fail);
// 		    });
// 	    } else {
		var args = helpers.map(
		    function(x) { 
			return (isJsValue(x) ? x.val : x); },
		    initArgs);
		var thisArg = parent ? parent.val : null;
		
		return types.internalPause(
		    function(caller, success, fail) {
			try {
			    var jsCallReturn = fun.val.apply(thisArg, args);
			    if ( jsCallReturn === undefined ) {
//				console.trace();
//					console.log("here");
					success(types.VOID);
			    } else {
				success(wrapJsValue(
				    jsCallReturn))
			    }
			} catch(e) {
//			    console.log("Failure");
//			    console.log(e);
			    fail(e);
			}
		});
//	    }
	});


EXPORTS['js-new'] =
    new types.PrimProc('js-new',
		       1,
		       true, false,
		       function(constructor, initArgs) {
		 	   check(constructor, isJsFunction, 'js-new', 'javascript function', 1);

			   var args = helpers.map(function(x) { return (isJsValue(x) ? x.val : x); }, initArgs);
			   var proxy = function() {
			       constructor.val.apply(this, args);
			   };
			   proxy.prototype = constructor.val.prototype;

			   return wrapJsValue(new proxy());
		       });


EXPORTS['js-make-hash'] =
    new types.CasePrimitive('js-make-hash',
			    [new types.PrimProc('js-make-hash', 0, false, false, function() { return new JsValue('hash', {}); }),
			     new types.PrimProc('js-make-hash',
						1,
						false, false,
						function(bindings) {
						    helpers.checkListOf(bindings, function(x) { return isAssocList(x) && types.isString(x.first); },
									'js-make-hash', '(listof string X)', 1);

						    var ret = {};
						    while (bindings !== types.EMPTY) {
			  				var key = bindings.first.first.toString();
							var val = bindings.first.rest.first;
							ret[key] = (isJsValue(val) ? val.val : val);
							bindings = bindings.rest;
						    }
						    return new JsValue('hash', ret);
						}) ]);


EXPORTS['js-undefined'] = new JsValue('undefined', undefined);
EXPORTS['js-null'] = new JsValue('null', null);


EXPORTS['js-undefined?'] =
    new types.PrimProc("js-undefined?", 
		       1,
		       false,
		       false, 
		       function(x) {
			   return isJsValue(x) && x.val === undefined;
		       });

EXPORTS['js-null?'] =
    new types.PrimProc("js-null?", 
		       1,
		       false,
		       false, 
		       function(x) {
			   return isJsValue(x) && x.val === null;
		       });






EXPORTS['js-value?'] = 
    new types.PrimProc('js-value?', 1, false, false, isJsValue);
EXPORTS['js-object?'] =
    new types.PrimProc('js-object?', 1, false, false, isJsObject);
EXPORTS['js-function?'] = 
    new types.PrimProc('js-function?', 1, false, false, isJsFunction);
