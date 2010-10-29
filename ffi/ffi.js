/********************************
 *** Scheme -> Javascript FFI ***
 ********************************/


var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.call(null, arr[i], i);
	}
}


var isJsObject = function(x) {
	return types.isJsValue(x) && typeof(x.val) == 'object';
};


var isJsFunction = function(x) {
	return types.isJsValue(x) && typeof(x.val) == 'function';
};


var isAssocList = function(x) {
	return isPair(x) && isPair(x.rest()) && isEmpty(x.rest().rest());
};


var check = helpers.check;



EXPORTS['scheme->prim-js'] =
    new types.PrimProc('scheme->prim-js',
		 1,
		 false, false,
		 function(x) {
		 	check(x, function(y) { return ( types.isReal(y) ||
							types.isString(y) ||
							types.isSymbol(y) ||
							types.isChar(y) ||
							types.isBoolean(y) ) ||
							types.isVector(y); },
			      'scheme->prim-js', 'real number, string, symbol, char, boolean, or vector', 1);

			var returnVal;
		 	if ( types.isReal(x) ) {
				if ( !( jsnums.equals(x, jsnums.nan) ||
					jsnums.equals(x, jsnums.inf) ||
					jsnums.equals(x, jsnums.negative_inf) ) &&
				     ( jsnums.greaterThan(x, 9e15) ||
				       jsnums.lessThan(x, -9e15) ) ) {
					raise(types.incompleteExn(
						types.exnFailContract,
						helpers.format('scheme->primitive-js: only numbers in [-9e15, 9e15] '
								+ 'are accurately representable in javascript; given: ~s',
							       [x]),
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
				returnVal = helpers.map(function(y) { return (types.isJsValue(y) ? y.val : y); },
							x.elts);
			}
			return helpers.wrapJsValue(returnVal);
		 });


EXPORTS['prim-js->scheme'] =
    new types.PrimProc('prim-js->scheme',
		 1,
		 false, false,
		 function(x) {
		 	check(x, function(y) { return types.isJsValue(y) &&
						      ( typeof(y.val) == 'number' ||
							typeof(y.val) == 'string' ||
							typeof(y.val) == 'boolean' ||
							typeof(y.val) == 'function' ||
							y.val instanceof Array ); },
			      'prim-js->scheme', 'javascript number, string, boolean, function, or array', 1);

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
				return types.vector( helpers.map(helpers.wrapJsValue, x.val) );
			}
		 });


EXPORTS['procedure->cps-js-fun'] =
    new types.PrimProc('procedure->cps-js-fun',
		 1,
		 false, true,
		 function(aState, proc) {
		 	check(proc, types.isFunction, 'procedure->cps-js-fun', 'procedure', 1);

			var caller = makeCaller(aState);
			aState.v = types.jsValue(proc.name + ' (cps)', function() {
				var args = helpers.map(helpers.wrapJsValue, arguments);
				var k = (args.length == 0 ? function() {} : args.shift().val);

				caller(proc, args, k, 'proc->cps-js: ' + proc.name);
			});
		 });

EXPORTS['procedure->void-js-fun'] =
    new types.PrimProc('procedure->void-js-fun',
		 1,
		 false, true,
		 function(aState, proc) {
		 	check(proc, types.isFunction, 'procedure->void-js-fun', 'procedure', 1);

			var caller = makeCaller(aState);
			aState.v = types.jsValue(proc.name + ' (void)', function() {
				var args = helpers.map(helpers.wrapJsValue, arguments);
				caller(proc, args, function() {}, 'proc->void-js: ' + proc.name);
			});
		 });


EXPORTS['js-==='] =
    new types.PrimProc('js-===',
		 2,
		 false, false,
		 function(v1, v2) {
		 	check(v1, types.isJsValue, 'js-===', 'javascript value', 1);
			check(v2, types.isJsValue, 'js-===', 'javascript value', 2);

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
			return types.jsValue(nameStr, obj);
		 });



EXPORTS['js-get-field'] =
    new types.PrimProc('js-get-field',
		 2,
		 true, false,
		 function(root, firstSelector, selectors) {
		 	selectors.unshift(firstSelector);
			var allArgs = [root].concat(selectors);
		 	check(root, types.isJsValue, 'js-get-field', 'js-value', 1, allArgs);
			arrayEach(selectors, function(x, i) { check(x, types.isString, 'js-get-field', 'string', i+2, allArgs); });

			var name = [root.name];
			var obj = root.val;

			var fail = function(reason) {
				var joinedName = name.join('');
				raise(types.incompleteExn(
					types.exnFailContract,
					helpers.format('js-get-field: tried to access field ~a of ~a, but the latter was ~a',
						       [selectors[i], joinedName, reason]),
					[]));
			};

			for (var i = 0; i < selectors.length; i++) {
				if ( obj === undefined ) {
					fail('undefined');
				}
				else if ( types.isWrappedSchemeValue(obj) ) {
					fail( helpers.format('the scheme value ~s', [obj.val]) );
				}

				name.push( '["' + selectors[i].toString() + '"]' );
				obj = obj[selectors[i].toString()];
			}

			if ( types.isWrappedSchemeValue(obj) ) {
				return obj.val;
			}
			else {
				return types.jsValue(name.join(''), obj);
			}
		 });


EXPORTS['js-set-field!'] =
    new types.PrimProc('js-set-field!',
		 3,
		 false, false,
		 function(obj, field, v) {
		 	check(obj, function(x) { return types.isJsValue(x) && (typeof(x) == 'object' || typeof(x) == 'function'); },
			      'js-set-field!', 'javascript object or function', 1, arguments);
			check(field, types.isString, 'js-set-field!', 'string', 2, arguments);

			obj.val[field.toString()] = (types.isJsValue(v) ? v.val : types.wrappedSchemeValue(v));
			return types.VOID;
		 });


EXPORTS['js-typeof'] =
    new types.PrimProc('js-typeof',
		 1,
		 false, false,
		 function(v) {
		 	check(v, types.isJsValue, 'js-typeof', 'js-value', 1);
			return typeof(v.val);
		 });


EXPORTS['js-instanceof'] =
    new types.PrimProc('js-instanceof',
		 2,
		 false, false,
		 function(v, type) {
		 	check(v, types.isJsValue, 'js-instanceof', 'js-value', 1, arguments);
			check(type, isJsFunction, 'js-instanceof', 'javascript function', 2, arguments);

			return (v.val instanceof type.val);
		 });


EXPORTS['js-call'] =
    new types.PrimProc('js-call',
		 2,
		 true, false,
		 function(fun, parent, initArgs) {
		 	var allArgs = [fun, parent].concat(initArgs);
		     alert('checking function:' + isJsFunction(fun));
		 	check(fun, isJsFunction, 'js-call', 'javascript function', 1, allArgs);
		     alert('checking parent');
			check(parent, function(x) { return (x === false || types.isJsObject(x)); },
			      'js-call', 'javascript object or false', 2, allArgs);
			
		     alert('jscall: unwrapping args');

			var args = helpers.map(function(x) { return (types.isJsValue(x) ? x.val : x); }, initArgs);
			var thisArg = parent ? parent.val : null;
		     alert('about to apply call');
			var jsCallReturn = fun.val.apply(thisArg, args);
			if ( jsCallReturn === undefined ) {
				return types.VOID;
			}
			else {
				return helpers.wrapJsValue(jsCallReturn);
			}
		 });


EXPORTS['js-new'] =
    new types.PrimProc('js-new',
		 1,
		 true, false,
		 function(constructor, initArgs) {
		 	check(constructor, isJsFunction, 'js-new', 'javascript function', 1);

			var args = helpers.map(function(x) { return (types.isJsValue(x) ? x.val : x); }, initArgs);
			var proxy = function() {
				constructor.val.apply(this, args);
			};
			proxy.prototype = constructor.val.prototype;

			return helpers.wrapJsValue(new proxy());
		 });


EXPORTS['js-make-hash'] =
    new types.CasePrimitive('js-make-hash',
	[new types.PrimProc('js-make-hash', 0, false, false, function() { return types.jsValue('hash', {}); }),
	 new types.PrimProc('js-make-hash',
		      1,
		      false, false,
		      function(bindings) {
			  checkListOf(bindings, function(x) { return types.isAssocList(x) && types.isString(x.first()); },
				      'js-make-hash', '(listof string X)', 1);

			  var ret = {};
			  while ( !bindings.isEmpty() ) {
			  	var key = bindings.first().first().toString();
				var val = bindings.first().rest().first();
				ret[key] = (types.isJsValue(val) ? val.val : val);
				bindings = bindings.rest();
			  }
			  return types.jsValue('hash', ret);
		      }) ]);


EXPORTS['js-undefined'] = types.jsValue('undefined', undefined);
EXPORTS['js-null'] = types.jsValue('null', null);


EXPORTS['js-undefined?'] =
    new types.PrimProc("js-undefined?", 
		       1,
		       false,
		       false, 
		       function(x) {
			   return types.isJsValue(x) && x.val === undefined;
		       });

EXPORTS['js-null?'] =
    new types.PrimProc("js-null?", 
		       1,
		       false,
		       false, 
		       function(x) {
			   return types.isJsValue(x) && x.val === null;
		       });