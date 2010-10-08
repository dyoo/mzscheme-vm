/********************************
 *** Scheme -> Javascript FFI ***
 ********************************/

/**

WARNING WARNING

This code won't work yet and is in the middle of being modularized.

This code was just lifted out of primitives.js, and so there are
unbound references everywhere.

*/


EXPORTS['scheme->prim-js'] =
    new PrimProc('scheme->prim-js',
		 1,
		 false, false,
		 function(x) {
		 	check(x, function(y) { return ( isReal(y) ||
							isString(y) ||
							isSymbol(y) ||
							isChar(y) ||
							isBoolean(y) ) ||
							isVector(y); },
			      'scheme->prim-js', 'real number, string, symbol, char, boolean, or vector', 1);

			var returnVal;
		 	if ( isReal(x) ) {
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
			else if ( isString(x) ) {
				returnVal = x.toString();
			}
			else if ( isSymbol(x) || isChar(x) ) {
				returnVal = x.val;
			}
			else if ( isBoolean(x) ) {
				returnVal = x;
			}
			else if ( isVector(x) ) {
				returnVal = helpers.map(function(y) { return (isJsValue(y) ? y.val : y); },
							x.elts);
			}
			return helpers.wrapJsValue(returnVal);
		 });


EXPORTS['prim-js->scheme'] =
    new PrimProc('prim-js->scheme',
		 1,
		 false, false,
		 function(x) {
		 	check(x, function(y) { return isJsValue(y) &&
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
				return new PrimProc('', 0, true, false, function(args) { return x.val.apply(null, args); });
			}
			else if ( x.val instanceof Array ) {
				return types.vector( helpers.map(helpers.wrapJsValue, x.val) );
			}
		 });


EXPORTS['procedure->cps-js-fun'] =
    new PrimProc('procedure->cps-js-fun',
		 1,
		 false, true,
		 function(aState, proc) {
		 	check(proc, isFunction, 'procedure->cps-js-fun', 'procedure', 1);

			var caller = makeCaller(aState);
			aState.v = types.jsValue(proc.name + ' (cps)', function() {
				var args = helpers.map(helpers.wrapJsValue, arguments);
				var k = (args.length == 0 ? function() {} : args.shift().val);

				caller(proc, args, k, 'proc->cps-js: ' + proc.name);
			});
		 });

EXPORTS['procedure->void-js-fun'] =
    new PrimProc('procedure->void-js-fun',
		 1,
		 false, true,
		 function(aState, proc) {
		 	check(proc, isFunction, 'procedure->void-js-fun', 'procedure', 1);

			var caller = makeCaller(aState);
			aState.v = types.jsValue(proc.name + ' (void)', function() {
				var args = helpers.map(helpers.wrapJsValue, arguments);
				caller(proc, args, function() {}, 'proc->void-js: ' + proc.name);
			});
		 });


EXPORTS['js-==='] =
    new PrimProc('js-===',
		 2,
		 false, false,
		 function(v1, v2) {
		 	check(v1, isJsValue, 'js-===', 'javascript value', 1);
			check(v2, isJsValue, 'js-===', 'javascript value', 2);

			return v1.val === v2.val;
		 });


EXPORTS['js-get-global-value'] =
    new PrimProc('js-get-global-value',
		 1,
		 false, false,
		 function(name) {
		 	check(name, isString, 'js-get-global-value', 'string', 1);

			var nameStr = name.toString();
			var obj = (nameStr === 'window') ? window : window[nameStr];
			return types.jsValue(nameStr, obj);
		 });



EXPORTS['js-get-field'] =
    new PrimProc('js-get-field',
		 2,
		 true, false,
		 function(root, firstSelector, selectors) {
		 	selectors.unshift(firstSelector);
			var allArgs = [root].concat(selectors);
		 	check(root, isJsValue, 'js-get-field', 'js-value', 1, allArgs);
			arrayEach(selectors, function(x, i) { check(x, isString, 'js-get-field', 'string', i+2, allArgs); });

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
    new PrimProc('js-set-field!',
		 3,
		 false, false,
		 function(obj, field, v) {
		 	check(obj, function(x) { return isJsValue(x) && (typeof(x) == 'object' || typeof(x) == 'function'); },
			      'js-set-field!', 'javascript object or function', 1, arguments);
			check(field, isString, 'js-set-field!', 'string', 2, arguments);

			obj.val[field.toString()] = (isJsValue(v) ? v.val : types.wrappedSchemeValue(v));
			return types.VOID;
		 });


EXPORTS['js-typeof'] =
    new PrimProc('js-typeof',
		 1,
		 false, false,
		 function(v) {
		 	check(v, isJsValue, 'js-typeof', 'js-value', 1);
			return typeof(v.val);
		 });


EXPORTS['js-instanceof'] =
    new PrimProc('js-instanceof',
		 2,
		 false, false,
		 function(v, type) {
		 	check(v, isJsValue, 'js-instanceof', 'js-value', 1, arguments);
			check(type, isJsFunction, 'js-instanceof', 'javascript function', 2, arguments);

			return (v.val instanceof type.val);
		 });


EXPORTS['js-call'] =
    new PrimProc('js-call',
		 2,
		 true, false,
		 function(fun, parent, initArgs) {
		 	var allArgs = [fun, parent].concat(initArgs);
		 	check(fun, isJsFunction, 'js-call', 'javascript function', 1, allArgs);
			check(parent, function(x) { return (x === false || isJsObject(x)); },
			      'js-call', 'javascript object or false', 2, allArgs);
			
			var args = helpers.map(function(x) { return (isJsValue(x) ? x.val : x); }, initArgs);
			var thisArg = parent ? parent.val : null;
			var jsCallReturn = fun.val.apply(thisArg, args);
			if ( jsCallReturn === undefined ) {
				return types.VOID;
			}
			else {
				return helpers.wrapJsValue(jsCallReturn);
			}
		 });


EXPORTS['js-new'] =
    new PrimProc('js-new',
		 1,
		 true, false,
		 function(constructor, initArgs) {
		 	check(constructor, isJsFunction, 'js-new', 'javascript function', 1);

			var args = helpers.map(function(x) { return (isJsValue(x) ? x.val : x); }, initArgs);
			var proxy = function() {
				constructor.val.apply(this, args);
			};
			proxy.prototype = constructor.val.prototype;

			return helpers.wrapJsValue(new proxy());
		 });


EXPORTS['js-make-hash'] =
    new CasePrimitive('js-make-hash',
	[new PrimProc('js-make-hash', 0, false, false, function() { return types.jsValue('hash', {}); }),
	 new PrimProc('js-make-hash',
		      1,
		      false, false,
		      function(bindings) {
			  checkListOf(bindings, function(x) { return isAssocList(x) && isString(x.first()); },
				      'js-make-hash', '(listof string X)', 1);

			  var ret = {};
			  while ( !bindings.isEmpty() ) {
			  	var key = bindings.first().first().toString();
				var val = bindings.first().rest().first();
				ret[key] = (isJsValue(val) ? val.val : val);
				bindings = bindings.rest();
			  }
			  return types.jsValue('hash', ret);
		      }) ]);


EXPORTS['js-undefined'] = types.jsValue('undefined', undefined);
EXPORTS['js-null'] = types.jsValue('null', null);
