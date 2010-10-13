
/************************
 *** World Primitives ***
 ************************/

var PrimProc = types.PrimProc;
var CasePrimitive = types.CasePrimitive;
var makeOptionPrimitive = types.makeOptionPrimitive;
var checkListOf = helpers.checkListOf;
var procArityContains = helpers.procArityContains;
var raise = helpers.raise;
var assocListToHash = helpers.assocListToHash;




// Every world configuration function (on-tick, stop-when, ...)
// produces a WorldConfigOption instance.
var WorldConfigOption = types.Class.extend({
	init: function(name) {
	    this.name = name;	    
	},

	configure: function(config) {
	    throw types.internalError("unimplemented", false);
	},

	toDomNode: function(cache) {
	    var div = document.createElement('div');
	    div.appendChild(document.createTextNode("(" + this.name + " ...)"));
	    return div;
	},

	toWrittenString: function(cache) {
	    return "(" + this.name + " ...)";
	},

	toDisplayedString: function(cache) {
	    return "(" + this.name + " ...)";
	}
});


var isWorldConfigOption = function(x) { return x instanceof WorldConfigOption; };



//////////////////////////////////////////////////////////////////////




EXPORTS['key=?'] =
    new PrimProc('key=?',
		 2,
		 false, false,
		 function(key1, key2) {
		 	return (key1.toString().toLowerCase() === key2.toString().toLowerCase());
		 });






var OnTickBang = WorldConfigOption.extend({
	init: function(handler, effectHandler, aDelay) {
	    this._super('on-tick');
	    this.handler = handler;
	    this.effectHandler = effectHandler;
	    this.aDelay = aDelay;
	},

	configure: function(config) {
	    var newVals = { 
		onTick: this.handler,
		onTickEffect: this.effectHandler,
		tickDelay: jsnums.toFixnum(jsnums.multiply(1000, this.aDelay))
	    };
	    return config.updateAll(newVals);
	}});




// The default tick delay is 28 times a second.
var DEFAULT_TICK_DELAY = types.rational(1, 28);

EXPORTS['on-tick'] =
	new CasePrimitive(
	    'on-tick',
	    [new PrimProc('on-tick',
			  1,
			  false, false,
			  function(f) {
			      check(f, isFunction, "on-tick", "procedure", 1);
			      return new OnTickBang(f,
						    new PrimProc('', 1, false, false,
								 function(w) { return types.effectDoNothing(); }),
						    DEFAULT_TICK_DELAY);
			  }),
	     new PrimProc('on-tick',
			  2,
			  false, false,
			  function(f, aDelay) {
			      check(f, isFunction, "on-tick", "procedure", 1, arguments);
			      check(aDelay, isNumber, "on-tick", "number", 2, arguments);
			      return new OnTickBang(f,
						    new PrimProc('', 1, false, false,
								 function(w) { return types.effectDoNothing(); }),
						    aDelay);
			  }) ]);



EXPORTS['on-tick!'] =
    new CasePrimitive('on-tick!',
	[new PrimProc('on-tick!',
		      2,
		      false, false,
		      function(handler, effectHandler) {
			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
			  return new OnTickBang(handler, effectHandler, DEFAULT_TICK_DELAY);
		      }),
	 new PrimProc('on-tick!',
		      3,
		      false, false,
		      function(handler, effectHandler, aDelay)  {
			  check(handler, isFunction, "on-tick!", "procedure", 1, arguments);
			  check(effectHandler, isFunction, "on-tick!","procedure", 2, arguments);
			  check(aDelay, isNumber, "on-tick!", "number", 3, arguments);
			  return new OnTickBang(handler, effectHandler, aDelay);
		      }) ]);



var onEvent = function(funName, inConfigName, numArgs) {
    return function(handler) {
	return onEventBang(funName, inConfigName)(handler,
						  new PrimProc('', numArgs, false, false, function() { return types.EMPTY; }));
    };
};

var onEventBang = function(funName, inConfigName) {
    return function(handler, effectHandler) {
	check(handler, isFunction, funName, 'procedure', 1, arguments);
	check(effectHandler, isFunction, funName, 'procedure', 2, arguments);
	return new (WorldConfigOption.extend({
		    init: function() {
			this._super(funName);
		    },
		    configure: function(config) {
			var newHash = {};
			newHash[inConfigName] = handler;
			newHash[inConfigName+'Effect'] = effectHandler;
			return config.updateAll(newHash);
		    }}))();
    };
};


EXPORTS['on-key'] = new PrimProc('on-key', 1, false, false, onEvent('on-key', 'onKey', 2));
EXPORTS['on-key!'] = new PrimProc('on-key!', 2, false, false, onEventBang('on-key!', 'onKey'));


EXPORTS['stop-when'] = new PrimProc('stop-when', 1, false, false,
				       onEvent('stop-when', 'stopWhen', 1));
EXPORTS['stop-when!'] = new PrimProc('stop-when!', 2, false, false,
					onEventBang('stop-when!', 'stopWhen'));


EXPORTS['on-redraw'] =
    new PrimProc('on-redraw',
		 1,
		 false, false,
		 function(f) {
		     check(f, isFunction, 'on-redraw', 'procedure', 1);
		     return new (WorldConfigOption.extend({
				 init: function() {
				     this._super('on-redraw');
				 },

				 configure: function(config) {
				     return config.updateAll({'onRedraw': f});
				 }}))();

		 });


EXPORTS['on-draw'] =
    new CasePrimitive('on-draw',
	[new PrimProc('on-draw',
		      1,
		      false, false,
		      function(domHandler) {
			  check(domHandler, isFunction, 'on-draw', 'procedure', 1);
			  return new (WorldConfigOption.extend({
				    init: function() {
					this._super('on-draw');
				    },
				    configure: function(config) {
					return config.updateAll({'onDraw': domHandler});
				    }
				}))();
		      }),
	 new PrimProc('on-draw',
		      2,
		      false, false,
		      function(domHandler, styleHandler) {
		 	  check(domHandler, isFunction, 'on-draw', 'procedure', 1, arguments);
			  check(styleHandler, isFunction, 'on-draw', 'procedure', 2, arguments);
			  return new (WorldConfigOption.extend({
				    init: function() {
					this._super('on-draw');
				    },
				    configure: function(config) {
					return config.updateAll({'onDraw': domHandler,
								 'onDrawCss': styleHandler});
				    }
				}))();
		      }) ]);


EXPORTS['initial-effect'] =
    new PrimProc('initial-effect',
		 1,
		 false, false,
		 function(effect) {
		     return new (WorldConfigOption.extend({
				 init: function() {
				     this._super("initial-effect");
				 },
				 configure: function(config) {
					return config.updateAll({'initialEffect': effect});
				 }
			     }))();
		 });



/**************************
 *** Jsworld Primitives ***
 **************************/


var jsp = function(attribList) {
	checkListOf(attribList, function(x) { return isList(x) && length(x) == 2; },
		    'js-p', 'list of (list of X Y)', 1);
	var attribs = assocListToHash(attribList);
	var node = jsworld.MobyJsworld.p(attribs);
	node.toWrittenString = function(cache) { return "(js-p)"; };
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; };
	return helpers.wrapJsValue(node);
};
EXPORTS['js-p'] =
    new CasePrimitive('js-p',
	[new PrimProc('js-p', 0, false, false, function() { return jsp(types.EMPTY); }),
	 new PrimProc('js-p', 1, false, false, jsp)]);


var jsdiv = function(attribList) {
	checkListOf(attribList, isAssocList, 'js-div', '(listof X Y)', 1);

	var attribs = assocListToHash(attribList);
	var node = jsworld.MobyJsworld.div(attribs);
	
	node.toWrittenString = function(cache) { return "(js-div)"; };
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; };
	return helpers.wrapJsValue(node);
};

EXPORTS['js-div'] =
    new CasePrimitive('js-div',
		      [new PrimProc('js-div', 0, false, false, function() {
			  var result =  jsdiv(types.EMPTY); 
			  return result;
		      }),
		       new PrimProc('js-div', 1, false, false, jsdiv)
		      ]);


var jsButtonBang = function(funName) {
	return function(worldUpdateF, effectF, attribList) {
		check(worldUpdateF, isFunction, funName, 'procedure', 1);
		check(effectF, isFunction, funName, 'procedure', 2);
		checkListOf(attribList, isAssocList, funName, '(listof X Y)', 3);

		var attribs = attribList ? assocListToHash(attribList) : {};
		var node = jsworld.MobyJsworld.buttonBang(worldUpdateF, effectF, attribs);

		node.toWrittenString = function(cache) { return '(' + funName + ' ...)'; };
		node.toDisplayedString = node.toWrittenString;
		node.toDomNode = function(cache) { return node; };
		return helpers.wrapJsValue(node);
	}
};
var jsButton = function(updateWorldF, attribList) {
	var noneF = new types.PrimProc('', 1, false, false, function(w) { return types.EMPTY; });
	return jsButtonBang('js-button')(updateWorldF, noneF, attribList);
};
EXPORTS['js-button'] =
    new CasePrimitive('js-button',
	[new PrimProc('js-button', 1, false, false, jsButton),
	 new PrimProc('js-button', 2, false, false, jsButton)]);

EXPORTS['js-button!'] =
    new CasePrimitive('js-button!',
	[new PrimProc('js-button!', 2, false, false, jsButtonBang('js-button!')),
	 new PrimProc('js-button!', 3, false, false, jsButtonBang('js-button!'))]);



var jsInput = function(type, updateF, attribList) {
	check(type, isString, 'js-input', 'string', 1);
	check(updateF, isFunction, 'js-input', 'procedure', 2);
	checkListOf(attribList, isAssocList, 'js-input', '(listof X Y)', 3);

	var attribs = attribList ? assocListToHash(attribList) : {};
	var node = jsworld.MobyJsworld.input(type, updateF, attribs);

	node.toWrittenString = function(cache) { return "(js-input ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return helpers.wrapJsValue(node);
};
EXPORTS['js-input'] =
	new CasePrimitive('js-input', 
	[new PrimProc('js-input', 2, false, false, jsInput),
	 new PrimProc('js-input', 3, false, false, jsInput)]);



var jsImg = function(src, attribList) {
	check(src, isString, "js-img", "string", 1);
	checkListOf(attribList, isAssocList, 'js-img', '(listof X Y)', 2);

	var attribs = assocListToHash(attribList);
	var node = jsworld.MobyJsworld.img(src, attribs);

	node.toWrittenString = function(cache) { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return helpers.wrapJsValue(node);
};
EXPORTS['js-img'] =
    new CasePrimitive('js-img',
	[new PrimProc('js-img', 1, false, false, function(src) { return jsImg(src, types.EMPTY); }),
	 new PrimProc('js-img', 2, false, false, jsImg)]);



EXPORTS['js-text'] =
    new PrimProc('js-text',
		 1,
		 false, false,
		 function(s) {
		 	check(s, isString, 'js-text', 'string', 1);

			var node = jsworld.MobyJsworld.text(s, []);
			node.toWrittenString = function(cache) { return "(js-text ...)"; }
			node.toDisplayedString = node.toWrittenString;
			node.toDomNode = function(cache) { return node; }
			return helpers.wrapJsValue(node);
		 });


var jsSelect = function(optionList, updateF, attribList) {
	checkListOf(optionList, isString, 'js-select', 'listof string', 1);
	check(updateF, isFunction, 'js-select', 'procedure', 2);
	checkListOf(attribList, isAssocList, 'js-select', '(listof X Y)', 3);

	var attribs = attribList ? assocListToHash(attribList) : {};
	var options = helpers.deepListToArray(optionList);
	var node = jsworld.MobyJsworld.select(options, updateF, attribs);

	node.toWrittenString = function(cache) { return '(js-select ...)'; };
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; };
	return helpers.wrapJsValue(node);
};
EXPORTS['js-select'] =
    new CasePrimitive('js-select',
	[new PrimProc('js-select', 2, false, false, jsSelect),
	 new PrimProc('js-select', 3, false, false, jsSelect)]);




EXPORTS['js-big-bang'] =
    new PrimProc('js-big-bang',
		 1,
		 true, true,
		 function(state, initW, handlers) {
		 	arrayEach(handlers,
				function(x, i) {
					check(x, function(y) { return isWorldConfigOption(y) || isList(y) || types.isWorldConfig(y); },
					      'js-big-bang', 'handler or attribute list', i+2);
				});
		     var unwrappedConfigs = 
			 helpers.map(function(x) {
					if ( isWorldConfigOption(x) ) {
						return function(config) { return x.configure(config); };
					}
					else {
						return x;
					}
			 	     },
				     handlers);
		     return types.internalPause(function(caller, restarter, onFail) {
			 var bigBangController;
			 var onBreak = function() {
			     bigBangController.breaker();
			 }
			 state.addBreakRequestedListener(onBreak);
			 bigBangController = jsworld.MobyJsworld.bigBang(initW, 
						     state.getToplevelNodeHook()(),
						     unwrappedConfigs,
						     caller, 
						     function(v) {
							 state.removeBreakRequestedListener(onBreak);
							 restarter(v);
						     });
		     })
		 });


EXPORTS['async-js-big-bang'] =
    new PrimProc('async-js-big-bang',
		 1,
		 true, true,
		 function(state, initW, handlers) {
		 	arrayEach(handlers,
				function(x, i) {
					check(x, function(y) { return isWorldConfigOption(y) || isList(y) || types.isWorldConfig(y); },
					      'js-big-bang', 'handler or attribute list', i+2);
				});
		     var unwrappedConfigs = 
			 helpers.map(function(x) {
					if ( isWorldConfigOption(x) ) {
						return function(config) { return x.configure(config); };
					}
					else {
						return x;
					}
			 	     },
				     handlers);
		     return types.internalPause(function(caller, restarter, onFail) {
			 var bigBangController;
			 var onBreak = function() {
			     bigBangController.breaker();
			 }
			 state.addBreakRequestedListener(onBreak);
			 bigBangController = jsworld.MobyJsworld.bigBang(initW, 
						     state.getToplevelNodeHook()(),
						     unwrappedConfigs,
						     caller, 
						     function(v) {
							 state.removeBreakRequestedListener(onBreak);
						     });
			 restarter(bigBangController);
		     })
		 });




//////////////////////////////////////////////////////////////////////


var emptyPage = function(attribList) {
	checkListOf(attribList, isAssocList, 'empty-page', '(listof X Y)', 1);

	var attribs = assocListToHash(attribList);
	var node = jsworld.MobyJsworld.emptyPage(attribs);
	
// 	node.toWrittenString = function(cache) { return "(js-div)"; };
// 	node.toDisplayedString = node.toWrittenString;
// 	node.toDomNode = function(cache) { return node; };
// 	return helpers.wrapJsValue(node);
	return node;
    };

    EXPORTS['empty-page'] =
	new CasePrimitive('empty-page',
			  [new PrimProc('empty-page', 0, false, false, 
					function() {  return emptyPage(types.EMPTY); }),
			   new PrimProc('empty-page', 1, false, false, emptyPage)]);

    
    EXPORTS['place-on-page'] = 
	new PrimProc('empty-page',
		     4,
		     false, false,
		     function(elt, left, top, page) {
			 // FIXME: add type checking
			 return jsworld.MobyJsworld.placeOnPage(
			     elt, left, top, page);
		     });
					    




//////////////////////////////////////////////////////////////////////





EXPORTS['make-world-config'] =
    new PrimProc('make-world-config',
		 2,
		 true, false,
		 function(startup, shutdown, handlers) {
		 	var allArgs = [startup, shutdown].concat(handlers);
		 	check(startup, isFunction, 'make-world-config', 'procedure', 1, allArgs);
			check(shutdown, procArityContains(1), 'make-world-config', 'procedure (arity 1)', 2, allArgs);
			arrayEach(handlers, function(x, i) { check(x, isFunction, 'make-world-config', 'handler', i+3, allArgs); });

			if ( !procArityContains(handlers.length)(startup) ) {
				raise( types.incompleteExn(
					types.exnFailContract,
					'make-world-config: 1st argument must have arity equal to '
					+ 'the number of arguments after the second',
					[]) );
			}

			return types.worldConfig(startup, shutdown, handlers);
		 });


EXPORTS['make-effect-type'] =
	makeOptionPrimitive(
	    'make-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-effect-type', 'procedure', 4, userArgs);
//		checkListOf(handlerIndices, isNatural, 'make-effect-type', 'exact non-negative integer', 5);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-effect-type', 'procedure or #f', 6, userArgs);
		// Check the number of arguments on the guard
		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		if ( guard && !procArityContains(numberOfGuardArgs)(guard) ) {
			raise(types.incompleteExn(
				types.exnFailContract,
				helpers.format(
					'make-effect-type: guard procedure does not accept ~a arguments '
					+ '(one more than the number constructor arguments): ~s',
					[numberOfGuardArgs, guard]),
				[]));
		}

//		var jsImpl = schemeProcToJs(aState, impl);
		var jsGuard = (guard ? schemeProcToJs(aState, guard) : false);
//		var handlerIndices_js = helpers.map(jsnums.toFixnum, helpers.schemeListToArray(handlerIndices));

//		var caller = makeCaller(aState);
//		var wrapHandler = function(handler, changeWorld) {
//			return types.jsObject('function', function() {
//				var externalArgs = arguments;
//				changeWorld(function(w, k) {
//					var args = [w];
//					for (var i = 0; i < externalArgs.length; i++) {
//						args.push( helpers.wrapJsValue(externalArgs[i]) );
//					}
//					caller(handler, args, k);
//				});
//			});
//		}

		var anEffectType = types.makeEffectType(name.toString(),
							superType,
							fieldCnt,
							impl,
//							handlerIndices_js,
							jsGuard,
							makeCaller(aState));
		aState.v = getMakeStructTypeReturns(anEffectType);
	    });


EXPORTS['effect-type?'] = new PrimProc('effect-type?', 1, false, false, types.isEffectType);
EXPORTS['effect?'] = new PrimProc('effect?', 1, false, false, types.isEffect);

//EXPORTS['make-effect:do-nothing'] = new PrimProc('make-effect:do-nothing', 0, false, false, types.EffectDoNothing.constructor);
//EXPORTS['effect:do-nothing?'] = new PrimProc('effect:do-nothing?', 1, false, false, types.EffectDoNothing.predicate);


EXPORTS['make-render-effect-type'] =
	makeOptionPrimitive(
	    'make-render-effect-type',
	    4,
	    [false],
	    true,
	    function(userArgs, aState, name, superType, fieldCnt, impl, guard) {
		check(name, isSymbol, 'make-render-effect-type', 'string', 1, userArgs);
		check(superType, function(x) { return x === false || types.isEffectType(x) },
		      'make-render-effect-type', 'effect type or #f', 2, userArgs);
		check(fieldCnt, isNatural, 'make-render-effect-type', 'exact non-negative integer', 3, userArgs);
		check(impl, isFunction, 'make-render-effect-type', 'procedure', 4, userArgs);
		check(guard, function(x) { return x === false || isFunction(x); }, 'make-render-effect-type', 'procedure or #f', 6, userArgs);
		// Check the number of arguments on the guard
		var numberOfGuardArgs = fieldCnt + 1 + (superType ? superType.numberOfArgs : 0);
		if ( guard && !procArityContains(numberOfGuardArgs)(guard) ) {
			raise(types.incompleteExn(
				types.exnFailContract,
				helpers.format(
					'make-effect-type: guard procedure does not accept ~a arguments '
					+ '(one more than the number constructor arguments): ~s',
					[numberOfGuardArgs, guard]),
				[]));
		}
		var jsGuard = (guard ? schemeProcToJs(aState, guard) : false);

		var aRenderEffectType = types.makeRenderEffectType(name.toString(),
								   superType,
								   fieldCnt,
								   impl,
								   jsGuard);
		aState.v = getMakeStructTypeReturns(aRenderEffectType);
	    });


EXPORTS['render-effect-type?'] = new PrimProc('render-effect-type?', 1, false, false, types.isRenderEffectType);
EXPORTS['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);


EXPORTS['world-with-effects'] =
    new PrimProc('world-with-effects',
		 2,
		 false, false,
		 function(effects, w) {
		 	check(effects, isCompoundEffect, 'world-with-effects', 'compound effect', 1, arguments);

			return jsworld.Jsworld.with_multiple_effects(w, helpers.flattenSchemeListToArray(effects));
		 });



EXPORTS['make-render-effect'] = new PrimProc('make-render-effect', 2, false, false, types.makeRenderEffect);

EXPORTS['render-effect?'] = new PrimProc('render-effect?', 1, false, false, types.isRenderEffect);

EXPORTS['render-effect-dom-node'] =
    new PrimProc('render-effect-dom-node',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-dom-node', 'render-effect', 1);
			return types.renderEffectDomNode(effect);
		 });

EXPORTS['render-effect-effects'] =
    new PrimProc('render-effect-effects',
		 1,
		 false, false,
		 function(effect) {
		 	check(effect, types.isRenderEffect, 'render-effect-effects', 'render-effect', 1);
			return types.renderEffectEffects(effect);
		 });


















//////////////////////////////////////////////////////////////////////

// Helper Functions








var checkList = function(x, functionName, position, args) {
	if ( !isList(x) ) {
		helpers.throwCheckError([functionName,
					 'list',
					 helpers.ordinalize(position),
					 x],
					position,
					args);
	}
}


var length = function(lst) {
	checkList(lst, 'length', 1, [lst]);
	var ret = 0;
	for (; !lst.isEmpty(); lst = lst.rest()) {
		ret = ret+1;
	}
	return ret;
}

















var getMakeStructTypeReturns = function(aStructType) {
	var name = aStructType.name;
	return new types.ValuesWrapper(
		[aStructType,
		 (new StructConstructorProc(name,
					    'make-'+name,
					    aStructType.numberOfArgs,
					    false,
					    false,
					    aStructType.constructor)),
		 (new StructPredicateProc(name, name+'?', 1, false, false, aStructType.predicate)),
		 (new StructAccessorProc(name,
					 name+'-ref',
					 2,
					 false,
					 false,
					 function(x, i) {
						check(x, aStructType.predicate, name+'-ref', 'struct:'+name, 1, arguments);
						check(i, isNatural, name+'-ref', 'non-negative exact integer', 2, arguments);

						var numFields = aStructType.numberOfFields;
						if ( jsnums.greaterThanOrEqual(i, numFields) ) {
							var msg = (name+'-ref: slot index for <struct:'+name+'> not in ' +
								   '[0, ' + (numFields-1) + ']: ' + i);
							raise( types.incompleteExn(types.exnFailContract, msg, []) );
						}
						return aStructType.accessor(x, jsnums.toFixnum(i));
					 })),
		 (new StructMutatorProc(name,
					name+'-set!',
					3,
					false,
					false,
					function(x, i, v) {
						check(x, aStructType.predicate, name+'-set!', 'struct:'+name, 1, arguments);
						check(i, isNatural, name+'-set!', 'non-negative exact integer', 2, arguments);

						var numFields = aStructType.numberOfFields;
						if ( jsnums.greaterThanOrEqual(i, numFields) ) {
							var msg = (name+'-set!: slot index for <struct'+name+'> not in ' +
								   '[0, ' + (numFields-1) + ']: ' + i);
							raise( types.incompleteExn(types.exnFailContract, msg, []) );
						}
						aStructType.mutator(x, jsnums.toFixnum(i), v)
					})) ]);
};




//////////////////////////////////////////////////////////////////////


var isNumber = jsnums.isSchemeNumber;
var isReal = jsnums.isReal;
var isRational = jsnums.isRational;
var isComplex = isNumber;
var isInteger = jsnums.isInteger;

var isNatural = function(x) {
	return jsnums.isExact(x) && isInteger(x) && jsnums.greaterThanOrEqual(x, 0);
};

var isNonNegativeReal = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
};

var isSymbol = types.isSymbol;
var isChar = types.isChar;
var isString = types.isString;
var isPair = types.isPair;
var isEmpty = function(x) { return x === types.EMPTY; };
var isList = helpers.isList;
var isListOf = helpers.isListOf;

var isVector = types.isVector;
var isBox = types.isBox;
var isHash = types.isHash;
var isByteString = types.isByteString;

var isByte = function(x) {
	return (isNatural(x) &&
		jsnums.lessThanOrEqual(x, 255));
}

var isBoolean = function(x) {
	return (x === true || x === false);
}

var isFunction = types.isFunction;

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






var isStyle = function(x) {
	return ((isString(x) || isSymbol(x)) &&
		(x.toString().toLowerCase() == "solid" ||
		 x.toString().toLowerCase() == "outline"));
};


var isAssocList = function(x) {
	return isPair(x) && isPair(x.rest()) && isEmpty(x.rest().rest());
};


var isCompoundEffect = function(x) {
	return ( types.isEffect(x) || isListOf(x, isCompoundEffect) );
};

var isJsValue = types.isJsValue;
var isJsFunction = function(x) {
    return isJsValue(x) && typeof(x.unbox()) == 'function';
};



var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.call(null, arr[i], i);
	}
}

//var throwCheckError = helpers.throwCheckError;
var check = helpers.check;


