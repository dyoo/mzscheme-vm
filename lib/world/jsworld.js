
// Depends on world.js, world-config.js

(function() {

    var Jsworld = jsworld.MobyJsworld = {};

    // The real low-level jsworld module:
    var _js = jsworld.Jsworld;


    var caller;
    var unsetCaller = function() {
    	caller = new function() {
		throw new Error('caller not yet defined!');
	};
    }
    







    // isHandler: X -> boolean
    // Right now, a handler is a function that consumes and produces
    // configs.  We should tighten up the type check eventually.
    var isHandler = function(x) {
	return typeof(x) == 'function';
    }




    //////////////////////////////////////////////////////////////////////
    //From this point forward, we define wrappers to integrate jsworld
    //with Moby.


    // deepListToArray: any -> any
    // Converts list structure to array structure.
    var deepListToArray = function(x) {
	var thing = x;
	if (types.isEmpty(thing)) {
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

    // assocListToAssocArray: (listof (list X Y)) -> (hashof X Y)
    var assocListToAssocArray = function(aList) {
	var result = {};
	while (! aList.isEmpty()) {
	    var key = aList.first().first();
	    var val = aList.first().rest().first();
	    result[key] = val;
	    aList = aList.rest();
	}
	return result;
    }


    // getBigBangWindow: -> window
    var getBigBangWindow = function() {
        if (window.document.getElementById("jsworld-div") !== undefined) {
	    return window;
	} else {
	    var newDiv = window.document.createElement("div");
	    newDiv.id = 'jsworld-div';
	    window.document.appendChild(newDiv);
	    return window;
	}
    }


    // types are
    // sexp: (cons node (listof sexp))
    // css-style: (node (listof (list string string)))

    // Exports:



    // FIXME: document how we may want to create and destroy toplevel
    // nodes on bigBang.
    Jsworld.makeToplevelNode = function() {
	return getBigBangWindow().document.getElementById("jsworld-div");
    };



    var isList = function(x) {
	return ( types.isPair(x) || types.isEmpty(x) );
    }




    // The default printWorldHook will write the written content of the node.
    // We probably want to invoke the pretty printer here instead!
    Jsworld.printWorldHook = function(world, node) {
	var newNode;
	if(node.lastChild == null) {
	    newNode = types.toDomNode(world);
	    node.appendChild(newNode);
	} else {
	    newNode = types.toDomNode(world);
	    node.replaceChild(newNode, node.lastChild);
	}
    };



    // Figure out the target of an event.
    // http://www.quirksmode.org/js/events_properties.html#target
    var findEventTarget = function(e) {
	var targ;
	if (e.target) 
	    targ = e.target;
	else if (e.srcElement) 
	    targ = e.srcElement;
	if (targ.nodeType == 3) // defeat Safari bug
	    targ = targ.parentNode;
	return targ;
    }

    // isNode: any -> boolean
    // Returns true if the thing has a nodeType.
    var isNode = function(thing) {
	return typeof(thing.nodeType) != 'undefined';
    }



    // checkWellFormedDomTree: X X (or number undefined) -> void
    // Check to see if the tree is well formed.  If it isn't,
    // we need to raise a meaningful error so the user can repair
    // the structure.
    //
    // Invariants:
    // The dom tree must be a pair.
    // The first element must be a node.
    // Each of the rest of the elements must be dom trees.
    // If the first element is a text node, it must NOT have children.
    var checkWellFormedDomTree = function(x, top, index) {
	if (types.isPair(x)) {
	    var firstElt = x.first();
	    var restElts = x.rest();

	    if (! isNode(firstElt)) {
		plt.Kernel.throwTypeError("on-draw",
					  1,
					  "dom element",
					  firstElt)
// 		throw new MobyTypeError(
// 		    plt.Kernel.format(
// 		         "on-draw: expected a dom-element, but received ~s instead, the first element within ~s",
// 			 [firstElt, top]));
	    }

	    if (firstElt.nodeType == Node.TEXT_NODE && !restElts.isEmpty() ) {
		plt.Kernel.throwTypeError("on-draw",
					  1,
					  "text node without children",
					  firstElt);
// 		throw new MobyTypeError(
// 		    plt.Kernel.format(
// 			"on-draw: the text node ~s must not have children.  It has ~s", 
// 			[firstElt, restElts]));
	    }

	    var i = 2;
	    while( !restElts.isEmpty() ) {
		checkWellFormedDomTree(restElts.first(), x, i);
		restElts = restElts.rest();
		i++;
	    }
	} else {
	    plt.Kernel.throwTypeError("on-draw",
				      1,
				      "dom s-expression",
				      x);
//	    throw new MobyTypeError(
// 		plt.Kernel.format(
// 		    "on-draw: expected a dom-s-expression, but received ~s instead~a",
// 		    [x,
// 		     (index != undefined ? 
// 		      plt.Kernel.format(", the ~a element within ~s.", [plt.Kernel.ordinalize(index), top])
// 		      : 
// 		      ".")]));
	}
    };


    // Compatibility for attaching events to nodes.
    var attachEvent = function(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.addEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.attachEvent('on' + eventName, fn, false);
	}
    };

    var detachEvent = function(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.removeEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.detachEvent('on' + eventName, fn, false);
	}
    }

//    var attachEvent = function(node, eventName, fn) {
//	plt.Kernel.attachEvent(node, eventName, fn);
//    }

    var preventDefault = function(event) {
	if (event.preventDefault) {
	    event.preventDefault();
	} else {
	    event.returnValue = false;
	}
    }

    var stopPropagation = function(event) {
	if (event.stopPropagation) {
	    event.stopPropagation();
	} else {
	    event.cancelBubble = true;
	}
    }


    // bigBang: world (listof (list string string)) (listof handler) -> world
    Jsworld.bigBang = function(initWorld, handlers, restarter, theCaller) {
	caller = theCaller;
	var attribs = types.EMPTY;
//	plt.Kernel.arrayEach(handlers,
//			     function(x, i) {
//				 plt.Kernel.check(x, function(x) { 
//				     return isHandler(x) || isList(x) },
//						  "js-big-bang", 
//						  "handler or attribute list",
//						  i+2) });
	var toplevelNode = Jsworld.makeToplevelNode();
	
	// Ensure that the toplevelNode can be focused by mouse or keyboard
	toplevelNode.tabIndex = 0;
	// Absorb all click events so they don't bubble up.
	attachEvent(toplevelNode,
		    'click',
		    function(e) {
			preventDefault(e);
			stopPropagation(e);
			return false;
		    },
		    false);
	

	var config = new world.config.WorldConfig();
	for(var i = 0; i < handlers.length; i++) {
	    if (isList(handlers[i])) {
		attribs = handlers[i];
	    } else if (isHandler(handlers[i])) {
		caller(handlers[i], config, function(result) { config = result; });
	    }
	}
	config = config.updateAll({'changeWorld': Jsworld.updateWorld,
				   'shutdownWorld': Jsworld.shutdownWorld});
	var stimuli = new world.stimuli.StimuliHandler(config, caller);
	
	var wrappedHandlers = [];
	var wrappedRedraw;
	var wrappedRedrawCss;
	

	if (config.lookup('onDraw')) {
	    wrappedRedraw = function(w, k) {
		try {
		    caller(config.lookup('onDraw'), [[w]],
			    function(newDomTree) {
				plt.Kernel.setLastLoc(undefined);
				checkWellFormedDomTree(newDomTree, newDomTree, undefined);
				var result = [toplevelNode, 
					      deepListToArray(newDomTree)];
				k(result);
			    });
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }

	    wrappedRedrawCss = function(w, k) {
		try {
		    caller(config.lookup('onDrawCss'), [[w]],
			    function(res) {
				var result = deepListToArray(res);
				plt.Kernel.setLastLoc(undefined);
				k(result);
			    });
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }
	    wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	} else if (config.lookup('onRedraw')) {
	    var reusableCanvas = undefined;
	    var reusableCanvasNode = undefined;
	    
	    wrappedRedraw = function(w, k) {
		try {
		    caller(config.lookup('onRedraw'), [[w]],
			    function(aScene) {
				// Performance hack: if we're using onRedraw, we know
				// we've got a scene, so we optimize away the repeated
				// construction of a canvas object.
				if ( world.Kernel.isImage(aScene) ) {
					var width = aScene.getWidth();
					var height = aScene.getHeight();

					if (! reusableCanvas) {
						reusableCanvas = world.Kernel.makeCanvas(width, height);
						// Note: the canvas object may itself manage objects,
						// as in the case of an excanvas.  In that case, we must make
						// sure jsworld doesn't try to disrupt its contents!
						reusableCanvas.jsworldOpaque = true;
						reusableCanvasNode = _js.node_to_tree(reusableCanvas);
					}

					reusableCanvas.width = width;
					reusableCanvas.height = height;			
					var ctx = reusableCanvas.getContext("2d");
					aScene.render(ctx, 0, 0);

					k([toplevelNode, reusableCanvasNode]);
				} else {
					k([toplevelNode, _js.node_to_tree(types.toDomNode(aScene))]);
				}
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }
	    
	    wrappedRedrawCss = function(w, k) {
		k([[reusableCanvas, 
		    ["width", reusableCanvas.width + "px"],
		    ["height", reusableCanvas.height + "px"]]]);
	    }
	    wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	} else {
	    wrappedHandlers.push(_js.on_world_change
				 (function(w, oldW, k) { 
				     Jsworld.printWorldHook(w, toplevelNode);
				     k();
				 }));
	}


	if (config.lookup('tickDelay')) {
	    var wrappedTick = function(w, k) {
		setTimeout(function() {
		    stimuli.onTick();
		},
			   0);
		k(w);
	    }
	    var wrappedDelay = jsnums.toFixnum( config.lookup('tickDelay') );
	    wrappedHandlers.push(_js.on_tick(wrappedDelay, wrappedTick));
	}

	if (config.lookup('stopWhen')) {
	    wrappedHandlers.push(_js.stop_when(function(w, k) { 
			caller(config.lookup('stopWhen'), [[w]],
				function(res) { k(res); });
		    }));
	}
	

	if (config.lookup('onKey')) {
	    // Add event handlers that listen in on key events that are applied
	    // directly on the toplevelNode.  We pay attention to keydown, and
	    // omit keypress.
	    attachEvent(toplevelNode,
			'keydown',
			function(e) {
			    stimuli.onKey(e);
			    preventDefault(e);
			    stopPropagation(e);
			    return false;
			});
	    attachEvent(toplevelNode,
			'keypress',
			function(e) {
			    preventDefault(e);
			    stopPropagation(e);
			    return false;
			});
	    toplevelNode.focus();
	}


// 	if (config.lookup('initialEffect')) {
// 	    var updaters =
// 		world.Kernel.applyEffect(config.lookup('initialEffect'));
// 	    for (var i = 0 ; i < updaters.length; i++) {
// 		if (config.lookup('stopWhen') && 
// 		    config.lookup('stopWhen')([initWorld])) {
// 		    break;
// 		} else {
// 		    initWorld = updaters[i](initWorld);
// 		}
// 	    }
// 	}
	

	
	_js.big_bang(toplevelNode,
		     initWorld,
		     wrappedHandlers,
		     assocListToAssocArray(attribs),
		     function(w) {
			 unsetCaller();
			 restarter(w);
		     });
    }



    var handleError = function(e) {
	plt.Kernel.reportError(e);
	// When something bad happens, shut down 
	// the world computation.
	plt.Kernel.reportError("Shutting down jsworld computations");
	world.stimuli.onShutdown(); 
    }
    


    // updateWorld: CPS( CPS(world -> world) -> void )
    Jsworld.updateWorld = function(updater, k) {
	var wrappedUpdater = function(world, k2) {
	    try {
		updater(world, k2);
	    } catch (e) {
		handleError(e);
		k2(world);
	    }
	}

	_js.change_world(wrappedUpdater, k);
    }
    


    // shutdownWorld: -> void
    // Shut down all world computations.
    Jsworld.shutdownWorld = function() {
	_js.shutdown();
    };


    var getAttribs = function(args) {
	if (args.length == 0) {
	    return []
	}
	if (args.length == 1) {
	    return assocListToAssocArray(args[0]);
	} else {
	    throw new Error("getAttribs recevied unexpected value for args: "
			    + args);
	}
    }



    // p: assoc -> node
    Jsworld.p = function(args) {
	var attribs = getAttribs(args);
	var node = _js.p(attribs);
	node.toWrittenString = function(cache) { 
	    return "(js-p)";
	};
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };

    // div: assoc -> node
    Jsworld.div = function(args) {
	var attribs = getAttribs(args);
	var node = _js.div(attribs);
	node.toWrittenString = function(cache) { 
	    return "(js-div)";
	};
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };

    // button: (world -> world) assoc -> node
    Jsworld.button = function(f, args) {
	var effectModule = plt.Kernel.invokeModule("moby/runtime/effect-struct");
	var noneF = function(world) {
	    return effectModule.EXPORTS['make-effect:none']();
	};
	var node = Jsworld.buttonStar(f, 
				      noneF,
				      args);
	node.toWrittenString = function(cache) { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    Jsworld.buttonStar = function(worldUpdateF, effectF, args) {
	var attribs = getAttribs(args);
	var wrappedF = function(world, evt, k) {
	    try {
		caller(effectF, [[world]],
			function(effect) {
			    caller(worldUpdateF, [[world]],
				function(newWorld) {
					world.Kernel.applyEffect(effect);
					k(newWorld);
				});
			});
	    } catch (e) {
		handleError(e);
		k(world);
	    }
	}
	var node = _js.button(wrappedF, attribs);
	node.toWrittenString = function(cache) { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };
    

    // input.
    Jsworld.input = function(type, updateF, args) {
//	plt.Kernel.check(type, plt.Kernel.isString, "js-input", "string", 1);
//	plt.Kernel.check(updateF, plt.Kernel.isFunction, "js-input", "(world string -> world)", 1);
	var attribs = getAttribs(args);
	var node = _js.input(type,
			     updateF,
			     attribs);
	node.toWrittenString = function(cache) { return "(js-input ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    Jsworld.get_dash_input_dash_value = function(node) {
//	plt.Kernel.check(node, 
//			 function(x) { return (plt.Kernel.isString(node) ||
//					       node.nodeType == 
//					       Node.ELEMENT_NODE) }, 
//			 "get-input-value",
//			 "dom-node",
//			 1);
	if (types.isString(node)) {
	    return (document.getElementById(node).value || "");
	} else {
	    return (node.value || "");
	}

    };



    // Images.
    Jsworld.img = function(src, args) {
	plt.Kernel.check(src, plt.Kernel.isString, "js-img", "string", 1);
	var attribs = getAttribs(args);
	var node = _js.img(src, attribs);
	node.toWrittenString = function(cache) { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    // text: string -> node
    Jsworld.text = function(s) {
	plt.Kernel.check(s, plt.Kernel.isString, "js-text", "string", 1);
	var node = _js.text(s, []);
	node.toWrittenString = function(cache) { return "(js-text ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };



    Jsworld.select = function(options, updateF, args) { 
//	plt.Kernel.checkListof(options, plt.Kernel.isString, "js-select", "string", 1);
//	plt.Kernel.check(updateF, plt.Kernel.isFunction, "js-select", "function", 2);
	var attribs = getAttribs(args);
	// convert options to array
	var optionsArray = deepListToArray(options);
	var node = _js.select(attribs, 
			      optionsArray,
			      function(w, selectEvent, k) { 
				  updateF(w, node.value, k)
			      });
	node.toWrittenString = function(cache) { return "(js-select ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    // fixme: add support for textarea, h1, canvas


    // raw_node: scheme-value assoc -> node
    Jsworld.rawNode = function(x, args) {
	var attribs = getAttribs(args);
	var node = _js.raw_node(types.toDomNode(x), attribs);
	node.toWrittenString = function(cache) { return "(js-raw-node ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };



})();
