
var jsworld = {};

// This is a combined version of jsworld to get everything into one file

(function() {

	/* Type signature notation
	 * CPS(a b ... -> c) is used to denote
	 *   a b ... (c -> void) -> void
	 */

	var Jsworld = jsworld.Jsworld = {};

	var currentFocusedNode = false;

	var doNothing = function() {};
	var isPair = types.isPair;
	var isEmpty = function(x) { return x === types.EMPTY; };
	var isList = function(x) { return (isPair(x) || isEmpty(x)); };

	var PAUSE = types.internalPause;

	//
	// WORLD STUFFS
	//

	var InitialWorld = function() {};

	var WrappedWorldWithEffects = function(effects, w) {
		if (w instanceof WrappedWorldWithEffects) {
			this.e = w.e.concat(effects);
			this.w = w.w;
		}
		else {
			this.e = effects;
			this.w = w;
		}
	};

	WrappedWorldWithEffects.prototype.getWorld = function() {
		return this.w;
	};

	WrappedWorldWithEffects.prototype.getEffects = function() {
		return this.e;
	};

	/////////////////////////////////////////////////////////////
	// BigBang, the actual type of a big bang

	var BigBang = function(caller, restarter, toplevelNode) {
		this.world = new InitialWorld();

		this.worldListeners = [];
		this.userConfigs = [];
		this.rendertimeEventDetachers = [];

		this.lastPage = undefined;
		this.lastCss = [];

		this.toplevelNode = toplevelNode;
		this.setCaller(caller);
		this.setRestarter(restarter);
		toplevelNode.tabIndex = 0;

		this.absorber = function(e) {
			if (e.preventDefault) {
				e.preventDefault();
			} else {
				e.returnValue = false;
			}

			if (e.stopPropagation) {
				e.stopPropagation();
			} else {
				e.cancelBubble = true;
			}
			return false;
		};
		attachEvent(this.toplevelNode, 'click', this.absorber);

		var that = this;
		var schemeChangeWorld =
			new types.PrimProc('change-world', 1, false, false,
				function(updater) {
					helpers.check(updater, helpers.procArityContains(1),
						      'change-world', 'procedure (arity 1)', 1);
					return PAUSE(function(newRestarter, newCaller) {
						that.changeWorld(function(w, k) { newCaller(updater,
											    [w], k,
											    function(e) { throw e; },
											    'change-world'); },
								 function() { newRestarter(types.VOID, 'restarting (change-world)'); });
					});
				});
		this.configStartupArg = types.makeBigBangInfo( schemeChangeWorld, helpers.wrapJsObject(toplevelNode) );
	};

	BigBang.prototype.setCaller = function(c) {
		var that = this;
		this.caller = function(op, args, k, onFail, callSite) {
			if ( !onFail ) {
				onFail = function(e) { that.handleError(e); };
			}
			return c(op, args, k, onFail, callSite);
		};
	};

	BigBang.prototype.setRestarter = function(r) {
		var that = this;
		var hasRestarted = false;
		this.restarter = function(v) {
			if (hasRestarted) {
				throw types.internalError('A single big-bang cannot call restarter twice!', false);
			}
			hasRestarted = true;
			r(v);
		};
	};

	BigBang.prototype.addWorldListener = function(listener) {
		this.worldListeners.push(listener);
	};

	BigBang.prototype.addRendertimeEventDetacher = function(detacher) {
		this.rendertimeEventDetachers.push(detacher);
	};

	BigBang.prototype.rendertimeDetachEvents = function() {
		for (var i = 0; i < this.rendertimeEventDetachers.length; i++) {
			this.rendertimeEventDetachers[i]();
		}
		this.rendertimeEventDetachers = [];
	};


	BigBang.prototype.changeWorld = function(updater, k) {
		var that = this;
		var originalWorld = this.world;

		var changeWorldHelp = function() {
			if (that.world instanceof WrappedWorldWithEffects) {
				var effects = that.world.getEffects();
				that.world = that.world.getWorld();
				helpers.forEachK(effects,
						 function(anEffect, k2) { anEffect.invokeEffect(that, k2); },
						 function(e) { throw e; },
						 changeWorldHelp2);
			}
			else {
				changeWorldHelp2();
			}
		};

		var changeWorldHelp2 = function() {
			 helpers.forEachK(that.worldListeners,
					  function(listener, k2) { listener(that.world, k2); },
					  function(e) { throw e; },
					  k);
		};

		try {
			updater(this.world, function(newWorld) {
						that.world = newWorld;
						changeWorldHelp();
					    });
		} catch(e) {
			this.world = originalWorld;
			this.handleError(e);
		}
	};


	BigBang.prototype.shutdown = function(w) {
		this.world = new InitialWorld();
		this.worldListeners = [];
		this.rendertimeDetachEvents();
		detachEvent(this.toplevelNode, 'click', this.absorber);

		var that = this;
		this.shutdownUserConfigs(function() {
			that.restarter(w);
		});
	};


	BigBang.prototype.startUserConfigs = function(k) {
		var that = this;
		helpers.forEachK(this.userConfigs,
				 function(aConfig, k2) {
					that.caller(aConfig.startup,
						    [that.configStartupArg],
						    function(res) {
						   	aConfig.shutdownArg = res;
							return k2();
						    },
						    function(e) { that.handleError(e); },
						    'startUserConfigs');
					},
					function(e) { that.handleError(e); },
					k);
	};

	BigBang.prototype.shutdownUserConfigs = function(k) {
		var that = this;
		helpers.forEachK(this.userConfigs,
				 function(aConfig, k2) {
				 	that.caller(aConfig.shutdown,
						    [aConfig.shutdownArg],
						    k2,
						    function(e) { throw e; },
						    'shutdownUserConfigs');
				 },
				 function(e) { throw e; },
				 k);
	};

	
	// executeRenderEffects executes render effects and unwraps js objects inside a dom tree
	BigBang.prototype.executeRenderEffects = function(x, k) {
		var that = this;
		if ( types.isRenderEffect(x) ) {
			x.callImplementation(this.caller,
					     function(y) { that.executeRenderEffects(y, k); });
		}
		else if ( isPair(x) ) {
			this.executeRenderEffects(x.first(), function(first) {
				that.executeRenderEffects(x.rest(), function(rest) {
					k( types.cons(first, rest) );
				});
			});
		}
		else {
			k(x);
		}
	};

	var deepUnwrapDomTree = function(x) {
		if ( types.isJsObject(x) ) {
			return deepUnwrapDomTree(x.obj);
		}
		else if ( isPair(x) ) {
			var ret = [];
			while ( !x.isEmpty() ) {
				ret.push( deepUnwrapDomTree(x.first()) );
				x = x.rest();
			}
			return ret;
		}
		else {
			return x;
		}
	};


	BigBang.prototype.handleError = function(e) {
		var wrapError = function(error) {
			if ( types.isSchemeError(e) || types.isInternalError(e) ) {
				return e;
			}
			else if ( typeof(e) === 'string' ) {
				return types.schemeError( types.incompleteExn(types.exnFail, e, []) );
			}
			else if ( e instanceof Error ) {
				return types.schemeError( types.incompleteExn(types.exnFail, e.message, []) );
			}
			else {
				return types.schemeError(e);
			}
		};

		try {
			if (typeof(console) !== 'undefined' && console.log) {
				if (e.stack) {
					console.log(e.stack);
				}
				else {
					console.log(e);
				}
			}

			this.shutdown( wrapError(e) );
		} catch(e2) {
			this.restarter( wrapError(e2) );
		}
	};





	//////////////////////////////////////////////////////////////////////
	//From this point forward, we define wrappers to integrate jsworld
	//with Moby.

	// types are
	// sexp: (cons node (listof sexp))
	// css-style: (node (listof (list string string)))


	// The default printWorldHook will write the written content of the node.
	// We probably want to invoke the pretty printer here instead!
	Jsworld.printWorldHook = function(world, node) {
		var newNode;
		if( node.lastChild === null ) {
			newNode = types.toDomNode(world);
			node.appendChild(newNode);
			helpers.maybeCallAfterAttach(newNode);
		} else {
			newNode = types.toDomNode(world);
			node.replaceChild(newNode, node.lastChild);
			helpers.maybeCallAfterAttach(newNode);
		}
	};

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
		var fail = function(formatStr, formatArgs) {
			helpers.raise(types.incompleteExn(
					types.exnFailContract,
					helpers.format(formatStr, formatArgs),
					[]));
		}

		if ( isPair(x) ) {
			var firstElt = x.first();
			var node = getBaseNode(firstElt);
			var restElts = x.rest();

			if ( !isNode(node) ) {
				fail('on-draw: expected type <dom-element> but found ~s; the first element within ~s',
				     [firstElt, top]);
			}

			if ( node.nodeType == Node.TEXT_NODE && !restElts.isEmpt() ) {
				fail('on-draw: text nodes cannot have children. Node ~s had children in ~s',
				     [firstElt, top]);
			}

			var i = 2;
			while ( !restElts.isEmpty() ) {
				checkWellFormedDomTree(restElts.first(), x, i);
				restElts = restElts.rest();
				i++;
			}
		} else {
			var formatStr = "on-draw: expected a dom-s-expression, but received ~s instead";
			var formatArgs = [x];
			if (index != undefined) {
				formatStr += "; the ~a element within ~s";
				formatArgs.push( helpers.ordinalize(index) );
				formatArgs.push(top);
			}

			fail(formatStr, formatArgs);
		}
	};

	/////////////////////////////////////////////////////////////////

	//
	// DOM UPDATING STUFFS
	//

	// nodeToTree: dom -> dom-tree
	// Given a native dom node, produces the appropriate tree.
	var nodeToTree = function(domNode) {
		var result = [domNode];
		for (var c = domNode.firstChild; c != null; c = c.nextSibling) {
			result.push(nodeToTree(c));
		}
		return result;
	}



	// nodes(tree(N)) = nodes(N)
	var nodes = function(tree) {
		var ret = [tree.node];
		
		if (tree.node.jsworldOpaque == true) {
			return ret;
		}

		for (var i = 0; i < tree.children.length; i++)
			ret = ret.concat(nodes(tree.children[i]));
		
		return ret;
	}

	// nodeEq: node node -> boolean
	// Returns true if the two nodes should be the same.
	var nodeEq = function(node1, node2) {
		return (node1 && node2 && node1 === node2);
	}

	var nodeNotEq = function(node1, node2) {
		return ! nodeEq(node1, node2);
	}



	// relations(tree(N)) = relations(N)
	var relations = function(tree) {
		var ret = [];
		
		if (tree.node.jsworldOpaque == true) { return []; }

		for (var i = 0; i < tree.children.length; i++)
			ret.push({ relation: 'parent', parent: tree.node, child: tree.children[i].node });
		
		for (var i = 0; i < tree.children.length - 1; i++)
			ret.push({ relation: 'neighbor', left: tree.children[i].node, right: tree.children[i + 1].node });
		
		for (var i = 0; i < tree.children.length; i++)
			ret = ret.concat(relations(tree.children[i]));
		
		return ret;
	}


	var appendChild = function(parent, child) {
		parent.appendChild(child);
	}


	// updateDom(nodes(Node), relations(Node)) = void
	var updateDom = function(toplevelNode, nodes, relations) {

		// TODO: rewrite this to move stuff all in one go... possible? necessary?
		
		// move all children to their proper parents
		for (var i = 0; i < relations.length; i++) {
			if (relations[i].relation == 'parent') {
			var parent = relations[i].parent, child = relations[i].child;
			if (nodeNotEq(child.parentNode, parent)) {
				appendChild(parent, child);
			} else {
			}
			}
		}
		
		// arrange siblings in proper order
		// truly terrible... BUBBLE SORT
		var unsorted = true;
		while (unsorted) {
			unsorted = false;
			
			for (var i = 0; i < relations.length; i++) {
			if (relations[i].relation == 'neighbor') {
				var left = relations[i].left, right = relations[i].right;
					
				if (nodeNotEq(left.nextSibling, right)) {
				left.parentNode.insertBefore(left, right)
				unsorted = true;
				}
			}
			}
			
//			if (!unsorted) break;
		}

		
		// remove dead nodes
		var live_nodes;
		
		// it is my hope that by sorting the nodes we get the worse of
		// O(n*log n) or O(m) performance instead of O(n*m)
		// for all I know Node.compareDocumentPosition is O(m)
		// and now we get O(n*m*log n)
		var positionComparator = function(a, b) {
			var rel = a.compareDocumentPosition(b);
			// children first
			if (rel & a.DOCUMENT_POSITION_CONTAINED_BY) return 1;
			if (rel & a.DOCUMENT_POSITION_CONTAINS) return -1;
			// otherwise use precedes/follows
			if (rel & a.DOCUMENT_POSITION_FOLLOWING) return -1;
			if (rel & a.DOCUMENT_POSITION_PRECEDING) return 1;
			// otherwise same node or don't care, we'll skip it anyway
			return 0;
		}
		
		try {
			// don't take out concat, it doubles as a shallow copy
			// (as well as ensuring we keep document.body)
			live_nodes = nodes.concat(toplevelNode).sort(positionComparator);
		}
		catch (e) {
			// probably doesn't implement Node.compareDocumentPosition
			live_nodes = null;
		}
		
		var node = toplevelNode, stop = toplevelNode.parentNode;
		while (node != stop) {
			while ( !(node.firstChild == null || node.jsworldOpaque) ) {
			// process first
			// move down
			node = node.firstChild;
			}
			
			while (node != stop) {
			var next = node.nextSibling, parent = node.parentNode;
			
			// process last
			var found = false;
			var foundNode = undefined;

			if (live_nodes != null)
				while (live_nodes.length > 0 && positionComparator(node, live_nodes[0]) >= 0) {
				var other_node = live_nodes.shift();
				if (nodeEq(other_node, node)) {
					found = true;
					foundNode = other_node;
					break;
				}
				// need to think about this
				//live_nodes.push(other_node);
				}
			else
				for (var i = 0; i < nodes.length; i++)
				if (nodeEq(nodes[i], node)) {
					found = true;
					foundNode = nodes[i];
					break;
				}
				
			if (!found) {
				if (node.isJsworldOpaque) {
				} else {
				// reparent children, remove node
				while (node.firstChild != null) {
					appendChild(node.parentNode, node.firstChild);
				}
				}
					
				next = node.nextSibling; // HACKY
				node.parentNode.removeChild(node);
			} else {
				mergeNodeValues(node, foundNode);
			}

			// move sideways
			if (next == null) node = parent;
			else { node = next; break; }
			}
		}

		
		refresh_node_values(nodes);
	}

	var mergeNodeValues = function(node, foundNode) {
//		for (attr in node) {
//			foundNode[attr] = node[attr];
//		}
	}



	// camelCase: string -> string
	var camelCase = function(name) {
		return name.replace(/\-(.)/g, function(m, l){return l.toUpperCase()});
	}


	var set_css_attribs = function(node, attribs) {
		for (var j = 0; j < attribs.length; j++){
			node.style[camelCase(attribs[j].attrib)] = attribs[j].values.join(" ");
		}
	}


	// isMatchingCssSelector: node css -> boolean
	// Returns true if the CSS selector matches.
	var isMatchingCssSelector = function(node, css) {
		if (css.id.match(/^\./)) {
			// Check to see if we match the class
			return ('class' in node && member(node['class'].split(/\s+/),
							  css.id.substring(1)));
		} else {
			return ('id' in node && node.id == css.id);
		}
	}


	var updateCss = function(nodes, css) {
		// clear CSS
		for (var i = 0; i < nodes.length; i++) {
			if ( !nodes[i].jsworldOpaque ) {
				clearCss(nodes[i]);
			}
		}
		
		// set CSS
		for (var i = 0; i < css.length; i++)
			if ('id' in css[i]) {
			for (var j = 0; j < nodes.length; j++)
				if (isMatchingCssSelector(nodes[j], css[i])) {
				set_css_attribs(nodes[j], css[i].attribs);
				}
			}
			else set_css_attribs(css[i].node, css[i].attribs);
	}


	var clearCss = function(node) {
		if ('style' in node)
			node.style.cssText = "";
	}



	// If any node cares about the world, send it in.
	var refresh_node_values = function(nodes) {
		for (var i = 0; i < nodes.length; i++) {
			if (nodes[i].onWorldChange) {
			nodes[i].onWorldChange(world);
			}
		}
	}


	var sexp2tree = function(sexp, aBigBang) {
		if (sexp.length == undefined) {
			return { node: sexp,
				 children: [] };
		}
		else {
			// If we have a NodeWithEvents, actually attach the events here
			var node = getBaseNode(sexp[0]);
			if ( sexp[0] instanceof NodeWithEvents ) {
				sexp[0].attachEvents(aBigBang);
			}
			return { node: node,
				 children: helpers.map(function(sexp) { return sexp2tree(sexp, aBigBang); },
						       sexp.slice(1)) };
		}
	};


	var sexp2attrib = function(sexp) {
		return { attrib: sexp[0], values: sexp.slice(1) };
	};

	var sexp2css_node = function(sexp) {
		var attribs = helpers.map(sexp2attrib, sexp.slice(1));
		if (typeof(sexp[0]) === 'string') {
			return [{ id: sexp[0], attribs: attribs }];
		}
		else if ( 'length' in sexp[0] ) {
			return helpers.map(function(id) { return { id: id, attribs: attribs } },
				   sexp[0]);
		}
		else {
			return [{ node: sexp[0], attribs: attribs }];
		}
	};

	var sexp2css = function(sexp) {
		return helpers.concatMap(sexp2css_node, sexp);
	};



	var doRedraw = function(aBigBang, w, redrawFun, redrawCssFun, k) {
		aBigBang.rendertimeDetachEvents();
		maintainingSelection(
		    function(k2) {
			// For legibility, here is the non-CPS version of the same function:
			/*
 			var newPage = redrawFun(w);		
			var newCss = redrawCssFun(w);
			var t = sexp2tree(newPage);
 			var ns = nodes(t);

			// Try to save the current selection and preserve it across updates
			// Also, avoid updating the dom if the value hasn't changed
 			if ( newPage !== aBigBang.lastPage ) {
				// Kludge: update the CSS styles first.
				// This is a workaround an issue with excanvas: any style change
				// clears the content of the canvas, so we do this first before
				// attaching the dom element.
				updateCss(ns, sexp2css(newCss));
				updateDom(aBigBang.toplevelNode, ns, relations(t));
 			} else if ( newCss !== aBigBang.lastCss ) {
				updateCss(ns, sexp2css(newCss));
 			}
			aBigBang.lastPage = newPage;
			aBigBang.lastCss = newCss;
			*/
			redrawFun(w,
			    function(newPage) {
				redrawCssFun(w,
				    function(newCss) {
					var t = sexp2tree(newPage, aBigBang);
					var ns = nodes(t);

					// Try to save the current selection and preserve it across updates
					// Also, avoid updating the dom if the value hasn't changed
					if ( newPage !== aBigBang.lastPage ) {
						// Kludge: update the CSS styles first.
						// This is a workaround an issue with excanvas: any style change
						// clears the content of the canvas, so we do this first before
						// attaching the dom element.
						updateCss(ns, sexp2css(newCss));
						updateDom(aBigBang.toplevelNode, ns, relations(t));
					}
					else if ( newCss !== aBigBang.lastCss ) {
						updateCss(ns, sexp2css(newCss));
					}
					// either way, update the lastPage and lastCss fields of the big bang
					aBigBang.lastPage = newPage;
					aBigBang.lastCss = newCss;
					k2();
				});
			});
		    }, k);
	};


	// maintainingSelection: (-> void) -> void
	// Calls the thunk f while trying to maintain the current focused selection.
	var maintainingSelection = function(f, k) {
		var currentFocusedSelection;
		if (hasCurrentFocusedSelection()) {
			currentFocusedSelection = getCurrentFocusedSelection();
			f(function() {
				currentFocusedSelection.restore();
				k();
			});
		} else {
			f(function() { k(); });
		}
	}



	var FocusedSelection = function() {
		this.focused = currentFocusedNode;
		this.selectionStart = currentFocusedNode.selectionStart;
		this.selectionEnd = currentFocusedNode.selectionEnd;
	}

	// Try to restore the focus.
	FocusedSelection.prototype.restore = function() {
		// FIXME: if we're scrolling through, what's visible
		// isn't restored yet.
		if (this.focused.parentNode) {
			this.focused.selectionStart = this.selectionStart;
			this.focused.selectionEnd = this.selectionEnd;
			this.focused.focus();
		} else if (this.focused.id) {
			var matching = document.getElementById(this.focused.id);
			if (matching) {
				matching.selectionStart = this.selectionStart;
				matching.selectionEnd = this.selectionEnd;
				matching.focus();
			}
		}
	};

	var hasCurrentFocusedSelection = function() {
		return currentFocusedNode != undefined;
	};

	var getCurrentFocusedSelection = function() {
		return new FocusedSelection();
	};





	/////////////////////////////////////////////////////////////////////
	// Configurations to be exported

	var StopWhenConfig = function(test) {
		this.test = test;
	};

	StopWhenConfig.prototype.getHandler = function(aBigBang) {
		var that = this;
		return function(w, k) {
			aBigBang.caller(that.test,
					[w],
					function(stop) {
						if (stop) {
							aBigBang.shutdown(w);
						} else {
							k();
						}
					},
					function(e) { that.handleError(e); },
					'stopWhen');
		}
	};

	
	var DrawConfig = types.Class.extend({
		getHandler: function(aBigBang) {
				throw types.internalError('tried to configure unimplemented drawing configuration', false);
		}
	});

	var OnDrawPageConfig = DrawConfig.extend({
		init: function(draw, drawCss) {
			this.draw = draw;
			this.drawCss = drawCss;
		},
		getHandler: function(aBigBang) {
		    var that = this;
		    var wrappedRedraw = function(w, k) {
			aBigBang.caller(that.draw,
					[w],
					function(newDomTree) {
						aBigBang.executeRenderEffects(newDomTree, function(tree) {
							checkWellFormedDomTree(tree, tree, undefined);
							k([aBigBang.toplevelNode,
							   deepUnwrapDomTree(tree)]);
						});
					},
					function(e) { that.handleError(e); },
					'onDrawPage');
		    };

		    var wrappedRedrawCss = function(w, k) { k([]); };
		    if ( this.drawCss ) {
			wrappedRedrawCss = function(w, k) {
				aBigBang.caller(that.drawCss, [w],
						function(res) {
							k( helpers.deepListToArray(res) );
						},
						function(e) { aBigBang.handleError(e); },
						'onDrawPageCss');
			};
		    }
		    return function(w, k) {
			doRedraw(aBigBang, w, wrappedRedraw, wrappedRedrawCss, k);
		    };
		}
	});


	var OnDrawSceneConfig = DrawConfig.extend({
		init: function(draw) {
			this.draw = draw;
		},
		getHandler: function(aBigBang) {
		    var that = this;
		    var reusableCanvas = undefined;
		    var reusableCanvasNode = undefined;

		    var wrappedRedraw = function(w, k) {
			aBigBang.caller(that.draw, [w],
			    function(aScene) {
				// Performance hack: if we're using onRedraw, we know
				// we've got a scene, so we optimize away the repeated
				// construction of a canvas object.
				if ( world.Kernel.isImage(aScene) ) {
					var width = aScene.getWidth();
					var height = aScene.getHeight();

					if ( !reusableCanvas ) {
						reusableCanvas = world.Kernel.makeCanvas(width, height);
						// Node: the canvas object may itself manage object,
						// as in the case of an excanvas. In that case, we must make
						// sure jsworld doesn't try to disrupt its contents!
						reusableCanvas.jsworldOpaque = true;
						reusableCanvasNode = nodeToTree(reusableCanvas);
					}

					reusableCanvas.width = width;
					reusableCanvas.height = height;
					var ctx = reusableCanvas.getContext('2d');
					aScene.render(ctx, 0, 0);

					k([aBigBang.toplevelNode, reusableCanvasNode]);
				}
				else {
					helpers.raise(types.incompleteExn(
							types.exnFailContract,
							helpers.format('~a: result of type <scene> expected, your function produced: ~s',
								       [that.draw.name, aScene])));
//					k([aBigBang.toplevelNode, nodeToTree( types.toDomNode(aScene) )]);
				}
			    },
			    function(e) { aBigBang.handleError(e); },
			    'onDrawScene');
		    };

		    var wrappedRedrawCss = function(w, k) {
			k([[reusableCanvas,
			    ['width', reusableCanvas.width + 'px'],
			    ['height', reusableCanvas.height + 'px']]]);
		    };
		    return function(w, k) {
			doRedraw(aBigBang, w, wrappedRedraw, wrappedRedrawCss, k);
		    };
		}
	});
			


	/////////////////////////////////////////////////////////////////
	// Actual BigBang


	var raiseBigBangException = function(message, args) {
		var formatStr = message + helpers.map(function() { return ' ~s'; }, args).join('');
		helpers.raise(types.incompleteExn(
				types.exnFailContract,
				helpers.format(formatStr, args),
				[]));
	};

	var bigBang = function(initWorld, toplevelNode, handlers, caller, restarter, controller) {
		var newBigBang = new BigBang(caller, restarter, toplevelNode);

		var stopWhen = false;
		var drawHandler = false;
		for (var i = 0; i < handlers.length; i++) {
			if ( handlers[i] instanceof StopWhenConfig ) {
				if (stopWhen) {
					raiseBigBangException('js-big-bang: expected no more than one stop-when argument; arguments were:',
							      [initWorld].concat(handlers));
				}
				stopWhen = handlers[i].getHandler(newBigBang);
			}
			else if ( handlers[i] instanceof DrawConfig ) {
				if (drawHandler) {
					raiseBigBangException('js-big-bang: expects no more than one world-drawing arguments; arguments were:',
							      [initWorld].concat(handlers));
				}
				drawHandler = handlers[i].getHandler(newBigBang);
			}
			else if ( types.isWorldConfig(handlers[i]) ) {
				newBigBang.userConfigs.push(handlers[i]);
			}
		}
		if ( !drawHandler ) {
			drawHandler = function(w, k) {
				Jsworld.printWorldHook(w, newBigBang.toplevelNode);
				k();
			};
		}
		newBigBang.addWorldListener(drawHandler);

		if (stopWhen) {
			newBigBang.addWorldListener(stopWhen);
		}

		// Set the controller's breaker function
		// This requires a state to get the current closure
		controller.breaker = function(aState) {
			newBigBang.handleError(
				types.schemeError(
					types.incompleteExn(
						// unfortunately, the current state is clean, so we need to wait until
						// the state is restored before getting the current continuation or it will be useless
						function(name, contMarks) {
							var continuation = new types.ContinuationClosureValue(aState.vstack, aState.cstack);
							return types.exnBreak(name, contMarks, continuation);
						},
						'user break',
						[])));
		};
		
		// change the world to the initial world and start the user configs.
		newBigBang.changeWorld(function(w, k) { k(initWorld); },
				       function() { newBigBang.startUserConfigs(doNothing); });
	};



	/////////////////////////////////////////////////////////////////////
	// Event Things

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
		if (node.removeEventListener) {
			// Mozilla
			node.removeEventListener(eventName, fn, false);
		} else {
			// IE
			node.detachEvent('on' + eventName, fn, false);
		}
	};


	// NodeWithEvents holds a dom node and event attachers
	// to attach events at render time
	var NodeWithEvents = function(node) {
		this.node = node;
		this.eventAttachers = [];
	};

	NodeWithEvents.prototype.toDomNode = function(cache) {
		return this.node;
	};

	NodeWithEvents.prototype.attachEvents = function(aBigBang) {
		for (var i = 0; i < this.eventAttachers.length; i++) {
			this.eventAttachers[i](aBigBang);
		}
	};

	NodeWithEvents.prototype.addEventAttacher = function(attacher) {
		this.eventAttachers.push(attacher);
	};


	// getBaseNode: (or/c dom-node NodeWithEvents) -> domNode
	// gets the actual dom node out of something that may be a wrapped node
	var getBaseNode = function(node) {
		if ( types.isJsObject(node) ) {
			return getBaseNode(node.obj);
		}
		else if ( node instanceof NodeWithEvents ) {
			return node.node;
		}
		else {
			return node;
		}
	};


	// addEvent: (or/c dom-node NodeWithEvents) string (event -> ) -> NodeWithEvents
	// makes sure node is wrapped in a NodeWithEvents and adds a new event attacher (based on the handler creator)
	// to attach a new event at render time.
	var addEvent = function(node, event, handlerCreator) {
		var wrappedNode = node;
		if ( !(node instanceof NodeWithEvents) ) {
			wrappedNode = new NodeWithEvents(node);
		}

		var attacher = function(aBigBang) {
			var eventHandler = handlerCreator(aBigBang);
			attachEvent(node, event, eventHandler);
			aBigBang.addRendertimeEventDetacher(function() { detachEvent(node, event, eventHandler); });
		};

		wrappedNode.addEventAttacher(attacher);
		return wrappedNode;
	};



	var addFocusTracking = function(node) {
		var baseNode = getBaseNode(node);
		attachEvent(baseNode, "focus", function(e) { currentFocusedNode = baseNode; });
		attachEvent(baseNode, "blur", function(e) { currentFocusedNode = undefined; });
		return node;
	};

	var copyAttribs = function(node, attribs) {
		var wrappedNode = node;
		baseNode = getBaseNode(node);
		if (attribs) {
			for (a in attribs) {
				if ( attribs.hasOwnProperty(a) ) {
					if (typeof(attribs[a]) === 'function') {
						var handlerCreator = function(aBigBang) {
							return function(e) {
								aBigBang.changeWorld(function(w, k) { attribs[a](w, e, k); }, doNothing);
							};
						};
						wrappedNode = addEvent(wrappedNode, a, handlerCreator);
					}
					else {
						baseNode[a] = attribs[a];
					}
				}
			}
		}
		return wrappedNode;
	};


	var stopClickPropagation = function(node) {
		attachEvent(getBaseNode(node), 'click',
			    function(e) {
			    	if (e.stopPropagation) {
					e.stopPropagation();
				}
				else {
					e.cancelBubble = true;
				}
			    });
		return node;
	};


	//
	// NODE TYPES
	//

	var p = function(attribs) {
		return addFocusTracking( copyAttribs(document.createElement('p'), attribs) );
	};

	var div = function(attribs) {
		return addFocusTracking( copyAttribs(document.createElement('div'), attribs) );
	};

	var text = function(s, attribs) {
		var result = document.createElement("div");
		result.appendChild(document.createTextNode(s));
		result.style['white-space'] = 'pre';
		result.jsworldOpaque = true;
		return result;
	};

	var img = function(src, attribs) {
		var n = document.createElement('img');
		n.src = src;
		return addFocusTracking( copyAttribs(n, attribs) );
	};

	var textarea = function(attribs) {
		return addFocusTracking( copyAttribs(document.createElement('textarea'), attribs) );
	};

	var header = function(n) {
		return function(attribs) {
			return addFocusTracking( copbyAttribs(document.createElement('h'+n), attribs) );
		};
	};

//	var button = function(updateF, attribs) {
//		var handlerCreator = function(aBigBang) {
//			return function(e) {
//				aBigBang.changeWorld(function(w, k) { aBigBang.caller(updateF, [w], k); }, doNothing);
//			};
//		};
//		var n = addEvent(document.createElement('button'), 'click', handlerCreator);
//		return addFocusTracking( copyAttribs(n, attribs) );
//	};
//	Jsworld.button = button;


	var buttonBang = function(updateWorldF, effectF, attribs) {
		var handlerCreator = function(aBigBang) {
			return function(e) {
				aBigBang.changeWorld(function(w, k) {
					/* This function is CPSed and hence difficult to read.
					 * A non-cps version that treates updateWorldF and effectF
					 * as regular functions is written below.
					 *
					 * *******************************************
					 *
					 * var effect_schemeList = effectF(w);
					 * var effects = helpers.flattenSchemeListToArray(effect_schemeList);
					 * var newWorld = updateWorldF(w);
					 * if ( effects.length == 0 ) {
					 * 	return newWorld;
					 * } else {
					 * 	return worldWithEffects(newWorld, effects);
					 * }
					 *
					 * *******************************************
					 */
					aBigBang.caller(effectF, [w],
						function(effects_schemeList) {
							var effects = helpers.flattenSchemeListToArray(effects_schemeList);
							aBigBang.caller(updateWorldF, [w],
								function(newWorld) {
									if ( effects.length == 0 ) {
										k(newWorld);
									}
									else {
										k( worldWithEffects(newWorld, effects) );
									}
								});
						});
				},
				doNothing);
			};
		};
		var n = document.createElement('button');
		// stop default behavior of buttons to prevent page reloading
		n.onclick = function(e) { return false; };
		
		var wrappedNode = addEvent(n, 'click', handlerCreator);
		return stopClickPropagation( addFocusTracking(copyAttribs(wrappedNode, attribs)) );
	};


	var input = function(type, updateF, attribs) {
		type = type.toLowerCase();
		var dispatchTable = { text: textInput,
				      password: textInput,
				      checkbox: checkboxInput };

		if ( dispatchTable[type] ) {
			return (dispatchTable[type])(type, updateF, attribs);
		}
		else {
			helpers.raise(types.incompleteExn(
					types.exnFailContract,
					helpers.format('js-input: does not currently support type ~s', [type]),
					[]));
		}
	};

	var textInput = function(type, updateF, attribs) {
		var n = document.createElement('input');
		n.type = type;
		var handlerCreator = function(aBigBang) {
			return function(e) {
				setTimeout(function() {
						aBigBang.changeWorld(function(w, k) { aBigBang.caller(updateF, [w, n.value], k); },
								     doNothing);
					   },
					   0);
			};
		};

		var wrappedNode = addEvent(n, 'keydown', handlerCreator);
		return stopClickPropagation( addFocusTracking(copyAttribs(wrappedNode, attribs)) );
	};

	var checkboxInput = function(type, updateF, attribs) {
		var n = document.createElement('input');
		n.type = type;

		var handlerCreator = function(aBigBang) {
			return function(e) {
				setTimeout(function() {
						aBigBang.changeWorld(function(w, k) { aBigBang.caller(updateF, [w, n.checked], k); },
								     doNothing);
					   },
					   0);
			};
		};

		var wrappedNode = addEvent(n, 'change', handlerCreator);
		return stopClickPropagation( copyAttribs(wrappedNode, attribs) );
	};


	var select = function(options, updateF, attribs) {
		var n = document.createElement('select');
		for (var i = 0; i < options.length; i++) {
			n.add( option({value: options[i]}), null );
		}
		n.jsworldOpaque = true;

		var handlerCreator = function(aBigBang) {
			return function(e) {
				aBigBang.changeWorld(function(w, k) { aBigBang.caller(updateF, [w, e.target.value], k); },
						     doNothing);
			};
		};
		var wrappedNode = addEvent(n, 'change', handlerCreator);
		return addFocusTracking( copyAttribs(wrappedNode, attribs) );
	};

	var option = function(attribs) {
		var node = document.createElement('option');
		node.text = attribs.value;
		node.value = attribs.value;
		return node;
	};


	/////////////////////////////////////////////////
	// Exports
	Jsworld.worldWithEffects = function(effects, w) {
		return new WrappedWorldWithEffects(effects, w);
	};
	Jsworld.hasEffects = function(w) {
		return (w instanceof WrappedWorldWithEffects);
	};

	Jsworld.isBuiltInConfig = function(x) {
		return ( x instanceof StopWhenConfig ||
			 x instanceof DrawConfig );
	};
	Jsworld.stopWhenConfig = function(test) { return new StopWhenConfig(test); };
	Jsworld.onDrawPageConfig = function(draw, drawCss) { return new OnDrawPageConfig(draw, drawCss); };
	Jsworld.onDrawSceneConfig = function(draw) { return new OnDrawSceneConfig(draw); };

	Jsworld.bigBang = bigBang;

	Jsworld.p = p;
	Jsworld.div = div;
	Jsworld.text = text;
	Jsworld.img = img;
	Jsworld.textarea = textarea;
	Jsworld.h1 = header(1);
	Jsworld.h2 = header(2);
	Jsworld.h3 = header(3);
	Jsworld.h4 = header(4);
	Jsworld.h5 = header(5);
	Jsworld.h6 = header(6);
	Jsworld.buttonBang = buttonBang;
	Jsworld.input = input;
	Jsworld.select = select;

})();


