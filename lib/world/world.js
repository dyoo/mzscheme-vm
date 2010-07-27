

// Depends on kernel.js, world-config.js, effect-struct.js
(function() {
    
    world.Kernel = {};
    var worldListeners = [];
    var stopped;
    var timerInterval = false;


    // Inheritance from pg 168: Javascript, the Definitive Guide.
    var heir = function(p) {
	var f = function() {}
	f.prototype = p;
	return new f();
    }


    // clone: object -> object
    // Copies an object.  The new object should respond like the old
    // object, including to things like instanceof
    var clone = function(obj) {
	var C = function() {}
	C.prototype = obj;
	var c = new C();
	for (property in obj) {
	    if (obj.hasOwnProperty(property)) {
		c[property] = obj[property];
	    }
	}
	return c;
    };




    var announceListeners = [];
    world.Kernel.addAnnounceListener = function(listener) {
	announceListeners.push(listener);
    };
    world.Kernel.removeAnnounceListener = function(listener) {
	var idx = announceListeners.indexOf(listener);
	if (idx != -1) {
	    announceListeners.splice(idx, 1);
	}
    };
    world.Kernel.announce = function(eventName, vals) {
	for (var i = 0; i < announceListeners.length; i++) {
	    try {
		announceListeners[i](eventName, vals);
	    } catch (e) {}
	}
    };










    // changeWorld: world -> void
    // Changes the current world to newWorld.
    var changeWorld = function(newWorld) {
	world = newWorld;
	notifyWorldListeners();
    }


    // updateWorld: (world -> world) -> void
    // Public function: update the world, given the old state of the
    // world.
    world.Kernel.updateWorld = function(updater) {
	var newWorld = updater(world);
	changeWorld(newWorld);
    }


    world.Kernel.shutdownWorld = function() {
	stopped = true;
    };


    // notifyWorldListeners: -> void
    // Tells all of the world listeners that the world has changed.
    var notifyWorldListeners = function() {
	var i;
	for (i = 0; i < worldListeners.length; i++) {
	    worldListeners[i](world);
	}
    }

    // addWorldListener: (world -> void) -> void
    // Adds a new world listener: whenever the world is changed, the aListener
    // will be called with that new world.
    var addWorldListener = function(aListener) {
	worldListeners.push(aListener);
    }
    

    // getKeyCodeName: keyEvent -> String
    // Given an event, try to get the name of the key.
    var getKeyCodeName = function(e) {
	var code = e.charCode || e.keyCode;
	var keyname;
	if (code == 37) {
	    keyname = "left";
	} else if (code == 38) {
	    keyname = "up";
	} else if (code == 39) {
	    keyname = "right";
	} else if (code == 40) {
	    keyname = "down";
	} else {
	    keyname = String.fromCharCode(code); 
	}
	return keyname;
    }


    // resetWorld: -> void
    // Resets all of the world global values.
    var resetWorld = function() {
	if (timerInterval) {
	    clearInterval(timerInterval);
	    timerInterval = false;
	}
	stopped = false;
	worldListeners = [];
    }


    var getBigBangWindow = function(width, height) {
        if (window.document.getElementById("canvas") != undefined) {
	    return window;
	}

        var newWindow = window.open(
	    "big-bang.html",
	    "big-bang");
	//"toolbar=false,location=false,directories=false,status=false,menubar=false,width="+width+",height="+height);
	if (newWindow == null) { 
            throw new Error("Error: Not allowed to create a new window."); }

	return newWindow;
    }


    // bigBang: number number world (arrayof (-> void)) -> void
    // Begins a world computation.  The initial world is aWorld, and handlers
    // register other reactive functions (timer tick, key press, etc.) which
    // will change the world.
    world.Kernel.bigBang = function(width, height, aWorld, handlers) {
//	helpers.check(width, helpers.isNumber, "big-bang", "number", 1);
//	helpers.check(height, helpers.isNumber, "big-bang", "number", 2);
//	helpers.arrayEach(args, function(x, i) { 
//	    helpers.check(x, helpers.isFunction, "big-bang", "handler", i+4) });
	

	var i;
	var newWindow = getBigBangWindow(width, height);
	var canvas = 
	    newWindow.document.getElementById("canvas");
	canvas.width = jsnums.toFixnum(width);
	canvas.height = jsnums.toFixnum(height);

	resetWorld();

	var config = new world.config.WorldConfig();
	for (i = 0; i < handlers.length; i++) {
	    config = handlers[i](config);
	}
	config = config.updateAll({'changeWorld': world.Kernel.updateWorld,
				   'shutdownWorld': world.Kernel.shutdownWorld});
	world.config.CONFIG = config;


	if (config.lookup('onKey')) {
	    newWindow.onkeydown = function(e) {
		world.stimuli.onKey(e);
	    }
	}

	if (config.lookup('onRedraw')) {
	    addWorldListener(function (w) {
		var context = canvas.getContext("2d");
		uCaller(config.lookup('onRedraw'), [w],
			function (aScene) {
//				var aScene = config.lookup('onRedraw')([w]);
				aScene.render(context, 0, 0);
			});
	    });
	}

	addWorldListener(function (w) {
	    if (config.lookup('stopWhen')) {
		if (config.lookup('stopWhen')([w])) {
		    stopped = true;
		}
	    }
	});


 	if(config.lookup('onTick')) {
	    scheduleTimerTick(newWindow, config);
	}


 	changeWorld(aWorld);

	if (config.lookup('initialEffect')) {
	    var updaters = world.Kernel.applyEffect(
		config.lookup('initialEffect'));
	    for (var i = 0; i < updaters.length; i++) {
		if (! stopped) {
		    updateWorld(updaters);
		}
	    }
	}

    };

    // scheduleTimerTick: -> void
    // Repeatedly schedules an evaluation of the onTick until the program has stopped.
    var scheduleTimerTick = function(window, config) {
	timerInterval = window.setInterval(
	    function() {
		if (stopped) {
		    window.clearTimeout(timerInterval);
		    timerInterval = false;
		}
		else {
		    world.stimuli.onTick();
		}
	    },
	    config.lookup('tickDelay'));
    }




    // Base class for all images.
    var BaseImage = function(pinholeX, pinholeY) {
	this.pinholeX = pinholeX;
	this.pinholeY = pinholeY;
    }
    world.Kernel.BaseImage = BaseImage;


    var isImage = function(thing) {
	return (thing !== null &&
		thing !== undefined &&
		thing instanceof BaseImage);
    }



    BaseImage.prototype.updatePinhole = function(x, y) {
	var aCopy = clone(this);
	aCopy.pinholeX = x;
	aCopy.pinholeY = y;
	return aCopy;
    };



    // render: context fixnum fixnum: -> void
    // Render the image, where the upper-left corner of the image is drawn at
    // (x, y).
    BaseImage.prototype.render = function(ctx, x, y) {
	    throw new Error('BaseImage.render unimplemented!');
//	plt.Kernel.throwMobyError(
//	    false, 
//	    "make-moby-error-type:generic-runtime-error", 
//	    "Unimplemented method render");
    };


    // makeCanvas: number number -> canvas
    // Constructs a canvas object of a particular width and height.
    world.Kernel.makeCanvas = function(width, height) {
	var canvas = document.createElement("canvas");
 	canvas.width = width;
 	canvas.height = height;

 	canvas.style.width = canvas.width + "px";
 	canvas.style.height = canvas.height + "px";
	
	// KLUDGE: IE compatibility uses /js/excanvas.js, and dynamic
	// elements must be marked this way.
	if (window && typeof window.G_vmlCanvasManager != 'undefined') {
	    canvas = window.G_vmlCanvasManager.initElement(canvas);
	}
	return canvas;
    };



    var withIeHack = function(canvas, f) {
// 	canvas.style.display = 'none';
// 	document.body.appendChild(canvas);
// 	try {
	var result = f(canvas);
// 	} catch(e) {
// 	    document.body.removeChild(canvas);
// 	    canvas.style.display = '';
// 	    throw e;
// 	}
// 	document.body.removeChild(canvas);
// 	canvas.style.display = '';
	return result;
    };


    BaseImage.prototype.toDomNode = function(cache) {
	var that = this;
	var width = that.getWidth();
	var height = that.getHeight();
	var canvas = world.Kernel.makeCanvas(width, height);

	// KLUDGE: on IE, the canvas rendering functions depend on a
	// context where the canvas is attached to the DOM tree.

	// We initialize an afterAttach hook; the client's responsible
	// for calling this after the dom node is attached to the
	// document.
	canvas.afterAttach = function() {
	    var ctx = canvas.getContext("2d");
	    that.render(ctx, 0, 0);
	};

	return canvas;
    };




    BaseImage.prototype.toWrittenString = function(cache) { return "<image>"; }
    BaseImage.prototype.toDisplayedString = function(cache) { return "<image>"; }

    BaseImage.prototype.isEqual = function(other, aUnionFind) {
	    return (this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY);
    };



    
    // isScene: any -> boolean
    // Produces true when x is a scene.
    var isScene = function(x) {
	return ((x != undefined) && (x != null) && (x instanceof SceneImage));
    };

    // SceneImage: primitive-number primitive-number (listof image) -> Scene
    var SceneImage = function(width, height, children) {
	BaseImage.call(this, 0, 0);
	this.width = width;
	this.height = height;
	this.children = children;
    }
    SceneImage.prototype = heir(BaseImage.prototype);


    // add: image primitive-number primitive-number -> Scene
    SceneImage.prototype.add = function(anImage, x, y) {
	return new SceneImage(this.width, 
			      this.height,
			      this.children.concat([[anImage, 
						     x - anImage.pinholeX, 
						     y - anImage.pinholeY]]));
    };

    // render: 2d-context primitive-number primitive-number -> void
    SceneImage.prototype.render = function(ctx, x, y) {
	var i;
	var childImage, childX, childY;
	// Clear the scene.
	ctx.clearRect(x, y, this.width, this.height);
	// Then ask every object to render itself.
	for(i = 0; i < this.children.length; i++) {
	    childImage = this.children[i][0];
	    childX = this.children[i][1];
	    childY = this.children[i][2];
	    ctx.save();
	    childImage.render(ctx, childX + x, childY + y);
	    ctx.restore();
	}
    };

    SceneImage.prototype.getWidth = function() {
	return this.width;
    };

    SceneImage.prototype.getHeight = function() {
	return this.height;
    };

    SceneImage.prototype.isEqual = function(other, aUnionFind) {
	    if (!(other instanceof SceneImage) &&
		this.pinholeX != other.pinholeX &&
		this.pinholeY != other.pinholeY &&
		this.width != other.width ||
		this.height != other.height ||
		this.children.length != other.children.length) {
		    return false;
	    }

	    for (var i = 0; i < this.children.length; i++) {
		    if ( !types.isEqual(this.children[i], other.children[i], aUnionFind) ) {
			    return false;
		    }
	    }
	    return true;
    };


    //////////////////////////////////////////////////////////////////////

    
    var FileImage = function(src, rawImage) {
	BaseImage.call(this, 0, 0);
	var self = this;
	this.src = src;
	this.isLoaded = false;
	if (rawImage && rawImage.complete) { 
	    this.img = rawImage;
	    this.isLoaded = true;
	    this.pinholeX = self.img.width / 2;
	    this.pinholeY = self.img.height / 2;
	} else {
	    // fixme: we may want to do something blocking here for
	    // onload, since we don't know at this time what the file size
	    // should be, nor will drawImage do the right thing until the
	    // file is loaded.
	    this.img = new Image();
	    this.img.onload = function() {
		self.isLoaded = true;
		self.pinholeX = self.img.width / 2;
		self.pinholeY = self.img.height / 2;
	    };
	    this.img.onerror = function(e) {
		self.img.onerror = "";
		self.img.src = "http://www.wescheme.org/images/broken.png";
	    }
	    this.img.src = src;
	}
    }
    FileImage.prototype = heir(BaseImage.prototype);
//    world.Kernel.FileImage = FileImage;

    
    var imageCache = {};
    FileImage.makeInstance = function(path) {
	if (! (path in imageCache)) {
	    imageCache[path] = new FileImage(path);
	} 
	return imageCache[path];
    };
    
    FileImage.installInstance = function(path, rawImage) {
	imageCache[path] = new FileImage(path, rawImage);
    };
    
    FileImage.installBrokenImage = function(path) {
	imageCache[path] = new TextImage("Unable to load " + path, 10, 
					 colorDb.get("red"));
    };



    FileImage.prototype.render = function(ctx, x, y) {
	ctx.drawImage(this.img, x, y);
    };


    FileImage.prototype.getWidth = function() {
	return this.img.width;
    };


    FileImage.prototype.getHeight = function() {
	return this.img.height;
    };

    // Override toDomNode: we don't need a full-fledged canvas here.
    FileImage.prototype.toDomNode = function(cache) {
	return this.img.cloneNode(true);
    };

    FileImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof FileImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.src == other.src);
//		    types.isEqual(this.img, other.img, aUnionFind));
    };


    //////////////////////////////////////////////////////////////////////


    // OverlayImage: image image -> image
    // Creates an image that overlays img1 on top of the
    // other image.
    var OverlayImage = function(img1, img2, shiftX, shiftY) {
	var deltaX = img1.pinholeX - img2.pinholeX + shiftX;
	var deltaY = img1.pinholeY - img2.pinholeY + shiftY;
	var left = Math.min(0, deltaX);
	var top = Math.min(0, deltaY);
	var right = Math.max(deltaX + img2.getWidth(), 
			     img1.getWidth());
	var bottom = Math.max(deltaY + img2.getHeight(),
			      img1.getHeight());

	BaseImage.call(this,
		       img1.pinholeX - left,
		       img1.pinholeY - top);
	this.img1 = img1;
	this.img2 = img2;
	this.width = right - left;
	this.height = bottom - top;

	this.img1Dx = -left;
	this.img1Dy = -top;
	this.img2Dx = deltaX - left;	
	this.img2Dy = deltaY - top;
    };

    OverlayImage.prototype = heir(BaseImage.prototype);
    
    
    OverlayImage.prototype.render = function(ctx, x, y) {
	this.img2.render(ctx, x + this.img2Dx, y + this.img2Dy);
	this.img1.render(ctx, x + this.img1Dx, y + this.img1Dy);
    };

    
    OverlayImage.prototype.getWidth = function() {
	return this.width;
    };
    
    OverlayImage.prototype.getHeight = function() {
	return this.height;
    };

    OverlayImage.prototype.isEqual = function(other, aUnionFind) {
	    return ( other instanceof OverlayImage &&
		     this.pinholeX == other.pinholeX &&
		     this.pinholeY == other.pinholeY &&
		     this.width == other.width &&
		     this.height == other.height &&
		     this.img1Dx == other.img1Dx &&
		     this.img1Dy == other.img1Dy &&
		     this.img2Dx == other.img2Dx &&
		     this.img2Dy == other.img2Dy &&
		     types.isEqual(this.img1, other.img1, aUnionFind) &&
		     types.isEqual(this.img2, other.img2, aUnionFind) );
    };
    

    //////////////////////////////////////////////////////////////////////



    var colorString = function(aColor) {
	return ("rgb(" + 
		types.colorRed(aColor) + "," +
		types.colorGreen(aColor) + ", " + 
		types.colorBlue(aColor) + ")");
    };



    var RectangleImage = function(width, height, style, color) {
	BaseImage.call(this, width/2, height/2);
	this.width = width;
	this.height = height;
	this.style = style;
	this.color = color;
    };
    RectangleImage.prototype = heir(BaseImage.prototype);


    RectangleImage.prototype.render = function(ctx, x, y) {
	if (this.style.toString().toLowerCase() == "outline") {
	    ctx.save();
	    ctx.beginPath();
	    ctx.strokeStyle = colorString(this.color);
	    ctx.strokeRect(x, y, this.width, this.height);
	    ctx.closePath();
	    ctx.restore();
	} else {
	    ctx.save();
	    ctx.beginPath();

	    ctx.fillStyle = colorString(this.color);
	    ctx.fillRect(x, y, this.width, this.height);

	    ctx.closePath();
	    ctx.restore();
	}
    };

    RectangleImage.prototype.getWidth = function() {
	return this.width;
    };


    RectangleImage.prototype.getHeight = function() {
	return this.height;
    };

    RectangleImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof RectangleImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.width == other.width &&
		    this.height == other.height &&
		    this.style == other.style &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };


    //////////////////////////////////////////////////////////////////////
    
    var TextImage = function(msg, size, color) {
	BaseImage.call(this, 0, 0);
	this.msg = msg;
	this.size = size;
	this.color = color;
	this.font = this.size + "px Optimer";

	
	var canvas = world.Kernel.makeCanvas(0, 0);
 	var ctx = canvas.getContext("2d");
	ctx.font = this.font;
	var metrics = ctx.measureText(msg);

	this.width = metrics.width;
	// KLUDGE: I don't know how to get at the height.
	this.height = ctx.measureText("m").width + 20;

    }

    TextImage.prototype = heir(BaseImage.prototype);

    TextImage.prototype.render = function(ctx, x, y) {
	ctx.save();
	ctx.font = this.font;
	ctx.textAlign = 'left';
	ctx.textBaseline = 'top';
	ctx.fillStyle = colorString(this.color);
	ctx.fillText(this.msg, x, y);
	ctx.restore();
    };
    
    TextImage.prototype.getWidth = function() {
	return this.width;
    };


    TextImage.prototype.getHeight = function() {
	return this.height;
    };

    TextImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof TextImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.msg == other.msg &&
		    this.size == other.size &&
		    types.isEqual(this.color, other.color, aUnionFind) &&
		    this.font == other.font);
    };


    //////////////////////////////////////////////////////////////////////

    var CircleImage = function(radius, style, color) {
	BaseImage.call(this, radius, radius);
	this.radius = radius;
	this.style = style;
	this.color = color;
    }
    CircleImage.prototype = heir(BaseImage.prototype);

    CircleImage.prototype.render = function(ctx, x, y) {
	ctx.save();
	ctx.beginPath();
	ctx.arc(x + this.radius,
		y + this.radius,
		this.radius, 0, 2*Math.PI, false);
	ctx.closePath();
	if (this.style.toString().toLowerCase() == "outline") {
	    ctx.strokeStyle = colorString(this.color);
	    ctx.stroke();
	} else {
	    ctx.fillStyle = colorString(this.color);
	    ctx.fill();
	}

	ctx.restore();
    };
    
    CircleImage.prototype.getWidth = function() {
	return this.radius * 2;
    };

    CircleImage.prototype.getHeight = function() {
	return this.radius * 2;
    };

    CircleImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof CircleImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.radius == other.radius &&
		    this.style == other.style &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };



    //////////////////////////////////////////////////////////////////////


    // StarImage: fixnum fixnum fixnum color -> image
    var StarImage = function(points, outer, inner, style, color) {
	BaseImage.call(this,
		       Math.max(outer, inner),
		       Math.max(outer, inner));
	this.points = points;
	this.outer = outer;
	this.inner = inner;
	this.style = style;
	this.color = color;

	this.radius = Math.max(this.inner, this.outer);
    };

    StarImage.prototype = heir(BaseImage.prototype);

    var oneDegreeAsRadian = Math.PI / 180;

    // render: context fixnum fixnum -> void
    // Draws a star on the given context.
    // Most of this code here adapted from the Canvas tutorial at:
    // http://developer.apple.com/safari/articles/makinggraphicswithcanvas.html
    StarImage.prototype.render = function(ctx, x, y) {
	ctx.save();
	ctx.beginPath();
	for( var pt = 0; pt < (this.points * 2) + 1; pt++ ) {
	    var rads = ( ( 360 / (2 * this.points) ) * pt ) * oneDegreeAsRadian - 0.5;
	    var radius = ( pt % 2 == 1 ) ? this.outer : this.inner;
	    ctx.lineTo(x + this.radius + ( Math.sin( rads ) * radius ), 
		       y + this.radius + ( Math.cos( rads ) * radius ) );
	}
	ctx.closePath();
	if (this.style.toString().toLowerCase() == "outline") {
	    ctx.strokeStyle = colorString(this.color);
	    ctx.stroke();
	} else {
	    ctx.fillStyle = colorString(this.color);
	    ctx.fill();
	}

	ctx.restore();
    };
    
    // getWidth: -> fixnum
    StarImage.prototype.getWidth = function() {
	return this.radius * 2;
    };


    // getHeight: -> fixnum
    StarImage.prototype.getHeight = function() {
	return this.radius * 2;
    };

    StarImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof StarImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.points == other.points &&
		    this.outer == other.outer &&
		    this.inner == other.inner &&
		    this.style == other.style &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };




    //////////////////////////////////////////////////////////////////////
    //Triangle
    ///////
    var TriangleImage = function(side, style, color) {
	this.width = side;
	this.height = Math.ceil(side * Math.sqrt(3) / 2);

	BaseImage.call(this, Math.floor(this.width/2), Math.floor(this.height/2));
	this.side = side;
	this.style = style;
	this.color = color;
    }
    TriangleImage.prototype = heir(BaseImage.prototype);


    TriangleImage.prototype.render = function(ctx, x, y) {
	var width = this.getWidth();
	var height = this.getHeight();
	ctx.save();
	ctx.beginPath();
	ctx.moveTo(x + this.side/2, y);
	ctx.lineTo(x + width, y + height);
	ctx.lineTo(x, y + height);
	ctx.closePath();

	if (this.style.toString().toLowerCase() == "outline") {
	    ctx.strokeStyle = colorString(this.color);
	    ctx.stroke();
	}
	else {
	    ctx.fillStyle = colorString(this.color);
	    ctx.fill();
	}
	ctx.restore();
    };
    


    TriangleImage.prototype.getWidth = function() {
	return this.width;
    };

    TriangleImage.prototype.getHeight = function() {
	return this.height;
    };

    TriangleImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof TriangleImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.side == other.side &&
		    this.style == other.style &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };



    //////////////////////////////////////////////////////////////////////
    //Ellipse 
    var EllipseImage = function(width, height, style, color) {
	BaseImage.call(this, Math.floor(width/2), Math.floor(height/2));
	this.width = width;
	this.height = height;
	this.style = style;
	this.color = color;
    };

    EllipseImage.prototype = heir(BaseImage.prototype);

    
    EllipseImage.prototype.render = function(ctx, aX, aY) {
	ctx.save();
	ctx.beginPath();

	// Most of this code is taken from:
	// http://webreflection.blogspot.com/2009/01/ellipse-and-circle-for-canvas-2d.html
        var hB = (this.width / 2) * .5522848,
        vB = (this.height / 2) * .5522848,
        eX = aX + this.width,
        eY = aY + this.height,
        mX = aX + this.width / 2,
        mY = aY + this.height / 2;
        ctx.moveTo(aX, mY);
        ctx.bezierCurveTo(aX, mY - vB, mX - hB, aY, mX, aY);
        ctx.bezierCurveTo(mX + hB, aY, eX, mY - vB, eX, mY);
        ctx.bezierCurveTo(eX, mY + vB, mX + hB, eY, mX, eY);
        ctx.bezierCurveTo(mX - hB, eY, aX, mY + vB, aX, mY);
        ctx.closePath();
	if (this.style.toString().toLowerCase() == "outline") {
 	    ctx.strokeStyle = colorString(this.color);
	    ctx.stroke();
	}
	else {
 	    ctx.fillStyle = colorString(this.color);
	    ctx.fill();
	}


	ctx.restore();
    };
    
    EllipseImage.prototype.getWidth = function() {
	return this.width;
    };

    EllipseImage.prototype.getHeight = function() {
	return this.height;
    };

    EllipseImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof EllipseImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.width == other.width &&
		    this.height == other.height &&
		    this.style == other.style &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };


    //////////////////////////////////////////////////////////////////////
    //Line
    var LineImage = function(x, y, color) {
	BaseImage.call(this, 0, 0);
	this.x = x;
	this.y = y;
	this.color = color;
    }

    LineImage.prototype = heir(BaseImage.prototype);

    
    LineImage.prototype.render = function(ctx, xstart, ystart) {
	ctx.save();
	ctx.moveTo(xstart, ystart);
	ctx.lineTo((this.x + xstart),
		   (this.y + ystart));
	ctx.strokeStyle = colorString(this.color);
	ctx.stroke();
	ctx.restore();
    };
    

    LineImage.prototype.getWidth = function() {
	return (this.x + 1);
    };
    

    LineImage.prototype.getHeight = function() {
	return (this.y + 1);
    };

    LineImage.prototype.isEqual = function(other, aUnionFind) {
	    return (other instanceof LineImage &&
		    this.pinholeX == other.pinholeX &&
		    this.pinholeY == other.pinholeY &&
		    this.x == other.x &&
		    this.y == other.y &&
		    types.isEqual(this.color, other.color, aUnionFind));
    };





    //////////////////////////////////////////////////////////////////////
    // Effects

    /**
     * applyEffect: compound-effect -> (arrayof (world -> world))

     applyEffect applies all of the effects

     @param aCompEffect a compound effect is either a scheme list of
     compound effects or a single primitive effect */
    world.Kernel.applyEffect = function(aCompEffect) {
	    throw new Error('applyEffect: we are not currently using effects!');
/*
	if ( types.isEmpty(aCompEffect) ) {
    	    // Do Nothing
	} else if ( types.isPair(aCompEffect) ) {
    	    var results = world.Kernel.applyEffect(aCompEffect.first());
    	    return results.concat(world.Kernel.applyEffect(aCompEffect.rest()));
	} else {
	    var newResult = aCompEffect.run();
	    if (newResult) {
	    	return newResult;
	    }
	}
	return [];
*/
    }

    //////////////////////////////////////////////////////////////////////////




    // Color database
    var ColorDb = function() {
	this.colors = {};
    }
    ColorDb.prototype.put = function(name, color) {
	this.colors[name] = color;
    };

    ColorDb.prototype.get = function(name) {
	return this.colors[name.toString().toUpperCase()];
    };


    // FIXME: update toString to handle the primitive field values.

    var colorDb = new ColorDb();
    colorDb.put("ORANGE", types.color(255, 165, 0));
    colorDb.put("RED", types.color(255, 0, 0));
    colorDb.put("ORANGERED", types.color(255, 69, 0));
    colorDb.put("TOMATO", types.color(255, 99, 71));
    colorDb.put("DARKRED", types.color(139, 0, 0));
    colorDb.put("RED", types.color(255, 0, 0));
    colorDb.put("FIREBRICK", types.color(178, 34, 34));
    colorDb.put("CRIMSON", types.color(220, 20, 60));
    colorDb.put("DEEPPINK", types.color(255, 20, 147));
    colorDb.put("MAROON", types.color(176, 48, 96));
    colorDb.put("INDIAN RED", types.color(205, 92, 92));
    colorDb.put("INDIANRED", types.color(205, 92, 92));
    colorDb.put("MEDIUM VIOLET RED", types.color(199, 21, 133));
    colorDb.put("MEDIUMVIOLETRED", types.color(199, 21, 133));
    colorDb.put("VIOLET RED", types.color(208, 32, 144));
    colorDb.put("VIOLETRED", types.color(208, 32, 144));
    colorDb.put("LIGHTCORAL", types.color(240, 128, 128));
    colorDb.put("HOTPINK", types.color(255, 105, 180));
    colorDb.put("PALEVIOLETRED", types.color(219, 112, 147));
    colorDb.put("LIGHTPINK", types.color(255, 182, 193));
    colorDb.put("ROSYBROWN", types.color(188, 143, 143));
    colorDb.put("PINK", types.color(255, 192, 203));
    colorDb.put("ORCHID", types.color(218, 112, 214));
    colorDb.put("LAVENDERBLUSH", types.color(255, 240, 245));
    colorDb.put("SNOW", types.color(255, 250, 250));
    colorDb.put("CHOCOLATE", types.color(210, 105, 30));
    colorDb.put("SADDLEBROWN", types.color(139, 69, 19));
    colorDb.put("BROWN", types.color(132, 60, 36));
    colorDb.put("DARKORANGE", types.color(255, 140, 0));
    colorDb.put("CORAL", types.color(255, 127, 80));
    colorDb.put("SIENNA", types.color(160, 82, 45));
    colorDb.put("ORANGE", types.color(255, 165, 0));
    colorDb.put("SALMON", types.color(250, 128, 114));
    colorDb.put("PERU", types.color(205, 133, 63));
    colorDb.put("DARKGOLDENROD", types.color(184, 134, 11));
    colorDb.put("GOLDENROD", types.color(218, 165, 32));
    colorDb.put("SANDYBROWN", types.color(244, 164, 96));
    colorDb.put("LIGHTSALMON", types.color(255, 160, 122));
    colorDb.put("DARKSALMON", types.color(233, 150, 122));
    colorDb.put("GOLD", types.color(255, 215, 0));
    colorDb.put("YELLOW", types.color(255, 255, 0));
    colorDb.put("OLIVE", types.color(128, 128, 0));
    colorDb.put("BURLYWOOD", types.color(222, 184, 135));
    colorDb.put("TAN", types.color(210, 180, 140));
    colorDb.put("NAVAJOWHITE", types.color(255, 222, 173));
    colorDb.put("PEACHPUFF", types.color(255, 218, 185));
    colorDb.put("KHAKI", types.color(240, 230, 140));
    colorDb.put("DARKKHAKI", types.color(189, 183, 107));
    colorDb.put("MOCCASIN", types.color(255, 228, 181));
    colorDb.put("WHEAT", types.color(245, 222, 179));
    colorDb.put("BISQUE", types.color(255, 228, 196));
    colorDb.put("PALEGOLDENROD", types.color(238, 232, 170));
    colorDb.put("BLANCHEDALMOND", types.color(255, 235, 205));
    colorDb.put("MEDIUM GOLDENROD", types.color(234, 234, 173));
    colorDb.put("MEDIUMGOLDENROD", types.color(234, 234, 173));
    colorDb.put("PAPAYAWHIP", types.color(255, 239, 213));
    colorDb.put("MISTYROSE", types.color(255, 228, 225));
    colorDb.put("LEMONCHIFFON", types.color(255, 250, 205));
    colorDb.put("ANTIQUEWHITE", types.color(250, 235, 215));
    colorDb.put("CORNSILK", types.color(255, 248, 220));
    colorDb.put("LIGHTGOLDENRODYELLOW", types.color(250, 250, 210));
    colorDb.put("OLDLACE", types.color(253, 245, 230));
    colorDb.put("LINEN", types.color(250, 240, 230));
    colorDb.put("LIGHTYELLOW", types.color(255, 255, 224));
    colorDb.put("SEASHELL", types.color(255, 245, 238));
    colorDb.put("BEIGE", types.color(245, 245, 220));
    colorDb.put("FLORALWHITE", types.color(255, 250, 240));
    colorDb.put("IVORY", types.color(255, 255, 240));
    colorDb.put("GREEN", types.color(0, 255, 0));
    colorDb.put("LAWNGREEN", types.color(124, 252, 0));
    colorDb.put("CHARTREUSE", types.color(127, 255, 0));
    colorDb.put("GREEN YELLOW", types.color(173, 255, 47));
    colorDb.put("GREENYELLOW", types.color(173, 255, 47));
    colorDb.put("YELLOW GREEN", types.color(154, 205, 50));
    colorDb.put("YELLOWGREEN", types.color(154, 205, 50));
    colorDb.put("MEDIUM FOREST GREEN", types.color(107, 142, 35));
    colorDb.put("OLIVEDRAB", types.color(107, 142, 35));
    colorDb.put("MEDIUMFORESTGREEN", types.color(107, 142, 35));
    colorDb.put("DARK OLIVE GREEN", types.color(85, 107, 47));
    colorDb.put("DARKOLIVEGREEN", types.color(85, 107, 47));
    colorDb.put("DARKSEAGREEN", types.color(143, 188, 139));
    colorDb.put("LIME", types.color(0, 255, 0));
    colorDb.put("DARK GREEN", types.color(0, 100, 0));
    colorDb.put("DARKGREEN", types.color(0, 100, 0));
    colorDb.put("LIME GREEN", types.color(50, 205, 50));
    colorDb.put("LIMEGREEN", types.color(50, 205, 50));
    colorDb.put("FOREST GREEN", types.color(34, 139, 34));
    colorDb.put("FORESTGREEN", types.color(34, 139, 34));
    colorDb.put("SPRING GREEN", types.color(0, 255, 127));
    colorDb.put("SPRINGGREEN", types.color(0, 255, 127));
    colorDb.put("MEDIUM SPRING GREEN", types.color(0, 250, 154));
    colorDb.put("MEDIUMSPRINGGREEN", types.color(0, 250, 154));
    colorDb.put("SEA GREEN", types.color(46, 139, 87));
    colorDb.put("SEAGREEN", types.color(46, 139, 87));
    colorDb.put("MEDIUM SEA GREEN", types.color(60, 179, 113));
    colorDb.put("MEDIUMSEAGREEN", types.color(60, 179, 113));
    colorDb.put("AQUAMARINE", types.color(112, 216, 144));
    colorDb.put("LIGHTGREEN", types.color(144, 238, 144));
    colorDb.put("PALE GREEN", types.color(152, 251, 152));
    colorDb.put("PALEGREEN", types.color(152, 251, 152));
    colorDb.put("MEDIUM AQUAMARINE", types.color(102, 205, 170));
    colorDb.put("MEDIUMAQUAMARINE", types.color(102, 205, 170));
    colorDb.put("TURQUOISE", types.color(64, 224, 208));
    colorDb.put("LIGHTSEAGREEN", types.color(32, 178, 170));
    colorDb.put("MEDIUM TURQUOISE", types.color(72, 209, 204));
    colorDb.put("MEDIUMTURQUOISE", types.color(72, 209, 204));
    colorDb.put("HONEYDEW", types.color(240, 255, 240));
    colorDb.put("MINTCREAM", types.color(245, 255, 250));
    colorDb.put("ROYALBLUE", types.color(65, 105, 225));
    colorDb.put("DODGERBLUE", types.color(30, 144, 255));
    colorDb.put("DEEPSKYBLUE", types.color(0, 191, 255));
    colorDb.put("CORNFLOWERBLUE", types.color(100, 149, 237));
    colorDb.put("STEEL BLUE", types.color(70, 130, 180));
    colorDb.put("STEELBLUE", types.color(70, 130, 180));
    colorDb.put("LIGHTSKYBLUE", types.color(135, 206, 250));
    colorDb.put("DARK TURQUOISE", types.color(0, 206, 209));
    colorDb.put("DARKTURQUOISE", types.color(0, 206, 209));
    colorDb.put("CYAN", types.color(0, 255, 255));
    colorDb.put("AQUA", types.color(0, 255, 255));
    colorDb.put("DARKCYAN", types.color(0, 139, 139));
    colorDb.put("TEAL", types.color(0, 128, 128));
    colorDb.put("SKY BLUE", types.color(135, 206, 235));
    colorDb.put("SKYBLUE", types.color(135, 206, 235));
    colorDb.put("CADET BLUE", types.color(96, 160, 160));
    colorDb.put("CADETBLUE", types.color(95, 158, 160));
    colorDb.put("DARK SLATE GRAY", types.color(47, 79, 79));
    colorDb.put("DARKSLATEGRAY", types.color(47, 79, 79));
    colorDb.put("LIGHTSLATEGRAY", types.color(119, 136, 153));
    colorDb.put("SLATEGRAY", types.color(112, 128, 144));
    colorDb.put("LIGHT STEEL BLUE", types.color(176, 196, 222));
    colorDb.put("LIGHTSTEELBLUE", types.color(176, 196, 222));
    colorDb.put("LIGHT BLUE", types.color(173, 216, 230));
    colorDb.put("LIGHTBLUE", types.color(173, 216, 230));
    colorDb.put("POWDERBLUE", types.color(176, 224, 230));
    colorDb.put("PALETURQUOISE", types.color(175, 238, 238));
    colorDb.put("LIGHTCYAN", types.color(224, 255, 255));
    colorDb.put("ALICEBLUE", types.color(240, 248, 255));
    colorDb.put("AZURE", types.color(240, 255, 255));
    colorDb.put("MEDIUM BLUE", types.color(0, 0, 205));
    colorDb.put("MEDIUMBLUE", types.color(0, 0, 205));
    colorDb.put("DARKBLUE", types.color(0, 0, 139));
    colorDb.put("MIDNIGHT BLUE", types.color(25, 25, 112));
    colorDb.put("MIDNIGHTBLUE", types.color(25, 25, 112));
    colorDb.put("NAVY", types.color(36, 36, 140));
    colorDb.put("BLUE", types.color(0, 0, 255));
    colorDb.put("INDIGO", types.color(75, 0, 130));
    colorDb.put("BLUE VIOLET", types.color(138, 43, 226));
    colorDb.put("BLUEVIOLET", types.color(138, 43, 226));
    colorDb.put("MEDIUM SLATE BLUE", types.color(123, 104, 238));
    colorDb.put("MEDIUMSLATEBLUE", types.color(123, 104, 238));
    colorDb.put("SLATE BLUE", types.color(106, 90, 205));
    colorDb.put("SLATEBLUE", types.color(106, 90, 205));
    colorDb.put("PURPLE", types.color(160, 32, 240));
    colorDb.put("DARK SLATE BLUE", types.color(72, 61, 139));
    colorDb.put("DARKSLATEBLUE", types.color(72, 61, 139));
    colorDb.put("DARKVIOLET", types.color(148, 0, 211));
    colorDb.put("DARK ORCHID", types.color(153, 50, 204));
    colorDb.put("DARKORCHID", types.color(153, 50, 204));
    colorDb.put("MEDIUMPURPLE", types.color(147, 112, 219));
    colorDb.put("CORNFLOWER BLUE", types.color(68, 64, 108));
    colorDb.put("MEDIUM ORCHID", types.color(186, 85, 211));
    colorDb.put("MEDIUMORCHID", types.color(186, 85, 211));
    colorDb.put("MAGENTA", types.color(255, 0, 255));
    colorDb.put("FUCHSIA", types.color(255, 0, 255));
    colorDb.put("DARKMAGENTA", types.color(139, 0, 139));
    colorDb.put("VIOLET", types.color(238, 130, 238));
    colorDb.put("PLUM", types.color(221, 160, 221));
    colorDb.put("LAVENDER", types.color(230, 230, 250));
    colorDb.put("THISTLE", types.color(216, 191, 216));
    colorDb.put("GHOSTWHITE", types.color(248, 248, 255));
    colorDb.put("WHITE", types.color(255, 255, 255));
    colorDb.put("WHITESMOKE", types.color(245, 245, 245));
    colorDb.put("GAINSBORO", types.color(220, 220, 220));
    colorDb.put("LIGHT GRAY", types.color(211, 211, 211));
    colorDb.put("LIGHTGRAY", types.color(211, 211, 211));
    colorDb.put("SILVER", types.color(192, 192, 192));
    colorDb.put("GRAY", types.color(190, 190, 190));
    colorDb.put("DARK GRAY", types.color(169, 169, 169));
    colorDb.put("DARKGRAY", types.color(169, 169, 169));
    colorDb.put("DIM GRAY", types.color(105, 105, 105));
    colorDb.put("DIMGRAY", types.color(105, 105, 105));
    colorDb.put("BLACK", types.color(0, 0, 0));





    ///////////////////////////////////////////////////////////////
    // Exports

    world.Kernel.isImage = isImage;
    world.Kernel.isScene = isScene;
    world.Kernel.isColor = function(thing) {
	return (types.isColor(thing) ||
		((types.isString(thing) || types.isSymbol(thing)) &&
		 typeof(colorDb.get(thing)) != 'undefined'));
    };
    world.Kernel.colorDb = colorDb;

    world.Kernel.sceneImage = function(width, height, children) {
	   return new SceneImage(width, height, children);
    };
    world.Kernel.circleImage = function(radius, style, color) {
	    return new CircleImage(radius, style, color);
    };
    world.Kernel.starImage = function(points, outer, inner, style, color) {
	    return new StarImage(points, outer, inner, style, color);
    };
    world.Kernel.rectangleImage = function(width, height, style, color) {
	    return new RectangleImage(width, height, style, color);
    };
    world.Kernel.triangleImage = function(side, style, color) {
	    return new TriangleImage(side, style, color);
    };
    world.Kernel.ellipseImage = function(width, height, style, color) {
	    return new EllipseImage(width, height, style, color);
    };
    world.Kernel.lineImage = function(x, y, color) {
	    return new LineImage(x, y, color);
    };
    world.Kernel.overlayImage = function(img1, img2, shiftX, shiftY) {
	    return new OverlayImage(img1, img2, shiftX, shiftY);
    };
    world.Kernel.textImage = function(msg, size, color) {
	    return new TextImage(msg, size, color);
    };
    world.Kernel.fileImage = function(path) {
	    return FileImage.makeInstance(path);
    };


    world.Kernel.isSceneImage = function(x) { return x instanceof SceneImage; };
    world.Kernel.isCircleImage = function(x) { return x instanceof CircleImage; };
    world.Kernel.isStarImage = function(x) { return x instanceof StarImage; };
    world.Kernel.isRectangleImage = function(x) { return x instanceof RectangleImage; };
    world.Kernel.isTriangleImage = function(x) { return x instanceof TriangleImage; };
    world.Kernel.isEllipseImage = function(x) { return x instanceof EllipseImage; };
    world.Kernel.isLineImage = function(x) { return x instanceof LineImage; };
    world.Kernel.isOverlayImage = function(x) { return x instanceof OverlayImage; };
    world.Kernel.isTextImage = function(x) { return x instanceof TextImage; };
    world.Kernel.isFileImage = function(x) { return x instanceof FileImage; };





})();
