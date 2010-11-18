/************************
 *** Image Primitives ***
 ************************/

var world = {};
world.Kernel = STATE.invokedModules["mzscheme-vm/world/kernel"].lookup("kernel");



//////////////////////////////////////////////////////////////////////
// Helpers

// FIXME: tree-prune the helpers.  A lot of this is copy-pasted from
// primitive.js.


var PrimProc = types.PrimProc;
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

var isImage = world.Kernel.isImage;
var isScene = world.Kernel.isScene;
var isColor = world.Kernel.isColor;
var colorDb = world.Kernel.colorDb;
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

var isJsObject = function(x) {
	return isJsValue(x) && typeof(x.val) == 'object';
};

var isJsFunction = function(x) {
	return isJsValue(x) && typeof(x.val) == 'function';
};


var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
		f.call(null, arr[i], i);
	}
}

//var throwCheckError = helpers.throwCheckError;
var check = helpers.check;

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

var checkListOf = helpers.checkListOf;

var checkListOfLength = function(lst, n, functionName, position, args) {
	if ( !isList(lst) || (length(lst) < n) ) {
		helpers.throwCheckError([functionName,
					 'list with ' + n + ' or more elements',
					 helpers.ordinalize(position),
					 lst],
					position,
					args);
	}
}

var checkAllSameLength = function(lists, functionName, args) {
	if (lists.length == 0)
		return;
	
	var len = length(lists[0]);
	arrayEach(lists,
		  function(lst, i) {
			if (length(lst) != len) {
				var argsStr = helpers.map(function(x) { return " ~s"; }, args).join('');
				var msg = helpers.format(functionName + ': all lists must have the same size; arguments were:' + argsStr,
							 args);
				raise( types.incompleteExn(types.exnFailContract, msg, []) );
			}
		});
}




//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////




EXPORTS['image?'] = new PrimProc('image?', 1, false, false, isImage);

EXPORTS['image=?'] =
    new PrimProc('image=?',
		 2,
		 false, false,
		 function(img1, img2) {
		 	check(img1, isImage, 'image=?', 'image', 1);
			check(img2, isImage, 'image=?', 'image', 2);
			return isEqual(img1, img2);
		 });


EXPORTS['make-color'] =
    new PrimProc('make-color',
		 3,
		 false, false,
		 function(r, g, b) {
		 	check(r, isByte, 'make-color', 'number between 0 and 255', 1, arguments);
		 	check(g, isByte, 'make-color', 'number between 0 and 255', 2, arguments);
		 	check(b, isByte, 'make-color', 'number between 0 and 255', 3, arguments);

			return types.color(jsnums.toFixnum(r),
					   jsnums.toFixnum(g),
					   jsnums.toFixnum(b));
		 });

EXPORTS['color-red'] =
    new PrimProc('color-red',
		 1,
		 false, false,
		 function(col) {
		 	check(col, types.isColor, 'color-red', 'color', 1);
			return types.colorRed(col);
		 });

EXPORTS['color-green'] =
    new PrimProc('color-green',
		 1,
		 false, false,
		 function(col) {
		 	check(col, types.isColor, 'color-green', 'color', 1);
			return types.colorGreen(col);
		 });

EXPORTS['color-blue'] =
    new PrimProc('color-blue',
		 1,
		 false, false,
		 function(col) {
		 	check(col, types.isColor, 'color-blue', 'color', 1);
			return types.colorBlue(col);
		 });


EXPORTS['empty-scene'] =
    new PrimProc('empty-scene',
		 2,
		 false, false,
		 function(width, height) {
		 	check(width, isNonNegativeReal, 'empty-scene', 'non-negative number', 1, arguments);
			check(height, isNonNegativeReal, 'empty-scene', 'non-negative number', 2, arguments);
		     return world.Kernel.sceneImage(jsnums.toFixnum(width), jsnums.toFixnum(height), [], true);
		 });


EXPORTS['place-image'] =
    new PrimProc('place-image',
		 4,
		 false, false,
		 function(picture, x, y, background) {
			check(picture, isImage, "place-image", "image", 1, arguments);
			check(x, isReal, "place-image", "real", 2, arguments);
			check(y, isReal, "place-image", "real", 3, arguments);
			check(background, function(x) { return isScene(x) || isImage(x) },
			      "place-image", "image", 4, arguments);
			if (isScene(background)) {
			    return background.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
			} else {
			    var newScene = world.Kernel.sceneImage(background.getWidth(),
								   background.getHeight(),
								   [], 
								   false);
			    newScene = newScene.add(background.updatePinhole(0, 0), 0, 0);
			    newScene = newScene.add(picture, jsnums.toFixnum(x), jsnums.toFixnum(y));
			    return newScene;
			}
		 });


EXPORTS['put-pinhole'] =
    new PrimProc('put-pinhole',
		 3,
		 false, false,
		 function(img, x, y) {
			check(img, isImage, "put-pinhole", "image", 1, arguments);
			check(x, isReal, "put-pinhole", "real", 2, arguments);
			check(y, isReal, "put-pinhole", "real", 3, arguments);
			return img.updatePinhole(jsnums.toFixnum(x), jsnums.toFixnum(y));
    		 });


EXPORTS['circle'] =
    new PrimProc('circle',
		 3,
		 false, false,
		 function(aRadius, aStyle, aColor) {
			check(aRadius, isNonNegativeReal, "circle", "non-negative number", 1, arguments);
			check(aStyle, isStyle, "circle", "style", 2, arguments);
			check(aColor, isColor, "circle", "color", 3, arguments);


			if (colorDb.get(aColor)) {
				aColor = colorDb.get(aColor);
			}
		     return world.Kernel.circleImage(jsnums.toFixnum(aRadius), aStyle.toString(), aColor);
		 });


EXPORTS['star'] =
    new PrimProc('star',
		 5,
		 false, false,
		 function(aPoints, anOuter, anInner, aStyle, aColor) {
			check(aPoints, function(x) { return isNatural(x) && jsnums.greaterThanOrEqual(x, 3); },
			      "star", "positive integer greater than or equal to 3", 1, arguments);
			check(anOuter, function(x) { return isReal(x) && jsnums.greaterThan(x, 0); },
			      "star", "positive number", 2, arguments);
			check(anInner, function(x) { return isReal(x) && jsnums.greaterThan(x, 0); },
			      "star", "positive number", 2, arguments);
			check(aStyle, isStyle, "star", "style", 4, arguments);
			check(aColor, isColor, "star", "color", 5, arguments);

			if (colorDb.get(aColor)) {
				aColor = colorDb.get(aColor);
			}
			return world.Kernel.starImage(jsnums.toFixnum(aPoints),
						      jsnums.toFixnum(anOuter),
						      jsnums.toFixnum(anInner),
						      aStyle.toString(),
						      aColor);
		 });


EXPORTS['nw:rectangle'] =
    new PrimProc('nw:rectangle',
		 4,
		 false, false,
		 function(w, h, s, c) {
			check(w, isNonNegativeReal, "nw:rectangle", "non-negative number", 1, arguments);
			check(h, isNonNegativeReal, "nw:rectangle", "non-negative number", 2, arguments);
			check(s, isStyle, "nw:rectangle", "style", 3, arguments);
			check(c, isColor, "nw:rectangle", "color", 4, arguments);

			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
			var aRect = world.Kernel.rectangleImage(jsnums.toFixnum(w),
								jsnums.toFixnum(h),
								s.toString(), c);
			return aRect.updatePinhole(0, 0);
		 });


EXPORTS['rectangle'] =
    new PrimProc('rectangle',
		 4,
		 false, false,
		 function(w, h, s, c) {
			check(w, isNonNegativeReal, "rectangle", "non-negative number", 1, arguments);
			check(h, isNonNegativeReal, "rectangle", "non-negative number", 2, arguments);
			check(s, isStyle, "rectangle", "style", 3, arguments);
			check(c, isColor, "rectangle", "color", 4, arguments);

			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
			return world.Kernel.rectangleImage(jsnums.toFixnum(w),
							   jsnums.toFixnum(h),
							   s.toString(), c);
		 });


EXPORTS['triangle'] =
    new PrimProc('triangle',
		 3,
		 false, false,
		 function(r, s, c) {
			check(r, isNonNegativeReal, "triangle", "non-negative number", 1, arguments);
			check(s, isStyle, "triangle", "style", 2, arguments);
			check(c, isColor, "triangle", "color", 3, arguments);
			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
		     return world.Kernel.triangleImage(jsnums.toFixnum(r), s.toString(), c);
		 });


EXPORTS['ellipse'] =
    new PrimProc('ellipse',
		 4,
		 false, false,
		 function(w, h, s, c) {
			check(w, isNonNegativeReal, "ellipse", "non-negative number", 1, arguments);
			check(h, isNonNegativeReal, "ellipse", "non-negative number", 2, arguments);
			check(s, isStyle, "ellipse", "string", 3, arguments);
			check(c, isColor, "ellipse", "color", 4, arguments);
			
			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
			return world.Kernel.ellipseImage(jsnums.toFixnum(w),
							 jsnums.toFixnum(h),
							 s.toString(),
							 c);
		 });


EXPORTS['line'] =
    new PrimProc('line',
		 3,
		 false, false,
		 function(x, y, c) {
			check(x, isReal, "line", "finite real number", 1, arguments);
			check(y, isReal, "line", "finite real number", 2, arguments);
			check(c, isColor, "line", "color", 3, arguments);
			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
			var line = world.Kernel.lineImage(jsnums.toFixnum(x),
							  jsnums.toFixnum(y),
							  c);
		        return line;
		 });


EXPORTS['overlay'] =
    new PrimProc('overlay',
		 2,
		 true, false,
		 function(img1, img2, restImages) {
			check(img1, isImage, "overlay", "image", 1, arguments);
			check(img2, isImage, "overlay", "image", 2, arguments);
			arrayEach(restImages, function(x, i) { check(x, isImage, "overlay", "image", i+3); }, arguments);

			var img = world.Kernel.overlayImage(img1, img2, 0, 0);
			for (var i = 0; i < restImages.length; i++) {
				img = world.Kernel.overlayImage(img, restImages[i], 0, 0);
			}
			return img;
		 });


EXPORTS['overlay/xy'] =
    new PrimProc('overlay/xy',
		 4,
		 false, false,
		 function(img1, deltaX, deltaY, img2) {
			check(img1, isImage, "overlay/xy", "image", 1, arguments);
			check(deltaX, isReal, "overlay/xy", "finite real number", 2, arguments);
			check(deltaY, isReal, "overlay/xy", "finite real number", 3, arguments);
			check(img2, isImage, "overlay/xy", "image", 4, arguments);

		     return world.Kernel.overlayImage(img1.updatePinhole(0, 0),
						      img2.updatePinhole(0, 0),
						      jsnums.toFixnum(deltaX),
						      jsnums.toFixnum(deltaY));
		 });


EXPORTS['underlay'] =
    new PrimProc('underlay',
		 2,
		 true, false,
		 function(img1, img2, restImages) {
			check(img1, isImage, "underlay", "image", 1, arguments);
			check(img2, isImage, "underlay", "image", 2, arguments);
			arrayEach(restImages, function(x, i) { check(x, isImage, "underlay", "image", i+3); }, arguments);

			var img = world.Kernel.overlayImage(img2, img1, 0, 0);
			for (var i = 0; i < restImages.length; i++) {
				img = world.Kernel.overlayImage(restImages[i], img, 0, 0);
			}
			return img;
		 });


EXPORTS['underlay/xy'] =
    new PrimProc('underlay/xy',
		 4,
		 false, false,
		 function(img1, deltaX, deltaY, img2) {
			check(img1, isImage, "underlay/xy", "image", 1, arguments);
			check(deltaX, isReal, "underlay/xy", "finite real number", 2, arguments);
			check(deltaY, isReal, "underlay/xy", "finite real number", 3, arguments);
			check(img2, isImage, "underlay/xy", "image", 4, arguments);

		     return world.Kernel.overlayImage(img2.updatePinhole(0, 0), 
						      img1.updatePinhole(0, 0),
						      -jsnums.toFixnum(deltaX),
						      -jsnums.toFixnum(deltaY));
		 });


EXPORTS['rotate'] =
new PrimProc('rotate',
			 2,
			 false, false,
			 function(angle, img) {
			 check(angle, isReal, "rotate", "finite real number", 1, arguments);
			 check(img, isImage, "rotate", "image", 2, arguments);
			 
			     return world.Kernel.rotateImage(jsnums.toFixnum(angle), img);
			 });

EXPORTS['scale/xy'] =
new PrimProc('scale/xy',
			 3,
			 false, false,
			 function(xFactor, yFactor, img) {
			 check(xFactor, isReal, "scale/xy", "finite real number", 1, arguments);
			 check(yFactor, isReal, "scale/xy", "finite real number", 2, arguments);
			 check(img, isImage, "scale/xy", "image", 3, arguments);
			 
			 return world.Kernel.scaleImage(jsnums.toFixnum(xFactor), 
							jsnums.toFixnum(yFactor),
							img);

			 });

EXPORTS['scale'] =
new PrimProc('scale',
			 2,
			 false, false,
			 function(factor, img) {
			 check(factor, isReal, "scale", "finite real number", 1, arguments);
			 check(img, isImage, "scale", "image", 2, arguments);
			 
			 return world.Kernel.scaleImage(jsnums.toFixnum(factor),
							jsnums.toFixnum(factor),
							img);
			 });


EXPORTS['text'] =
    new PrimProc('text',
		 3,
		 false, false,
		 function(aString, aSize, aColor) {
			check(aString, isString, "text", "string", 1, arguments);
		     check(aSize, function(x) { return isNatural(x) && jsnums.greaterThan(x, 0) && isByte(x); },
			      "text", "exact integer between 1 and 255", 2, arguments);
			check(aColor, isColor, "text", "color", 3, arguments);

			if (colorDb.get(aColor)) {
				aColor = colorDb.get(aColor);
			}
		        return world.Kernel.textImage(aString.toString(), jsnums.toFixnum(aSize), aColor);
		 });


EXPORTS['image-url'] =
    new PrimProc('image-url',
		 1,
		 false, true,
		 function(state, path) {
		     check(path, isString, "image-url", "string", 1);
		     return types.internalPause(function(caller, success, fail) {
			 var rawImage = new Image();
			 rawImage.onload = function() {
			     success(world.Kernel.fileImage(
				 path.toString(),
				 rawImage));
			 };
			 rawImage.onerror = function(e) {
			     fail(types.schemeError(types.incompleteExn(
					types.exnFail,
					" (unable to load: " + path + ")",
					[])));
			 };
			 rawImage.src = path.toString();
		     });
		 });


EXPORTS['image-width'] =
    new PrimProc('image-width',
		 1,
		 false, false,
		 function(img) {
		 	check(img, isImage, 'image-width', 'image', 1);
			return img.getWidth();
		 });


EXPORTS['image-height'] =
    new PrimProc('image-height',
		 1,
		 false, false,
		 function(img) {
		 	check(img, isImage, 'image-height', 'image', 1);
			return img.getHeight();
		 });



