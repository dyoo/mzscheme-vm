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
var CasePrimitive = types.CasePrimitive;
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

var isAngle = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0) && jsnums.lessThan(x, 360);
};

var isSideCount = function(x) {
	return isInteger(x) && jsnums.greaterThanOrEqual(x, 3);
};
var isStepCount = function(x) {
	return isInteger(x) && jsnums.greaterThanOrEqual(x, 1);
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
var isFontFamily = function(x){
	return ((isString(x) || isSymbol(x)) &&
			(x.toString().toLowerCase() == "default" ||
			 x.toString().toLowerCase() == "decorative" ||
			 x.toString().toLowerCase() == "roman" ||
			 x.toString().toLowerCase() == "script" ||
			 x.toString().toLowerCase() == "swiss" ||
			 x.toString().toLowerCase() == "modern" ||
			 x.toString().toLowerCase() == "symbol" ||
			 x.toString().toLowerCase() == "system"))
	|| !x;		// false is also acceptable
};
var isFontStyle = function(x){
	return ((isString(x) || isSymbol(x)) &&
			(x.toString().toLowerCase() == "normal" ||
			 x.toString().toLowerCase() == "italic" ||
			 x.toString().toLowerCase() == "slant"))
	|| !x;		// false is also acceptable
};
var isFontWeight = function(x){
	return ((isString(x) || isSymbol(x)) &&
			(x.toString().toLowerCase() == "normal" ||
			 x.toString().toLowerCase() == "bold" ||
			 x.toString().toLowerCase() == "light"))
	|| !x;		// false is also acceptable
};
var colorDb = world.Kernel.colorDb;
var isMode = function(x) {
	return ((isString(x) || isSymbol(x)) &&
		(x.toString().toLowerCase() == "solid" ||
		 x.toString().toLowerCase() == "outline"));
};

var isPlaceX = function(x) {
	return ((isString(x) || isSymbol(x)) &&
			(x.toString().toLowerCase() == "left"  ||
			 x.toString().toLowerCase() == "right" ||
			 x.toString().toLowerCase() == "center" ||
			 x.toString().toLowerCase() == "middle"));
};

var isPlaceY = function(x) {
	return ((isString(x) || isSymbol(x)) &&
			(x.toString().toLowerCase() == "top"	  ||
			 x.toString().toLowerCase() == "bottom"   ||
			 x.toString().toLowerCase() == "baseline" ||
			 x.toString().toLowerCase() == "center"   ||
			 x.toString().toLowerCase() == "middle"));
};


var isAssocList = function(x) {
	return isPair(x) && isPair(x.rest) && isEmpty(x.rest.rest);
};


var isCompoundEffect = function(x) {
	return ( types.isEffect(x) || isListOf(x, isCompoundEffect) );
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


EXPORTS['place-image/align'] =
new PrimProc('place-image/align',
			 6,
			 false, false,
			 function(img, x, y, placeX, placeY, background) {
			 check(img,		isImage,	"place-image/align", "image",	1, arguments);
			 check(x,		isReal,		"place-image/align", "real",	2, arguments);
			 check(y,		isReal,		"place-image/align", "real",	3, arguments);
			 check(placeX,	isPlaceX,	"place-image/align", "x-place", 4, arguments);
			 check(placeY,	isPlaceY,	"place-image/align", "y-place", 5, arguments);
			 check(background, function(x) { return isScene(x) || isImage(x) },
										"place-image/align", "image",	6, arguments);
			 
			 // calculate x and y based on placeX and placeY
			 if		 (placeX == "left"  ) x = x + img.pinholeX;
			 else if (placeX == "right" ) x = x - img.pinholeX;
			 if		 (placeY == "top"   ) y = y + img.pinholeY;
			 else if (placeY == "bottom") y = y - img.pinholeY;

			 if (isScene(background)) {
			 return background.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
			 } else {
			 var newScene = world.Kernel.sceneImage(background.getWidth(),
													background.getHeight(),
													[], 
													false);
			 newScene = newScene.add(background.updatePinhole(0, 0), 0, 0);
			 newScene = newScene.add(img, jsnums.toFixnum(x), jsnums.toFixnum(y));
			 return newScene;
			 }
			 });


EXPORTS['scene+line'] =
new PrimProc('scene+line',
			 6,
			 false, false,
			 function(img, x1, y1, x2, y2, c) {
			 check(img,		isImage,	"scene+line", "image",				1, arguments);
			 check(x1,		isReal,		"scene+line", "finite real number", 2, arguments);
			 check(y1,		isReal,		"scene+line", "finite real number", 3, arguments);
			 check(x2,		isReal,		"scene+line", "finite real number", 4, arguments);
			 check(y2,		isReal,		"scene+line", "finite real number", 5, arguments);
			 check(c,		isColor,	"scene+line", "color",				6, arguments);
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
			 // make a scene containing the image
		     newScene = world.Kernel.sceneImage(jsnums.toFixnum(img.getWidth()), 
												jsnums.toFixnum(img.getHeight()), 
												[],
												true);
			 newScene = newScene.add(img.updatePinhole(0, 0), 0, 0);
			 // make an image containing the line
			 line = world.Kernel.lineImage(jsnums.toFixnum(x2-x1),
										   jsnums.toFixnum(y2-y1),
										   c,
										   false);
			 // add the line to scene, offset by the original amount
			 return newScene.add(line, jsnums.toFixnum(x1), jsnums.toFixnum(y1));
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
			check(aStyle, isMode, "circle", "style", 2, arguments);
			check(aColor, isColor, "circle", "color", 3, arguments);


			if (colorDb.get(aColor)) {
				aColor = colorDb.get(aColor);
			}
		     return world.Kernel.circleImage(jsnums.toFixnum(aRadius), aStyle.toString(), aColor);
		 });


EXPORTS['star'] =
    new CasePrimitive(
	'star',
	// implementation to match htdp/image
	[new PrimProc('star',
		      5,
		      false, false,		      
		      function(n, outer, inner, m, c) {
			  check(n, isSideCount, "star", 
				"positive integer greater than or equal to 3", 
				1, arguments);
			  check(outer, isNonNegativeReal, "star", 
				"positive number", 
				2, arguments);
			  check(inner, 
				isNonNegativeReal, "star",
				"positive number", 3, arguments);
			  check(m, isMode, "star", "style", 4, arguments);
			  check(c, isColor, "star", "color", 5, arguments);
			  if (colorDb.get(c)) {
			      c = colorDb.get(c);
			  }
			  return world.Kernel.starImage(jsnums.toFixnum(n),
							jsnums.toFixnum(outer),
							jsnums.toFixnum(inner),
							m.toString(),
							c);
		      }),
	 // implementation to match 2htdp/image
	 new PrimProc('star', 
		      3,
		      false, false,
		      function(sideLength, mode, color) {
			  check(sideLength, isNonNegativeReal,
				"star", "non-negative number", 1, arguments);
			  check(mode, isMode, "star", "style", 2, arguments);
			  check(color, isColor, "star", "color", 3, arguments);
			  if (colorDb.get(color)) {
			      color = colorDb.get(color);
			  }
			  return world.Kernel.polygonImage(jsnums.toFixnum(sideLength), 
							   jsnums.toFixnum(5), 
							   jsnums.toFixnum(2), 
							   mode.toString(), 
							   color);
		      })]);



EXPORTS['radial-star'] =
new PrimProc('radial-star',
			 5,
			 false, false,
			 function(aPoints, anOuter, anInner, aStyle, aColor) {
			 check(aPoints, function(x) { return isNatural(x) && jsnums.greaterThanOrEqual(x, 2); },
									"radial-star", "positive integer greater than or equal to 2", 1, arguments);
			 check(anOuter, function(x) { return isReal(x) && jsnums.greaterThan(x, 0); },
									"radial-star", "positive number", 2, arguments);
			 check(anInner, function(x) { return isReal(x) && jsnums.greaterThan(x, 0); },
									"radial-star", "positive number", 2, arguments);
			 check(aStyle, isMode, "radial-star", "style", 4, arguments);
			 check(aColor, isColor, "radial-star", "color", 5, arguments);
			 
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
			check(s, isMode, "nw:rectangle", "style", 3, arguments);
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
			check(s, isMode, "rectangle", "style", 3, arguments);
			check(c, isColor, "rectangle", "color", 4, arguments);

			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
			return world.Kernel.rectangleImage(jsnums.toFixnum(w),
							   jsnums.toFixnum(h),
							   s.toString(), c);
		 });

EXPORTS['regular-polygon'] =
new PrimProc('regular-polygon',
			 4,
			 false, false,
			 function(length, count, s, c) {
			 check(length,	isNonNegativeReal,	"regular-polygon", "non-negative number", 1, arguments);
			 check(count,	isSideCount,		"regular-polygon", "positive integer greater than or equal to 3", 2, arguments);
			 check(s,		isMode, "regular-polygon", "style", 3, arguments);
			 check(c,		isColor, "regular-polygon", "color", 4, arguments);
			 
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
			 return world.Kernel.polygonImage(jsnums.toFixnum(length), 
											  jsnums.toFixnum(count), 
											  jsnums.toFixnum(1), 
											  s.toString(), 
											  c);
			 });

EXPORTS['star-polygon'] =
new PrimProc('star-polygon',
			 5,
			 false, false,
			 function(length, count, step, s, c) {
			 check(length,	isNonNegativeReal,	"star-polygon", "non-negative number", 1, arguments);
			 check(count,	isSideCount,		"star-polygon", "positive integer greater than or equal to 3", 2, arguments);
			 check(step,	isStepCount,		"star-polygon", "positive integer greater than or equal to 1", 3, arguments);
			 check(s,		isMode,				"star-polygon", "style", 4, arguments);
			 check(c,		isColor,			"star-polygon", "color", 5, arguments);
			 
			 if (colorDb.get(c)) {
				c = colorDb.get(c);
			 }
			 return world.Kernel.polygonImage(jsnums.toFixnum(length), 
											  jsnums.toFixnum(count), 
											  jsnums.toFixnum(step), 
											  s.toString(), 
											  c);
			 });

EXPORTS['rhombus'] =
new PrimProc('rhombus',
			 4,
			 false, false,
			 function(l, a, s, c) {
			 check(l, isNonNegativeReal, "rhombus", "non-negative number", 1, arguments);
			 check(a, isNonNegativeReal, "rhombus", "non-negative number", 2, arguments);
			 check(s, isMode, "rhombus", "style", 3, arguments);
			 check(c, isColor, "rhombus", "color", 4, arguments);
			 
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
			 return world.Kernel.rhombusImage(jsnums.toFixnum(l), jsnums.toFixnum(a), s.toString(), c);
			 });

EXPORTS['square'] =
new PrimProc('square',
			 3,
			 false, false,
			 function(l, s, c) {
			 check(l, isNonNegativeReal, "square", "non-negative number", 1, arguments);
			 check(s, isMode, "square", "style", 3, arguments);
			 check(c, isColor, "square", "color", 4, arguments);
			 
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
			 return world.Kernel.squareImage(jsnums.toFixnum(l), s.toString(), c);
			 });

EXPORTS['triangle'] =
    new PrimProc('triangle',
		 3,
		 false, false,
		 function(s, m, c) {
			check(s, isNonNegativeReal, "triangle", "non-negative number", 1, arguments);
			check(m, isMode, "triangle", "style", 2, arguments);
			check(c, isColor, "triangle", "color", 3, arguments);
			if (colorDb.get(c)) {
				c = colorDb.get(c);
			}
		     return world.Kernel.triangleImage(jsnums.toFixnum(s), 
						       60, 
						       m.toString(),
						       c);
		 });


EXPORTS['right-triangle'] =
new PrimProc('right-triangle',
			 4,
			 false, false,
			 function(side1, side2, s, c) {
			 check(side1, isNonNegativeReal, "right-triangle", "non-negative number", 1, arguments);
			 check(side2, isNonNegativeReal, "right-triangle", "non-negative number", 2, arguments);
			 check(s, isMode, "right-triangle", "style", 3, arguments);
			 check(c, isColor, "right-triangle", "color", 4, arguments);
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
		     return world.Kernel.rightTriangleImage(jsnums.toFixnum(side1), jsnums.toFixnum(side2), s.toString(), c);
			 });


EXPORTS['isosceles-triangle'] =
new PrimProc('isosceles-triangle',
			 4,
			 false, false,
			 function(side, angle, s, c) {
			 check(side, isNonNegativeReal, "isosceles-triangle", "non-negative number", 1, arguments);
			 check(angle, isAngle, "isosceles-triangle", "finite real number between 0 and 360", 2, arguments);
			 check(s, isMode, "isosceles-triangle", "style", 3, arguments);
			 check(c, isColor, "isosceles-triangle", "color", 4, arguments);
			 if (colorDb.get(c)) {
			 c = colorDb.get(c);
			 }
		     return world.Kernel.triangleImage(jsnums.toFixnum(side), jsnums.toFixnum(angle), s.toString(), c);
			 });


EXPORTS['ellipse'] =
    new PrimProc('ellipse',
		 4,
		 false, false,
		 function(w, h, s, c) {
			check(w, isNonNegativeReal, "ellipse", "non-negative number", 1, arguments);
			check(h, isNonNegativeReal, "ellipse", "non-negative number", 2, arguments);
			check(s, isMode, "ellipse", "string", 3, arguments);
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


EXPORTS['add-line'] =
new PrimProc('add-line',
			 6,
			 false, false,
			 function(img, x1, y1, x2, y2, c) {
			 check(img, isImage,	"add-line", "image",			  1, arguments);
			 check(x1,	isReal,		"add-line", "finite real number", 2, arguments);
			 check(y1,	isReal,		"add-line", "finite real number", 3, arguments);
			 check(x2,	isReal,		"add-line", "finite real number", 4, arguments);
			 check(y2,	isReal,		"add-line", "finite real number", 5, arguments);
			 check(c,	isColor,	"add-line", "color",			  6, arguments);
			 if (colorDb.get(c)) {
				c = colorDb.get(c);
			 }
			 line = world.Kernel.lineImage(jsnums.toFixnum(x2-x1),
										   jsnums.toFixnum(y2-y1),
										   c,
										   true);
			 return world.Kernel.overlayImage(line, img, "middle", "middle");
			 });


EXPORTS['overlay'] =
    new PrimProc('overlay',
		 2,
		 true, false,
		 function(img1, img2, restImages) {
			check(img1, isImage, "overlay", "image", 1, arguments);
			check(img2, isImage, "overlay", "image", 2, arguments);
			arrayEach(restImages, function(x, i) { check(x, isImage, "overlay", "image", i+3); }, arguments);

			var img = world.Kernel.overlayImage(img1, img2, "middle", "middle");
			for (var i = 0; i < restImages.length; i++) {
				img = world.Kernel.overlayImage(img, restImages[i], "middle", "middle");
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


EXPORTS['overlay/align'] =
new PrimProc('overlay/align',
			 4,
			 true, false,
			 function(placeX, placeY, img1, img2, restImages) {
			 check(placeX, isPlaceX, "overlay/align", "x-place", 1, arguments);
			 check(placeY, isPlaceY, "overlay/align", "y-place", 2, arguments);
			 check(img1, isImage, "overlay/align", "image", 3, arguments);
			 check(img2, isImage, "overlay/align", "image", 4, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "overlay/align", "image", i+4); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img1,
												 img2,
												 placeX.toString(),
												 placeY.toString());
			 
			 for (var i = 0; i < restImages.length; i++)
				img = world.Kernel.overlayImage(img,
												restImages[i], 
												placeX.toString(), 
												placeY.toString());

		     return img;
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


EXPORTS['underlay/align'] =
new PrimProc('underlay/align',
			 4,
			 true, false,
			 function(placeX, placeY, img1, img2, restImages) {
			 check(placeX, isPlaceX, "underlay/align", "x-place", 1, arguments);
			 check(placeY, isPlaceY, "underlay/align", "y-place", 2, arguments);
			 check(img1, isImage, "underlay/align", "image", 3, arguments);
			 check(img2, isImage, "underlay/align", "image", 4, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "underlay/align", "image", i+4); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img2,
												  img1,
												  placeX.toString(),
												  placeY.toString());
			 
			 for (var i = 0; i < restImages.length; i++)
			 img = world.Kernel.overlayImage(restImages[i], 
											  img,
											  placeX.toString(), 
											  placeY.toString());
			 
		     return img;
			 });


EXPORTS['beside'] =
new PrimProc('beside',
			 2,
			 true, false,
			 function(img1, img2, restImages) {
			 check(img1, isImage, "beside", "image", 1, arguments);
			 check(img2, isImage, "beside", "image", 2, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "beside", "image", i+4); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img1,
												 img2,
												 "beside",
												 "middle");
			 
			 for (var i = 0; i < restImages.length; i++)
			 img = world.Kernel.overlayImage(img,restImages[i], "beside", "middle");
			 
		     return img;
			 });

EXPORTS['beside/align'] =
new PrimProc('beside/align',
			 3,
			 true, false,
			 function(placeY, img1, img2, restImages) {
			 check(placeY, isPlaceY, "beside/align", "y-place", 1, arguments);
			 check(img1, isImage, "beside/align", "image", 2, arguments);
			 check(img2, isImage, "beside/align", "image", 3, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "beside", "image", i+3); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img1,
												 img2,
												 "beside",
												 placeY.toString());
			 
			 for (var i = 0; i < restImages.length; i++)
			 img = world.Kernel.overlayImage(img,
											 restImages[i], 
											 "beside",
											 placeY.toString());
			 
		     return img;
			 });

EXPORTS['above'] =
new PrimProc('above',
			 2,
			 true, false,
			 function(img1, img2, restImages) {
			 check(img1, isImage, "above", "image", 1, arguments);
			 check(img2, isImage, "above", "image", 2, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "above", "image", i+4); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img1,
												 img2,
												 "middle",
												 "above");
			 
			 for (var i = 0; i < restImages.length; i++)
			 img = world.Kernel.overlayImage(img,
											 restImages[i], 
											 "middle",
											 "above");
			 
		     return img;
			 });

EXPORTS['above/align'] =
new PrimProc('above/align',
			 3,
			 true, false,
			 function(placeX, img1, img2, restImages) {
			 check(placeX, isPlaceX, "above/align", "x-place", 1, arguments);
			 check(img1, isImage, "above/align", "image", 1, arguments);
			 check(img2, isImage, "above/align", "image", 2, arguments);
			 arrayEach(restImages, function(x, i) { check(x, isImage, "above/align", "image", i+4); }, arguments);
			 
			 var img = world.Kernel.overlayImage(img1,
												 img2,
												 placeX.toString(),
												 "above");
			 
			 for (var i = 0; i < restImages.length; i++)
			 img = world.Kernel.overlayImage(img,
											 restImages[i], 
											 placeX.toString(),
											 "above");
			 
		     return img;
			 });

EXPORTS['rotate'] =
new PrimProc('rotate',
			 2,
			 false, false,
			 function(angle, img) {
			 check(angle, isAngle, "rotate", "finite real number between 0 and 360", 1, arguments);
			 check(img, isImage, "rotate", "image", 2, arguments);
				 // negate the angle, to make it a counterclockwise rotation
			     return world.Kernel.rotateImage(jsnums.toFixnum(-angle), img);
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

EXPORTS['crop'] =
new PrimProc('crop',
			 5,
			 false, false,
			 function(x, y, width, height, img) {
			 check(x,	  isReal, "crop", "finite real number", 1, arguments);
			 check(y,	  isReal, "crop", "finite real number", 2, arguments);
			 check(width, isNonNegativeReal, "crop", "positive real number", 3, arguments);
			 check(height,isNonNegativeReal, "crop", "positive real number", 4, arguments);
			 check(img,   isImage,"crop", "image", 5, arguments);
			 return world.Kernel.cropImage(jsnums.toFixnum(x),
										   jsnums.toFixnum(y),
										   jsnums.toFixnum(width),
										   jsnums.toFixnum(height),
										   img);
			 });

EXPORTS['frame'] =
new PrimProc('frame',
			 1,
			 false, false,
			 function(img) {
			 check(img,   isImage,"frame", "image", 1, arguments);
			 return world.Kernel.frameImage(img);
			 });

EXPORTS['flip-vertical'] =
new PrimProc('flip-vertical',
			 1,
			 false, false,
			 function(img) {
			 check(img, isImage, "flip-vertical", "image", 1, arguments);
			 return world.Kernel.flipImage(img, "vertical");
			 });


EXPORTS['flip-horizontal'] =
new PrimProc('flip-horizontal',
			 1,
			 false, false,
			 function(img) {
			 check(img, isImage, "flip-horizontal", "image", 1, arguments);
			 return world.Kernel.flipImage(img, "horizontal");
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
		        return world.Kernel.textImage(aString.toString(), jsnums.toFixnum(aSize), aColor,
											  "normal", "Optimer","","",false);
		 });


EXPORTS['text/font'] =
new PrimProc('text/font',
			 8,
			 false, false,
			 function(aString, aSize, aColor, aFace, aFamily, aStyle, aWeight, aUnderline) {
			 check(aString, isString,		"text/font", "string",	1, arguments);
		     check(aSize,	function(x) { return isNatural(x) && jsnums.greaterThan(x, 0) && isByte(x); },
				   "text/font", "exact integer between 1 and 255",	2, arguments);
			 check(aColor,	isColor,		"text/font", "color",	3, arguments);
			 check(aFace,	function(x) {return isString(x) || !x;},		
											"text/font", "face",	4, arguments);
			 check(aFamily,	isFontFamily,	"text/font", "family",	5, arguments);
			 check(aStyle,	isFontStyle,	"text/font", "style",	6, arguments);
			 check(aWeight,	isFontWeight,	"text/font", "weight",	7, arguments);
			 check(aUnderline,isBoolean,	"text/font", "underline?",8, arguments);
			 
			 if (colorDb.get(aColor)) {
			 aColor = colorDb.get(aColor);
			 }
			 return world.Kernel.textImage(aString.toString(), jsnums.toFixnum(aSize), aColor,
										   aFace.toString(), aFamily.toString(), aStyle.toString(),
										   aWeight.toString(), aUnderline);
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


EXPORTS['video-url'] =
new PrimProc('video-url',
			 1,
			 false, true,
			 function(state, path) {
		     check(path, isString, "video-url", "string", 1);
			 return types.internalPause(function(caller, success, fail) {
										var rawVideo = document.createElement('video');
										rawVideo.src = path.toString();
										rawVideo.addEventListener('canplay', function() {
										success(world.Kernel.videoImage(path.toString(), rawVideo));
										});
										rawVideo.addEventListener('error', function(e) {
										fail(types.schemeError(types.incompleteExn(
																				   types.exnFail,
																				   " (unable to load: " + path + ")",
																				   [])));
										});
										rawVideo.src = path.toString();
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


EXPORTS['image-baseline'] =
new PrimProc('image-baseline',
			 1,
			 false, false,
			 function(img) {
			 check(img, isImage, 'image-baseline', 'image', 1);
			 return img.getBaseline();
			 });


EXPORTS['image->color-list'] = 
   new PrimProc('image->color-list',
		 1,
		 false, false,
		 function(img) {
		     check(img, isImage, 'image->color-list', 'image', 1);
		     var width = img.getWidth(),
                         height = img.getHeight(),
		         canvas = world.Kernel.makeCanvas(width, height),
		         ctx = canvas.getContext("2d"),
                         imageData,
                         data,
                         i,
		         r, g, b, a;
		     img.render(ctx, 0, 0);
		     imageData = ctx.getImageData(0, 0, width, height);
		     data = imageData.data;
		     var colors = [];
		     for (i = 0 ; i < data.length; i += 4) {
			 r = data[i];
			 g = data[i+1];
			 b = data[i+2];
			 a = data[i+3];
			 // FIXME: what to do about the alpha component?
			 colors.push(types.color(r, g, b));
		     }
		     return types.list(colors);
		 });


EXPORTS['color-list->image'] = 
    new PrimProc('color-list->image',
		 5,
		 false, false,
		 function(listOfColors, width, height, pinholeX, pinholeY) {
		     checkListOf(listOfColors, isColor, 'color-list->image', 'image', 1);
		     check(width, isNatural, 'color-list->image', 'natural', 2);
		     check(height, isNatural, 'color-list->image', 'natural', 3);
		     check(pinholeX, isNatural, 'color-list->image', 'natural', 4);
		     check(pinholeY, isNatural, 'color-list->image', 'natural', 5);
		     var canvas = world.Kernel.makeCanvas(jsnums.toFixnum(width),
							  jsnums.toFixnum(height)),
		         ctx = canvas.getContext("2d"),
   		         imageData = ctx.createImageData(jsnums.toFixnum(width),
							 jsnums.toFixnum(height)),
		         data = imageData.data,
		         aColor, i = 0;
		     while (listOfColors !== types.EMPTY) {
			 aColor = listOfColors.first;
			 data[i] = jsnums.toFixnum(types.colorRed(aColor));
			 data[i+1] = jsnums.toFixnum(types.colorGreen(aColor));
			 data[i+2] = jsnums.toFixnum(types.colorBlue(aColor));
			 data[i+3] = 255; // alpha?

			 i += 4;
			 listOfColors = listOfColors.rest;
		     };
		     return world.Kernel.imageDataImage(imageData);
		 });


EXPORTS['mode?']		= new PrimProc('mode?', 1, false, false, isMode);
EXPORTS['image-color?'] = new PrimProc('image-color?', 1, false, false, isColor);
EXPORTS['x-place?']		= new PrimProc('x-place?', 1, false, false, isPlaceX);
EXPORTS['y-place?']		= new PrimProc('y-place?', 1, false, false, isPlaceY);
EXPORTS['angle?']		= new PrimProc('angle?', 1, false, false, isAngle);
EXPORTS['side-count?']	= new PrimProc('side-count?', 1, false, false, isSideCount);
EXPORTS['step-count?']	= new PrimProc('step-count?', 1, false, false, isStepCount);

