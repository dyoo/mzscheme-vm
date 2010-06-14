// browser-specific hooks and definitions
var sys = {};

sys.print = function(s) {
	if ( s.toString().charAt(s.length-1) === '\n' ) {
		s = s.toString().substring(0, s.length-1) + "<br />";
	}
	document.write(s);
};


sys.inspect = function(x) {
    // FIXME: add more helpful inspect function that'll show
    // us what's really inside.  Perhaps use toString()?
    return x + '';
};


var DEBUG_ON = false;

var setDebug = function(v) {
    DEBUG_ON = v;
}

var debug = function(s) {
    if (DEBUG_ON) {
	sys.print(s);
    }
}

var debugF = function(f_s) {
    if (DEBUG_ON) {
	sys.print(f_s());
    }
}


var deepEqual = function (obj1, obj2) {
	if (obj1 === obj2) {
		return true;
	}

	for (var i in obj1) {
		if ( obj1.hasOwnProperty(i) ) {
			if ( !(obj2.hasOwnProperty(i) && deepEqual(obj1[i], obj2[i])) )
				return false;
		}
	}
	for (var i in obj2) {
		if ( obj2.hasOwnProperty(i) ) {
			if ( !(obj1.hasOwnProperty(i) && deepEqual(obj1[i], obj2[i])) )
				return false;
		}
	}
	return true;
}


var assert = {};

assert.equal = function(x, y) {
	if (x !== y) {
		alert('AssertError: ' + x + ' equal ' + y);
		throw new Error('AssertError: ' + x + ' equal ' + y);
	}
}

assert.deepEqual = function(x, y) {
	if ( !deepEqual(x, y) ) {
		alert('AssertError: ' + x + ' deepEqual ' + y);
		throw new Error('AssertError: ' + x + ' deepEqual ' + y);
	}
}


assert.ok = function(x) {
	if (!x) {
		alert('AssertError: not ok: ' + x);
		throw new Error('AssertError: not ok: ' + x );
	}
}


assert.throws = function(f) {
	try {
		f.apply(null, []);
	} catch (e) {
		return true;
	}
	throw new Error('AssertError: Throw expected, none received.');
}


