var PRIMITIVES = {};

var Primitive = function(name, arity, isRest, impl) {
    this.name = name;
    this.arity = arity;
    this.isRest = isRest;
    this.impl = impl;
};



var defaultPrint = 
    new Primitive('print', 1, false, function(x) {
	    sys.print(''+x);
	});


PRIMITIVES['current-print'] =
    new Primitive('current-print', 
		  0, 
		  false, 
		  function() {
		      return defaultPrint;
		  });



PRIMITIVES['for-each'] =
    new Primitive('for-each', 
		  2, 
		  true, 
		  function(f, lst1, restLists) {
		      return "foo";
		  });






//////////////////////////////////////////////////////////////////////


exports.isPrimitive = function(x) {
    return x instanceof Primitive;
};

exports.getPrimitive = function(name) {
    return PRIMITIVES[name];
}