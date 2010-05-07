var _runtime = require('./../../lib');


var code = "_runtime.list([_runtime.symbol(\"compilation-top\"),_runtime.rational(0),_runtime.list([_runtime.symbol(\"prefix\"),_runtime.rational(0),_runtime.list([_runtime.FALSE])]),_runtime.list([_runtime.symbol(\"mod\"),_runtime.symbol(\"42_runtime.ss.merged\"),_runtime.list([_runtime.symbol(\"prefix\"),_runtime.rational(0),_runtime.list([_runtime.FALSE,_runtime.symbol(\"print-values1736\")])]),_runtime.list([_runtime.list([_runtime.symbol(\"def-values\"),_runtime.list([_runtime.list([_runtime.symbol(\"toplevel\"),_runtime.rational(0),_runtime.rational(1),_runtime.TRUE,_runtime.FALSE])]),_runtime.list([_runtime.symbol(\"indirect\"),_runtime.symbol(\"print-values65\")])]),_runtime.list([_runtime.symbol(\"apply-values\"),_runtime.list([_runtime.symbol(\"toplevel\"),_runtime.rational(0),_runtime.rational(1),_runtime.FALSE,_runtime.FALSE]),_runtime.list([_runtime.symbol(\"constant\"),_runtime.rational(42)])])])])])";


var program = eval(code);
var state = _runtime.load(program);
_runtime.run(state);