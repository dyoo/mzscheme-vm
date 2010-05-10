var interpret = require('./interpret');
var types = require('./types');

exports.load = interpret.load;
exports.run = interpret.run;



exports.symbol = types.symbol;
exports.rational = types.rational;
exports.list = types.list;
exports.vector = types.vector;

exports.FALSE = Logic.FALSE;
exports.TRUE = Logic.TRUE;