

var isEqual = function(x, y) {
	return types.isEqual(x, y, new types.UnionFind());
}

var isNonNegativeReal = function(x) {
	return isReal(x) && jsnums.greaterThanOrEqual(x, 0);
};

var isReal = jsnums.isReal;




EXPORTS['check-expect'] =
    new types.PrimProc('check-expect',
		 2,
		 false, true,
		 function(aState, actual, expected) {
		 	if ( types.isFunction(actual) || types.isFunction(expected) ) {
				var msg = 'check-expect cannot compare functions';
				helpers.raise( types.incompleteExn(types.exnFailContract, msg, []) );
			}
		 	if ( !isEqual(actual, expected) ) {
				var msg = helpers.format('check-expect: actual value ~s differs from ~s, the expected value.\n',
							 [actual, expected]);
			        aState.getDisplayHook()(msg);
			    var stackTrace = state.getStackTraceFromContinuationMarks(
				state.captureCurrentContinuationMarks(aState));
			    for (var i = 0; i < stackTrace.length; i++) {
			        aState.getPrintHook()(helpers.makeLocationDom(stackTrace[i]));
			    }
			}
			aState.v = types.VOID;
		});
EXPORTS['EXAMPLE'] = EXPORTS['check-expect'];



EXPORTS['check-within'] =
    new types.PrimProc('check-within',
		 3,
		 false, true,
		 function(aState, actual, expected, range) {
		 	if ( !isNonNegativeReal(range) ) {
				var msg = helpers.format('check-within requires a non-negative real number for range, given ~s.',
							 [range]);
				helpers.raise( types.incompleteExn(types.exnFailContract, msg, []) );
			}
		 	if ( types.isFunction(actual) || types.isFunction(expected) ) {
				var msg = 'check-within cannot compare functions';
				helpers.raise( types.incompleteExn(types.exnFailContract, msg, []) );
			}
			
		 	if ( !( isEqual(actual, expected) ||
			        (isReal(actual) && isReal(expected) &&
				 jsnums.lessThanOrEqual(jsnums.abs(jsnums.subtract(actual, expected)),
					 		range)) ) ) {
				var msg = helpers.format('check-within: actual value ~s is not within ~s of expected value ~s.',
							 [actual, range, expected]);

			        aState.getDisplayHook()(msg);
			    var stackTrace = state.getStackTraceFromContinuationMarks(
				state.captureCurrentContinuationMarks(aState));
			    for (var i = 0; i < stackTrace.length; i++) {
			        aState.getPrintHook()(helpers.makeLocationDom(stackTrace[i]));
			    }
			}
			aState.v = types.VOID;
		});
				
