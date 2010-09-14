var evaluator = new Evaluator(
    { write: function(x) { writeToInteractions(x) },
      writeError: function(err) { reportError(err) },
    });



var writeToInteractions = function(thing) {
    var history = document.getElementById('history');
    if (typeof thing === 'string' || typeof thing === 'number') {
	var dom = document.createElement('div');
	dom.style['white-space'] = 'pre';
	dom.appendChild(document.createTextNode(thing + ''));
	history.appendChild(dom);
    } else {
	history.appendChild(thing);
    }
};


var reportError = function(exn) {

    // Under google-chrome, this will produce a nice error stack
    // trace that we can deal with.
    if (typeof(console) !== 'undefined' && console.log &&
	exn && exn.stack) {
	console.log(exn.stack);
    }


    var domElt = document.createElement('div');
    domElt.style['color'] = 'red';

    if (exn.domMessage) {
	domElt.appendChild(exn.domMessage);
    } else {
	var msg = evaluator.getMessageFromExn(exn);
	if (typeof(msg) === 'string') {
	    msg = document.createTextNode(msg);
	}
	domElt.appendChild(msg);
    }

    var stacktrace = evaluator.getTraceFromExn(exn);
    for (var i = 0; i < stacktrace.length; i++) {
	domElt.appendChild(document.createElement("br"));
	domElt.appendChild(document.createTextNode(
			     "in " + stacktrace[i].id +
			     ", at offset " + stacktrace[i].offset +
			     ", line " + stacktrace[i].line +
			     ", column " + stacktrace[i].column +
			     ", span " + stacktrace[i].span));
    };

    writeToInteractions(domElt);
};






var mainPageLoad = function() {
    var onSuccess = function() {
    };
    var onFail = function(exn) {
	reportError(exn);
    };
    if (MODULES[programModuleName].bytecode) {
	evaluator.executeCompiledProgram(MODULES[programModuleName].bytecode,
					 onSuccess,
					 onFail);
    } else {
	alert("Can not find module " + programModuleName);
    }
};
