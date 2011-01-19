var rpc = new easyXDM.Rpc(
    { remote: "provider.html" },
    {
	local: {compile : function(version, isModule, name, lang, text) {}},
	remote: { compile : {}}});


var doCompilation = function(isModule) {
    var lang = isModule ? 'wescheme' : 'wescheme-interaction';
    rpc.compile('1',
                isModule,
                'my program',
                lang,
                document.getElementById("textarea").value,
                function(response) {
		    evaluator.executeCompiledProgram(
			eval('(' + response.code + ')').bytecode,
			function() {
			    console.log('done');
			},
			function(err) {
			    console.log('error');
			    console.log(err);
			});
                },
                function(error) {
                    alert(error.message);
                });

};



var evaluator = new Evaluator(
    { write: function(x) { writeToInteractions(x) },
      writeError: function(err) { reportError(err) },
      compilationServletUrl: "/servlets/standalone.ss",
      scriptCompilationServletUrl: "/servlets/standalone.ss"
    });




var breakButtonPressed = function() {
    evaluator.requestBreak();
};


var writeToInteractions = function(thing) {
    var history = document.getElementById('transcript');
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
	domElt.appendChild(document.createTextNode(evaluator.getMessageFromExn(exn)+""));
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


var unblockInput = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    interactionText.disabled = false;
    executeButton.disabled = false;
    interactionText.value = '';
    interactionText.focus();
};


var blockInput = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    interactionText.disabled = true;
    executeButton.disabled = true;
};




var mainPageLoad = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    unblockInput();
};
