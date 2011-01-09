var installProvider = function() {

    var encodeHash = function(h) {
	var buf = [], name;
	for (name in h) {
	    if (h.hasOwnProperty(name)) {
		buf.push(name + "=" + encodeURIComponent(h[name]));
	    }
	}
	return buf.join("&");
    };

    var socket = new easyXDM.Rpc(
	{},
	{
	    local: {
		ping: function(onSuccess, onError) {
		    onSuccess('ping');
		},

		compile: function(version, name, lang, text,
				  onSuccess, onError) {
		    var request = new XMLHttpRequest();
		    request.open("POST", "/servlets/standalone.ss", true);
		    request.onreadystatechange = function() {
			if (request.readyState === 4) {
			    if (request.status === 200) {
				onSuccess(JSON.parse(request.responseText));
			    } else {
				onError(request.responseText);
			    }
			}
		    };
		    if (lang) {
			request.send(encodeHash({ version: version,
						  name: name,
						  lang: lang,
						  text: text }));
		    } else {
			request.send(encodeHash({ version: version,
						  name: name,
						  text: text }));
		    }
		}
	    },
	    remote: {}
	});
};
