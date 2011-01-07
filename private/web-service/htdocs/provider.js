var installProvider = function() {
    var socket = new easyXDM.Rpc(
	{},
	{
	    local: {
		compile: function(version, name, lang, text,
				  onSuccess, onError) {
		    onSuccess("ok, I see:" + version + ' ' + name + ' ' + lang +  ' ' + text);
		}
	    },
	    remote: {}
	});
};
