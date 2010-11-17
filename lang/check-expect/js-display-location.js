
var locationStructType =
    STATE.invokedModules['mzscheme-vm/lang/location'].lookup("struct:location");


EXPORTS['display-location'] = 
    new types.PrimProc(
	'display-location',
	2,
	false, true,
	function(aState, thing, location) {
            var aLoc = {'id': locationStructType.accessor(x, 0),
                        'offset' : locationStructType.accessor(x, 0),
                        'line' : locationStructType.accessor(x, 2),
                        'column' : locationStructType.accessor(x, 3),           
        
                        'span' : locationStructType.accessor(x, 4)};
            var aDom = helpers.makeLocationDom(aLoc);
	    aState.getPrintHook()(aDom);
	});