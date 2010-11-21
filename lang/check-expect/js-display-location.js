
var locationStructType =
    STATE.invokedModules['mzscheme-vm/lang/location'].lookup("struct:location");


EXPORTS['display-location'] = 
    new types.PrimProc(
	'display-location',
	2,
	false, true,
	function(aState, thing, aLoc) {
            var aHash = {'id': String(locationStructType.accessor(aLoc, 0)),
                         'offset' : locationStructType.accessor(aLoc, 0),
                         'line' : locationStructType.accessor(aLoc, 2),
                         'column' : locationStructType.accessor(aLoc, 3),
                         'span' : locationStructType.accessor(aLoc, 4)};
            var aDom = helpers.makeLocationDom(aHash);
	    aState.getPrintHook()(aDom);
	    aState.getDisplayHook()("\n");
	});