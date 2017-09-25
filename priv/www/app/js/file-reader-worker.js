// File reader worker

onmessage = function(oEvent) {
    var reader = new FileReader();
    var rawData = new ArrayBuffer();

    // Closure to capture the file information.
    reader.onload = (function(theFile) {
        return function(e) {
            console.log("[file-read-worker]: FILE LOADED", e, e.target.result);
            //rawData = e.target.result;
            postMessage(e.target.result);
        };
    })(oEvent.data);

    // Read in the image file as a data URL.
    reader.readAsArrayBuffer(oEvent.data);
};