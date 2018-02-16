// File reader worker

onmessage = function(oEvent) {
    var reader = new FileReader();
    var rawData = new ArrayBuffer();

    // Closure to capture the file information.
    reader.onload = (function(theFile) {
        return function(e) {
            console.log("[file-read-worker]: FILE LOADED", theFile, e.target.result);
            //rawData = e.target.result;
            e.target.result.file = theFile;
            postMessage({file: theFile, data: e.target.result});
        };
    })(oEvent.data);

    // Read in the image file as a data URL.
    reader.readAsArrayBuffer(oEvent.data);
};