
function initTerm(callback) {
    setTimeout(function() { 
        var term = new Terminal({convertEol: true});
        var currentText = '';
        term.open(document.getElementById('terminal'));
        term.write(' > ');
        term.onKey(function(x) {
            if (x.key != '\r') {
                term.write(x.key);
            } else {
                callback(currentText, function(processedText) {
                    term.write(processedText);
                    term.write('\r\n > ');
                });
            }
        });
    }, 150);
}