
function initTerm(callback) {
    // setTimeout(function() { 
        var term = new Terminal({convertEol: true});
        term._core.insertMode = true;
        
        var currentText = '';
        var cursorPosition = 0;

        term.open(document.getElementById('terminal'));
        term.write(' > ');
        term.onKey(function(x) {
            if (x.key == '\r') {
                callback(currentText, function(processedText) {
                    term.write('\r\n');
                    term.write(processedText);
                    term.write('\r\n > ');
                    currentText = '';
                    cursorPosition = 0;
                });
            } else if (x.key.charCodeAt(0) == 127) {
                if (currentText.length > 0) {
                    term.write("\b \b");
                    currentText = currentText.substring(0, currentText.length - 1);
                    cursorPosition -= 1;
                }
            } else if (x.key != '\x1B[A' && x.key != '\x1B[B' && x.key != '\x1B[C' && x.key != '\x1B[D') {
                term.write(x.key);
                currentText += x.key;
                cursorPosition += 1;
            } else if (x.key == '\x1B[D' && cursorPosition > 0) {
                term.write(x.key);
                cursorPosition -= 1;
            } else if (x.key == '\x1B[C' && cursorPosition < currentText.length) {
                term.write(x.key);
                cursorPosition += 1;
            }
            console.log(x);
        });
    // }, 150);
}