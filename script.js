function WKViewHasFocus() {
  var ae = document.activeElement;
  if (ae) {
    var name = ae.nodeName;
    return name == 'INPUT' || name == 'TEXTAREA';
  } else {
    return false;
  }
}
function WKViewKeyDown(event) {
  if (event.ctrlKey && event.key == 'g') {
    window.webkit.messageHandlers["webkitgtk--callback-key-down"].postMessage('C-g');
  }
}
document.addEventListener('keydown', WKViewKeyDown);
