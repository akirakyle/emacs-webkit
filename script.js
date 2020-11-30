/*
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
  console.log('WKViewKeyDown: '+event.key);
  if (event.ctrlKey && event.key == 'g') {
    window.webkit.messageHandlers["webkit--callback-unfocus"].postMessage('');
  }
}
document.addEventListener('keydown', WKViewKeyDown);
*/

// Local Variables:
// js-indent-level: 2
// End:
