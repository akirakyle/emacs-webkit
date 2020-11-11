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

window.__webkit_hints = [];
function webkitHints() {
  function webkitAddHint(elem) {
    let bounding = elem.getBoundingClientRect();
    if (bounding.top >= 0 &&
        bounding.left >= 0 &&
        bounding.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
        bounding.right <= (window.innerWidth || document.documentElement.clientWidth) &&
        (elem.offsetWidth > 0 || elem.offsetHeight > 0 || elem.getClientRects().length > 0) &&
        window.__webkit_hints.every(
          function (other_candidate){
            let other_overlay = other_candidate[0];
            other_bounding = other_overlay.getBoundingClientRect();
            return !(Math.abs(other_bounding.top - bounding.top) < 5
                     && Math.abs(other_bounding.left - bounding.left) < 5)
          })){
      let overlay = document.createElement('div');
      overlay.className = 'emacs-hint-label';
      overlay.style.position = 'fixed';
      overlay.style.left = bounding.left + 'px';
      overlay.style.top = bounding.top + 'px';
      document.body.appendChild(overlay);
      window.__webkit_hints.push([overlay, elem]);
    };
  }
  let elems = 'button, input, [href], select, textarea, [tabindex]:not([tabindex="-1"])';
  document.querySelectorAll(elems).forEach(webkitAddHint);
  let label_length = Math.ceil(Math.log(window.__webkit_hints.length)/Math.log(26));
  window.__webkit_hints.forEach(
    function (candidate, id) {
      let elem = candidate[0];
      elem.appendChild(document.createTextNode(
        id.toString(26).split('').map(
          function (char) {
            let code = char.charCodeAt(0);
            if (code < 97) {
              return String.fromCharCode(code + 49);
            }
            else {
              return String.fromCharCode(code + 10);
            }
          }).join('').padStart(label_length,'a')));
    });
  return window.__webkit_hints.length;
}
// Local Variables:
// js-indent-level: 2
// End:
