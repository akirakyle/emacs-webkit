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
    window.webkit.messageHandlers["webkitgtk--callback-c-g"].postMessage('');
  }
}
document.addEventListener('keydown', WKViewKeyDown);

window.__webkit_hints = [];
function webkitHints(hintKeys) {
  let N = hintKeys.length
  let tags = 'button, input, [href], select, textarea, [tabindex]:not([tabindex="-1"])';
  let elems = document.querySelectorAll(tags)
  let hintPadLen = Math.ceil(Math.log(elems.length)/Math.log(N))

  function idxToHintChars(idx) {
    return idx.toString(N).padStart(hintPadLen, '0').split('').map(
      function (digit) {
        return hintKeys.charAt(parseInt(digit, N));
      }).join('');
  }
  function webkitAddHint(elem, idx) {
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
          })
       ){
      console.log("new new new "+idx);
      let overlay = document.createElement('div');
      overlay.className = 'emacs-hint-label';
      overlay.style.position = 'fixed';
      overlay.style.left = bounding.left + 'px';
      overlay.style.top = bounding.top + 'px';
      document.body.appendChild(overlay);
      window.__webkit_hints.push([overlay, elem]);
      overlay.appendChild(document.createTextNode(idxToHintChars(idx)));
    };
  }
  elems.forEach(webkitAddHint);
  return window.__webkit_hints.length;
}
// Local Variables:
// js-indent-level: 2
// End:
