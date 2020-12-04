;; Not really tests per se, but rather a dump of elisp I've used to test stuff
(defun fake-module-reload (module)
  (interactive "Reload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(add-to-list 'load-path "~/git/emacs-webkit")
(fake-module-reload (expand-file-name "~/git/emacs-webkit/webkit-module.so"))
;;(setq debug-on-error t)

(require 'webkit)
(require 'evil-collection-webkit)
(require 'webkit-ace)

(evil-collection-xwidget-setup)

(webkit-browse-url "http://xkcd.com" t)
(setq webkit-own-window t)
(garbage-collect)

;;(setq my-pipe (get-buffer-process (cdr (car webkit--id-buffer-alist))))
(with-current-buffer (car webkit--buffers) (buffer-string))
(setq webkit--id (with-current-buffer (car webkit--buffers) webkit--id))
(setq webkit--id nil)

(webkit--xid-to-pointer (string-to-number (frame-parameter (selected-frame) 'window-id)))
(eq 'x (window-system))

(setq my-params (frame-parameters))
(while my-params
  (message "%S" (car my-params))
  (setq my-params (cdr my-params)))
(modify-frame-parameters nil '((inhibit-double-buffering . t)))

(setq webkit--script (webkit--file-to-string
                         (expand-file-name "script.js" webkit-base)))
(webkit--execute-js webkit--id
                    "webkitHints('aoeuhtns');" "message")
(webkit--execute-js webkit--id "alert(\"hi\")")
(webkit--execute-js webkit--id "\"hi\"" "message")
(webkit--add-user-script webkit--id "alert(\"hi\")")
(webkit--remove-all-user-scripts webkit--id)
(webkit--register-script-message webkit--id "message")
(webkit--unregister-script-message webkit--id "message")

(webkit--execute-js webkit--id
                    "window.webkit.messageHandlers.message.postMessage(\"hi\")")

(webkit--execute-js webkit--id
                    "window.webkit.messageHandlers[\"webkit--callback-key-down\"].postMessage(\"hi\")"
                    "message")

(webkit--proxy-set-uri webkit--id "socks://localhost:8000")
(webkit--proxy-set-default webkit--id)

(setq webkit--to-json-js "
function toJSON(node) {
  let propFix = { for: 'htmlFor', class: 'className' };
  let specialGetters = {
    style: (node) => node.style.cssText,
  };
  let attrDefaultValues = { style: '' };
  let obj = {
    nodeType: node.nodeType,
  };
  if (node.tagName) {
    obj.tagName = node.tagName.toLowerCase();
  } else if (node.nodeName) {
    obj.nodeName = node.nodeName;
  }
  if (node.nodeValue) {
    obj.nodeValue = node.nodeValue;
  }
  let attrs = node.attributes;
  if (attrs) {
    let defaultValues = new Map();
    for (let i = 0; i < attrs.length; i++) {
      let name = attrs[i].nodeName;
      defaultValues.set(name, attrDefaultValues[name]);
    }
    // Add some special cases that might not be included by enumerating
    // attributes above. Note: this list is probably not exhaustive.
    switch (obj.tagName) {
      case 'input': {
        if (node.type === 'checkbox' || node.type === 'radio') {
          defaultValues.set('checked', false);
        } else if (node.type !== 'file') {
          // Don't store the value for a file input.
          defaultValues.set('value', '');
        }
        break;
      }
      case 'option': {
        defaultValues.set('selected', false);
        break;
      }
      case 'textarea': {
        defaultValues.set('value', '');
        break;
      }
    }
    let arr = [];
    for (let [name, defaultValue] of defaultValues) {
      let propName = propFix[name] || name;
      let specialGetter = specialGetters[propName];
      let value = specialGetter ? specialGetter(node) : node[propName];
      if (value !== defaultValue) {
        arr.push([name, value]);
      }
    }
    if (arr.length) {
      obj.attributes = arr;
    }
  }
  let childNodes = node.childNodes;
  // Don't process children for a textarea since we used `value` above.
  if (obj.tagName !== 'textarea' && childNodes && childNodes.length) {
    let arr = (obj.childNodes = []);
    for (let i = 0; i < childNodes.length; i++) {
      arr[i] = toJSON(childNodes[i]);
    }
  }
  return obj;
}
toJSON(document);
")

(defun webkit--save-json (msg)
  (setq webkit--json (json-parse-string msg)))

(webkit--execute-js
 (with-current-buffer (car webkit--buffers) webkit--id)
 webkit--to-json-js "webkit--save-json")

(defun webkit--echo-uri (uri)
  (message uri))

(add-hook 'webkit-uri-changed-functions 'webkit--echo-uri)

(setq webkit-uri-changed-functions nil)
(setq webkit-progress-changed-functions nil)

(remove-hook 'window-size-change-functions #'webkit--adjust-size)
(webkit--show webkit--id)

(webkit--resize webkit--id 50 50 200 400)

(setq test-hist nil)
(maphash (lambda (k v)
           (push (cons k v) test-hist))
             webkit-history-table)
