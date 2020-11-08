(defun fake-module-reload (module)
  (interactive "Reload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(fake-module-reload (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so"))
;;(module-load (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so"))
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "~/git/emacs-webkitgtk"))

(require 'webkitgtk)
(require 'evil-collection-webkitgtk)

(evil-collection-xwidget-setup)

(webkitgtk-browse-url "http://xkcd.com")

;;(setq my-pipe (get-buffer-process (cdr (car webkitgtk--id-buffer-alist))))
(with-current-buffer (car webkitgtk--buffers) (buffer-string))
(setq webkitgtk--id (with-current-buffer (car webkitgtk--buffers) webkitgtk--id))

(webkitgtk--execute-js webkitgtk--id "alert(\"hi\")")
(webkitgtk--execute-js webkitgtk--id "\"hi\"" "message")
(webkitgtk--add-user-script webkitgtk--id "alert(\"hi\")")
(webkitgtk--remove-all-user-scripts webkitgtk--id)
(webkitgtk--register-script-message webkitgtk--id "message")
(webkitgtk--unregister-script-message webkitgtk--id "message")

;(webkitgtk--register-script-message webkitgtk--id "webkitgtk--callback-key-down")

(webkitgtk--execute-js webkitgtk--id
                       "window.webkit.messageHandlers.message.postMessage(\"hi\")")

(webkitgtk--execute-js webkitgtk--id
                       "window.webkit.messageHandlers[\"webkitgtk--callback-key-down\"].postMessage(\"hi\")"
                       "message")
