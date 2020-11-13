(defun fake-module-reload (module)
  (interactive "Reload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(fake-module-reload (expand-file-name "~/git/emacs-webkit/webkit-module.so"))
;;(module-load (expand-file-name "~/git/emacs-webkit/webkit-module.so"))
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "~/git/emacs-webkit"))

(require 'webkit)
(require 'evil-collection-webkit)

(evil-collection-xwidget-setup)

(webkit-browse-url "http://xkcd.com" t)
(setq webkit-search-prefix "https://google.com/search?q=")
(setq webkit-own-window t)

;;(setq my-pipe (get-buffer-process (cdr (car webkit--id-buffer-alist))))
(with-current-buffer (car webkit--buffers) (buffer-string))
(setq webkit--id (with-current-buffer (car webkit--buffers) webkit--id))

(webkit--execute-js webkit--id "alert(\"hi\")")
(webkit--execute-js webkit--id "\"hi\"" "message")
(webkit--add-user-script webkit--id "alert(\"hi\")")
(webkit--remove-all-user-scripts webkit--id)
(webkit--register-script-message webkit--id "message")
(webkit--unregister-script-message webkit--id "message")

;(webkit--register-script-message webkit--id "webkit--callback-key-down")

(webkit--execute-js webkit--id
                       "window.webkit.messageHandlers.message.postMessage(\"hi\")")

(webkit--execute-js webkit--id
                       "window.webkit.messageHandlers[\"webkit--callback-key-down\"].postMessage(\"hi\")"
                       "message")
(setq webkit--id nil)
(garbage-collect)

(remove-hook 'window-size-change-functions #'webkit--adjust-size)
(webkit--show webkit--id)

(webkit--resize webkit--id 50 50 200 400)
