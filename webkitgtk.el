;;; blah.el --- bla demo -*- lexical-binding: t; -*-

;;; Commentary:

;; This demonstrates the blah

;;; Code:


;; Don't require dynamic module at byte compile time.
(declare-function webkitgtk-load-uri  "webkitgtk-module")

(defun fake-module-reload (module)
  (interactive "Reload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(fake-module-reload (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so"))

;;(add-to-list 'load-path (expand-file-name "~/git/emacs-webkitgtk"))
;;(module-load (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so))
;;(require 'webkitgtk-module)

;;(webkitgtk-load-uri "http://google.com")

;;; blah.el ends here
