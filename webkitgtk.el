;;; webkitgtk.el --- webkitgtk dynamic module -*- lexical-binding: t; -*-

;;; Commentary:

;; blah

;;; Code:


;; Don't require dynamic module at byte compile time.
(declare-function webkitgtk-load-uri "webkitgtk-module")
(declare-function webkitgtk-new-view "webkitgtk-module")
(declare-function webkitgtk-destroy "webkitgtk-destroy")
(declare-function webkitgtk-move "webkitgtk-module")
(declare-function webkitgtk-hide "webkitgtk-hide")
(declare-function webkitgtk-show "webkitgtk-show")

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

;;(webkitgtk-new-view 200 200 200 200)
;;(webkitgtk-load-uri "http://xkcd.com")
;;(webkitgtk-move 300 300)
;;(webkitgtk-resize 300 300)

;;(defun webkitgtk-adjust-size (window)
;;(defun webkitgtk-adjust-size (window)
;;  "Adjust webkitgtk size for WINDOW
;;
;;Called from buffer-local window-size-change-functions so WINDOW's
;;buffer's major must must be webkitgtk-mode"
;;  (message "adjusting size...")
;;  (print window)
;;  (with-current-buffer (window-buffer window)
;;    (print (window-buffer window))
;;    (if (eq major-mode 'webkitgtk-mode)
;;        (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges window)))
;;          (when webkitgtk-hidden
;;            (setq-local webkitgtk-hidden nil)
;;            (webkitgtk-show))
;;          (webkitgtk-move left top)
;;          (webkitgtk-resize (- right left) (- bottom top)))
;;      (setq-local webkitgtk-hidden t)
;;      (webkitgtk-hide))))

(defun webkitgtk-adjust-size (frame)
  "Adjust webkitgtk size for window in FRAME"
  ;;(message "adjusting size...")
  ;;(print frame)
  (with-current-buffer webkitgtk-buffer
    (let ((windows (get-buffer-window-list (current-buffer) 'nomini frame)))
      (print windows)
      (if (not windows)
          (webkitgtk-hide)
        (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges
                                                   (car windows))))
          (webkitgtk-show)
          (webkitgtk-move left top)
          (webkitgtk-resize (- right left) (- bottom top)))
      ))))

(defun webkitgtk-show-hide ()
  (message "show/hide...")
  (print (current-buffer)))

(require 'browse-url)

(defun webkitgtk (url &optional buffer-name)
  "Create a new webkitgtk with URL

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `webkitgtk'"
  (interactive (progn (browse-url-interactive-arg "URL: ")))
  (let ((buffer (generate-new-buffer (or buffer-name "webkitgtk"))))
    (with-current-buffer buffer
      (webkitgtk-mode)
      (setq-local webkitgtk-hidden nil)
      (setq webkitgtk-buffer buffer)
      ;;(setq-local window-buffer-change-functions (list 'webkitgtk-adjust-size))
      ;;(setq-local window-size-change-functions (list 'webkitgtk-adjust-size))
      ;;(setq-local window-state-change-functions (list 'webkitgtk-adjust-size))
      (add-hook 'window-size-change-functions #'webkitgtk-adjust-size)
      ;;(add-hook 'window-configuration-change-hook #'webkitgtk-show-hide nil t)
      (add-hook 'kill-buffer-hook #'webkitgtk-destroy nil t)
      (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges)))
        (webkitgtk-new-view left top (- right left) (- bottom top))
        (webkitgtk-load-uri url)))))

(define-derived-mode webkitgtk-mode
  special-mode "webkitgtk" "webkitgtk view mode."
  (setq buffer-read-only t))

(provide 'webkitgtk)
;;; webkitgtk.el ends here
