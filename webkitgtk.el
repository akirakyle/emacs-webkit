;;; webkitgtk.el --- webkitgtk dynamic module -*- lexical-binding: t; -*-

;;; Commentary:

;; blah

;;; Code:


;; Don't require dynamic module at byte compile time.
(declare-function webkitgtk-load-uri "webkitgtk-module")
(declare-function webkitgtk-new "webkitgtk-module")
(declare-function webkitgtk-destroy "webkitgtk-module")
;;(declare-function webkitgtk-move "webkitgtk-module")
(declare-function webkitgtk-resize "webkitgtk-module")
(declare-function webkitgtk-hide "webkitgtk-module")
(declare-function webkitgtk-show "webkitgtk-module")

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
  (dolist (id-buffer webkitgtk--id-buffer-alist)
    (if (buffer-live-p (cdr id-buffer))
        (with-current-buffer (cdr id-buffer) 
          (let* ((windows (get-buffer-window-list (current-buffer) 'nomini frame)))
            ;;(print windows)
            (if (not windows)
                (webkitgtk-hide (car id-buffer))
              (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges
                                                         (car windows))))
                (webkitgtk-show (car id-buffer))
                ;;(webkitgtk-move left top)
                ;;(webkitgtk-resize (- right left) (- bottom top)))
                (webkitgtk-resize (car id-buffer)
                                  left top (- right left) (- bottom top)))
              (dolist (window (cdr windows))
                (switch-to-prev-buffer window)))))
      (webkitgtk-hide (car id-buffer))
      (setq webkitgtk--id-buffer-alist (delq id-buffer webkitgtk--id-buffer-alist)))))

(require 'browse-url)

(defun webkitgtk (url &optional buffer-name)
  "Create a new webkitgtk with URL

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `webkitgtk'"
  (interactive (progn (browse-url-interactive-arg "URL: ")))
  (let ((buffer (generate-new-buffer (or buffer-name "webkitgtk"))))
    (with-current-buffer buffer
      (webkitgtk-mode)
      ;;(setq-local webkitgtk-hidden nil)
      ;;(setq-local window-buffer-change-functions (list 'webkitgtk-adjust-size))
      ;;(setq-local window-size-change-functions (list 'webkitgtk-adjust-size))
      ;;(setq-local window-state-change-functions (list 'webkitgtk-adjust-size))
      ;;(add-hook 'kill-buffer-hook #'webkitgtk-destroy nil t)
      ;;(pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges)))
        ;;(webkitgtk-new-view left top (- right left) (- bottom top))
      (let ((id (webkitgtk-new)))
        (print id)
        (push (cons id buffer) webkitgtk--id-buffer-alist)
        (webkitgtk-load-uri id url)
        ))))

(define-derived-mode webkitgtk-mode
  special-mode "webkitgtk" "webkitgtk view mode."
  (setq buffer-read-only t))

(setq webkitgtk--id-buffer-alist nil)
(add-hook 'window-size-change-functions #'webkitgtk-adjust-size)

(provide 'webkitgtk)
;;; webkitgtk.el ends here
