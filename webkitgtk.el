;;; webkitgtk.el --- webkitgtk dynamic module -*- lexical-binding: t; -*-

;;; Commentary:

;; blah

;;; Code:


;; Don't require dynamic module at byte compile time.
(declare-function webkitgtk--load-uri "webkitgtk-module")
(declare-function webkitgtk--new "webkitgtk-module")
(declare-function webkitgtk--destroy "webkitgtk-module")
(declare-function webkitgtk--resize "webkitgtk-module")
(declare-function webkitgtk--hide "webkitgtk-module")
(declare-function webkitgtk--show "webkitgtk-module")
(declare-function webkitgtk--focus "webkitgtk-module")
(declare-function webkitgtk--unfocus "webkitgtk-module")

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

(defvar webkitgtk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'webkitgtk-goto-url)
    (define-key map "b" 'webkitgtk-back)
    (define-key map "f" 'webkitgtk-forward)
    (define-key map "r" 'webkitgtk-reload)
    (define-key map "i" 'webkitgtk-insert-mode)
    map)
  "Keymap for `webkitgtk-mode'.")

(defun webkitgtk--adjust-size (frame)
  "Adjust webkitgtk size for window in FRAME"
  ;;(message "adjusting size...")
  ;;(print frame)
  (dolist (id-buffer webkitgtk--id-buffer-alist)
    (if (buffer-live-p (cdr id-buffer))
        (with-current-buffer (cdr id-buffer) 
          (let* ((windows (get-buffer-window-list (current-buffer) 'nomini frame)))
            ;;(print windows)
            (if (not windows)
                (webkitgtk--hide (car id-buffer))
              (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges
                                                         (car windows))))
                (webkitgtk--show (car id-buffer))
                ;;(when (eq (frame-selected-window frame) (car windows))
                ;;  (webkitgtk--focus-wrapper (car id-buffer)))
                (webkitgtk--resize (car id-buffer)
                                  left top (- right left) (- bottom top)))
              (dolist (window (cdr windows))
                (switch-to-prev-buffer window)))))
      (webkitgtk--hide (car id-buffer))
      ;;(webkitgtk--unfocus-wrapper (car id-buffer))
      (setq webkitgtk--id-buffer-alist (delq id-buffer webkitgtk--id-buffer-alist)))))
  ;;(webkitgtk--change-focus frame))

(defun webkitgtk--change-focus (frame)
  "Change webkitgtk size for window in FRAME"
  ;;(message "changing focus...")
  ;;(print webkitgtk--focused)
  (let ((buffer (window-buffer (frame-selected-window frame))))
    ;;(print buffer)
    (with-current-buffer buffer
      (if (eq major-mode 'webkitgtk-mode)
          (let ((id (car (rassoc buffer webkitgtk--id-buffer-alist))))
            ;;(print id)
            (webkitgtk--focus id)
            (setq webkitgtk--focused id))
        (when webkitgtk--focused
          (webkitgtk--unfocus webkitgtk--focused)
          (setq webkitgtk--focused nil))))))

(defun webkitgtk-insert-mode ()
  (interactive)
  (when (eq major-mode 'webkitgtk-mode)
    (webkitgtk--focus (car (rassoc (current-buffer) webkitgtk--id-buffer-alist)))))

(require 'browse-url)

(defun webkitgtk-goto-url (url &optional new-session)
  (interactive (progn (browse-url-interactive-arg "URL: ")))
  (when (eq major-mode 'webkitgtk-mode)
    (webkitgtk--load-uri
     (car (rassoc (current-buffer) webkitgtk--id-buffer-alist)) url)))
  
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
      (let ((id (webkitgtk--new)))
        (print id)
        (push (cons id buffer) webkitgtk--id-buffer-alist)
        (webkitgtk--load-uri id url)))
      (switch-to-buffer buffer)))

(define-derived-mode webkitgtk-mode
  special-mode "webkitgtk" "webkitgtk view mode."
  (setq buffer-read-only t))

(setq webkitgtk--id-buffer-alist nil)
(setq webkitgtk--focused nil)
(add-hook 'window-size-change-functions #'webkitgtk--adjust-size)
;;(add-hook 'window-selection-change-functions #'webkitgtk--change-focus)

;;(remove-hook 'window-size-change-functions #'webkitgtk--adjust-size)
;;(remove-hook 'window-selection-change-functions #'webkitgtk--change-focus)

(provide 'webkitgtk)
;;; webkitgtk.el ends here
