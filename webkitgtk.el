;;; webkitgtk.el --- webkitgtk dynamic module -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Akira Kyle

;; Author: Akira Kyle <ak@akirakyle.com>
;; URL: https://github.com/
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; webkitgtk dynamic module

;;; Code:

;; Don't require dynamic module at byte compile time.
(declare-function webkitgtk--new "webkitgtk-module")
(declare-function webkitgtk--resize "webkitgtk-module")
(declare-function webkitgtk--hide "webkitgtk-module")
(declare-function webkitgtk--show "webkitgtk-module")
(declare-function webkitgtk--focus "webkitgtk-module")
(declare-function webkitgtk--unfocus "webkitgtk-module")
(declare-function webkitgtk--forward "webkitgtk-module")
(declare-function webkitgtk--back "webkitgtk-module")
(declare-function webkitgtk--reload "webkitgtk-module")
(declare-function webkitgtk--get-zoom "webkitgtk-module")
(declare-function webkitgtk--set-zoom "webkitgtk-module")
(declare-function webkitgtk--load-uri "webkitgtk-module")
(declare-function webkitgtk--execute-js "webkitgtk-module")
(declare-function webkitgtk--add-user-script "webkitgtk-module")
(declare-function webkitgtk--remove-all-user-scripts "webkitgtk-module")
(declare-function webkitgtk--register-script-message "webkitgtk-module")
(declare-function webkitgtk--unregister-script-message "webkitgtk-module")

(defun fake-module-reload (module)
  (interactive "Reload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(fake-module-reload (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so"))
;;(module-load (expand-file-name "~/git/emacs-webkitgtk/webkitgtk-module.so"))
;;(add-to-list 'load-path (expand-file-name "~/git/emacs-webkitgtk"))
;;(require 'webkitgtk-module)

(defvar webkitgtk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'webkitgtk-browse-url)
    (define-key map "f" 'webkitgtk-forward)
    (define-key map "b" 'webkitgtk-back)
    (define-key map "r" 'webkitgtk-reload)
    (define-key map "i" 'webkitgtk-insert-mode)
    (define-key map "+" 'webkitgtk-zoom-in)
    (define-key map "-" 'webkitgtk-zoom-out)

    ;;similar to image mode bindings
    (define-key map (kbd "SPC")                 'webkitgtk-scroll-up)
    (define-key map (kbd "S-SPC")               'webkitgtk-scroll-down)
    (define-key map (kbd "DEL")                 'webkitgtk-scroll-down)

    (define-key map [remap scroll-up]           'webkitgtk-scroll-up-line)
    (define-key map [remap scroll-up-command]   'webkitgtk-scroll-up)

    (define-key map [remap scroll-down]         'webkitgtk-scroll-down-line)
    (define-key map [remap scroll-down-command] 'webkitgtk-scroll-down)

    (define-key map [remap forward-char]        'webkitgtk-scroll-forward)
    (define-key map [remap backward-char]       'webkitgtk-scroll-backward)
    (define-key map [remap right-char]          'webkitgtk-scroll-forward)
    (define-key map [remap left-char]           'webkitgtk-scroll-backward)
    (define-key map [remap previous-line]       'webkitgtk-scroll-down-line)
    (define-key map [remap next-line]           'webkitgtk-scroll-up-line)

    (define-key map [remap beginning-of-buffer] 'webkitgtk-scroll-top)
    (define-key map [remap end-of-buffer]       'webkitgtk-scroll-bottom)
    map)
  "Keymap for `webkitgtk-mode'.")

(defun webkitgtk-zoom-in (&optional webkitgtk-id)
  "Increase webkitgtk view zoom factor."
  (interactive)
  (webkitgtk--set-zoom
   (or webkitgtk-id (webkitgtk--current-id))
   (+ (webkitgtk--get-zoom
       (or webkitgtk-id (webkitgtk--current-id)))
      0.1)))

(defun webkitgtk-zoom-out (&optional webkitgtk-id)
  "Decrease webkitgtk view zoom factor."
  (interactive)
  (webkitgtk--set-zoom
   (or webkitgtk-id (webkitgtk--current-id))
   (+ (webkitgtk--get-zoom
       (or webkitgtk-id (webkitgtk--current-id)))
      -0.1)))

(defun webkitgtk-scroll-up (&optional arg webkitgtk-id)
  "Scroll webkitgtk up by ARG pixels; or full window height if no ARG.
Stop if bottom of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls down."
  (interactive "P")
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   (format "window.scrollBy(0, %d);"
           (or arg (pcase-let ((`(,left ,top ,right ,bottom)
                                (window-inside-pixel-edges (selected-window))))
                    (- bottom top))))))

(defun webkitgtk-scroll-down (&optional arg webkitgtk-id)
  "Scroll webkitgtk down by ARG pixels; or full window height if no ARG.
Stop if top of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls up."
  (interactive "P")
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   (format "window.scrollBy(0, -%d);"
           (or arg (pcase-let ((`(,left ,top ,right ,bottom)
                                (window-inside-pixel-edges (selected-window))))
                     (- bottom top))))))

(defun webkitgtk-scroll-up-line (&optional n webkitgtk-id)
  "Scroll webkitgtk up by N lines.
The height of line is calculated with `window-font-height'.
Stop if the bottom edge of the page is reached.
If N is omitted or nil, scroll up by one line."
  (interactive "p")
  (webkitgtk-scroll-up (* n (window-font-height))))

(defun webkitgtk-scroll-down-line (&optional n webkitgtk-id)
  "Scroll webkitgtk down by N lines.
The height of line is calculated with `window-font-height'.
Stop if the top edge of the page is reached.
If N is omitted or nil, scroll down by one line."
  (interactive "p")
  (webkitgtk-scroll-down (* n (window-font-height))))

(defun webkitgtk-scroll-forward (&optional n webkitgtk-id)
  "Scroll webkitgtk horizontally by N chars.
The width of char is calculated with `window-font-width'.
If N is omitted or nil, scroll forwards by one char."
  (interactive "p")
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   (format "window.scrollBy(%d, 0);"
           (* n (window-font-width)))))

(defun webkitgtk-scroll-backward (&optional n webkitgtk-id)
  "Scroll webkitgtk back by N chars.
The width of char is calculated with `window-font-width'.
If N is omitted or nil, scroll backwards by one char."
  (interactive "p")
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   (format "window.scrollBy(-%d, 0);"
           (* n (window-font-width)))))

(defun webkitgtk-scroll-top (&optional webkitgtk-id)
  "Scroll webkitgtk to the very top."
  (interactive)
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   "window.scrollTo(pageXOffset, 0);"))

(defun webkitgtk-scroll-bottom (&optional webkitgtk-id)
  "Scroll webkitgtk to the very bottom."
  (interactive)
  (webkitgtk--execute-js
   (or webkitgtk-id (webkitgtk--current-id))
   "window.scrollTo(pageXOffset, window.document.body.scrollHeight);"))

(defun webkitgtk-forward (&optional webkitgtk-id)
  "Go forward in history."
  (interactive)
    (webkitgtk--forward (or webkitgtk-id (webkitgtk--current-id))))

(defun webkitgtk-back (&optional webkitgtk-id)
  "Go back in history."
  (interactive)
    (webkitgtk--back (or webkitgtk-id (webkitgtk--current-id))))

(defun webkitgtk-reload (&optional webkitgtk-id)
  "Reload current URL."
  (interactive)
  (webkitgtk--reload (or webkitgtk-id (webkitgtk--current-id))))

(defun webkitgtk-insert-mode (&optional webkitgtk-id)
  (interactive)
  (webkitgtk--focus (or webkitgtk-id (webkitgtk--current-id))))

(defun webkitgtk--current-id ()
  (when (eq major-mode 'webkitgtk-mode)
    (car (rassoc (current-buffer) webkitgtk--id-buffer-alist))))
  
(defun webkitgtk--adjust-size (frame)
  "Adjust webkitgtk size for window in FRAME"
  ;;(message "adjusting size...")
  (dolist (id-buffer webkitgtk--id-buffer-alist)
    (if (buffer-live-p (cdr id-buffer))
        (with-current-buffer (cdr id-buffer) 
          (let* ((windows (get-buffer-window-list (current-buffer) 'nomini frame)))
            (if (not windows)
                (webkitgtk--hide (car id-buffer))
              (pcase-let ((`(,left ,top ,right ,bottom) (window-inside-pixel-edges
                                                         (car windows))))
                (webkitgtk--show (car id-buffer))
                (webkitgtk--resize (car id-buffer)
                                  left top (- right left) (- bottom top)))
              (dolist (window (cdr windows))
                (switch-to-prev-buffer window)))))
      (webkitgtk--hide (car id-buffer))
      (setq webkitgtk--id-buffer-alist (delq id-buffer webkitgtk--id-buffer-alist)))))

(require 'browse-url)

(defun webkitgtk-browse-url (url &optional new-session)
  "Goto URL with webkitgtk using browse-url.

NEW-SESSION specifies whether to create a new webkitgtk session or use the 
current session."
  (interactive (progn (browse-url-interactive-arg "URL: ")))
  (when (or new-session (not (webkitgtk--current-id)))
    (webkitgtk-new))
  (webkitgtk--load-uri (webkitgtk--current-id) url))
  
(defun webkitgtk-new (&optional buffer-name)
  "Create a new webkitgtk with URL

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `webkitgtk'"
  (unless (boundp 'gtk-version-string)
    (user-error "Your Emacs was not compiled with gtk"))
  (let ((buffer (generate-new-buffer (or buffer-name "*webkitgtk*"))))
    (with-current-buffer buffer
      (webkitgtk-mode)
      (let ((id (webkitgtk--new (make-pipe-process
                                 :name "webkitgtk" :buffer buffer))))
        (push (cons id buffer) webkitgtk--id-buffer-alist)))
    (switch-to-buffer buffer)))

(define-derived-mode webkitgtk-mode
  special-mode "webkitgtk" "webkitgtk view mode."
  (setq buffer-read-only t))

(setq webkitgtk--id-buffer-alist nil)
(add-hook 'window-size-change-functions #'webkitgtk--adjust-size)
;;(remove-hook 'window-size-change-functions #'webkitgtk--adjust-size)

(provide 'webkitgtk)
;;; webkitgtk.el ends here
