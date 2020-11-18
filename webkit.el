;;; webkit.el --- webkit dynamic module -*- lexical-binding: t; -*-

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
;; webkit dynamic module

;;; Code:

;; Don't require dynamic module at byte compile time.
(declare-function webkit--new "webkit-module")
(declare-function webkit--destroy "webkit-module")
(declare-function webkit--resize "webkit-module")
(declare-function webkit--move-to-focused-frame "webkit-module")
(declare-function webkit--hide "webkit-module")
(declare-function webkit--show "webkit-module")
(declare-function webkit--focus "webkit-module")
(declare-function webkit--unfocus "webkit-module")
(declare-function webkit--forward "webkit-module")
(declare-function webkit--back "webkit-module")
(declare-function webkit--reload "webkit-module")
(declare-function webkit--get-zoom "webkit-module")
(declare-function webkit--set-zoom "webkit-module")
(declare-function webkit--get-title "webkit-module")
(declare-function webkit--get-uri "webkit-module")
(declare-function webkit--load-uri "webkit-module")
(declare-function webkit--search "webkit-module")
(declare-function webkit--search-finish "webkit-module")
(declare-function webkit--search-next "webkit-module")
(declare-function webkit--search-previous "webkit-module")
(declare-function webkit--start-web-inspector "webkit-module")
(declare-function webkit--enable-javascript "webkit-module")
(declare-function webkit--execute-js "webkit-module")
(declare-function webkit--add-user-style "webkit-module")
(declare-function webkit--remove-all-user-styles "webkit-module")
(declare-function webkit--add-user-script "webkit-module")
(declare-function webkit--remove-all-user-scripts "webkit-module")
(declare-function webkit--register-script-message "webkit-module")
(declare-function webkit--unregister-script-message "webkit-module")

(defun webkit--file-to-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defconst webkit--base (file-name-directory load-file-name))

(require 'webkit-module)
(require 'webkit-history)
(require 'webkit-ace)
(require 'browse-url)
(require 'eww)

(defgroup webkit nil
  "webkit browser ."
  :group 'convenience)

(defcustom webkit-search-prefix "https://duckduckgo.com/html/?q="
  "Prefix URL to search engine."
  :group 'webkit
  :type 'string)

(defcustom webkit-own-window nil
  "Whether webkit should use its own window instead of
attemptting to embed itself in its buffer. The curretly focused
frame must be display-graphic-p and either x or pgtk when
webkit-new is run in order for embedding to work."
  :group 'webkit
  :type 'boolean)

(defvar webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'webkit)
    (define-key map "f" 'webkit-forward)
    (define-key map "b" 'webkit-back)
    (define-key map "r" 'webkit-reload)
    (define-key map "i" 'webkit-insert-mode)
    (define-key map "+" 'webkit-zoom-in)
    (define-key map "-" 'webkit-zoom-out)
    (define-key map (kbd "C-y") 'webkit-copy-selection)

    ;;similar to image mode bindings
    (define-key map (kbd "SPC")                 'webkit-scroll-up)
    (define-key map (kbd "S-SPC")               'webkit-scroll-down)
    (define-key map (kbd "DEL")                 'webkit-scroll-down)

    (define-key map [remap scroll-up]           'webkit-scroll-up-line)
    (define-key map [remap scroll-up-command]   'webkit-scroll-up)

    (define-key map [remap scroll-down]         'webkit-scroll-down-line)
    (define-key map [remap scroll-down-command] 'webkit-scroll-down)

    (define-key map [remap forward-char]        'webkit-scroll-forward)
    (define-key map [remap backward-char]       'webkit-scroll-backward)
    (define-key map [remap right-char]          'webkit-scroll-forward)
    (define-key map [remap left-char]           'webkit-scroll-backward)
    (define-key map [remap previous-line]       'webkit-scroll-down-line)
    (define-key map [remap next-line]           'webkit-scroll-up-line)

    (define-key map [remap beginning-of-buffer] 'webkit-scroll-top)
    (define-key map [remap end-of-buffer]       'webkit-scroll-bottom)
    map)
  "Keymap for `webkit-mode'.")

(defun webkit-zoom-in (&optional webkit-id)
  "Increase webkit view zoom factor."
  (interactive)
  (webkit--set-zoom (or webkit-id webkit--id)
                    (+ (webkit--get-zoom (or webkit-id webkit--id)) 0.1)))

(defun webkit-zoom-out (&optional webkit-id)
  "Decrease webkit view zoom factor."
  (interactive)
  (webkit--set-zoom (or webkit-id webkit--id)
                    (+ (webkit--get-zoom (or webkit-id webkit--id)) -0.1)))

(defun webkit-scroll-by-pixels (arg &optional webkit-id)
  "Scroll webkit up by ARG pixels.

Stops if bottom of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls down."
  (interactive "P")
  (webkit--execute-js (or webkit-id webkit--id)
                      (format "window.scrollBy(0, %d);" arg)))

(defun webkit-scroll-by-percent (arg &optional webkit-id)
  "Scroll webkit up by ARG percent.

Stops if bottom of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls down."
  (interactive "P")
  (pcase-let ((`(,left ,top ,right ,bottom)
               (window-inside-pixel-edges (selected-window))))
    (webkit--execute-js (or webkit-id webkit--id)
                        (format "window.scrollBy(0, %d);"
                                (* arg (- bottom top))))))

(defun webkit-scroll-up (&optional webkit-id)
  "Scroll webkit up by full window height."
  (interactive)
  (webkit-scroll-by-percent 1))

(defun webkit-scroll-down (&optional webkit-id)
  "Scroll webkit down by full window height."
  (interactive)
  (webkit-scroll-by-percent -1))

(defun webkit-scroll-up-line (&optional n webkit-id)
  "Scroll webkit up by N lines.
The height of line is calculated with `window-font-height'.
Stop if the bottom edge of the page is reached.
If N is omitted or nil, scroll up by one line."
  (interactive "p")
  (webkit-scroll-by-pixels (* n (window-font-height))))

(defun webkit-scroll-down-line (&optional n webkit-id)
  "Scroll webkit down by N lines.
The height of line is calculated with `window-font-height'.
Stop if the top edge of the page is reached.
If N is omitted or nil, scroll down by one line."
  (interactive "p")
  (webkit-scroll-by-pixels (* (* -1 n) (window-font-height))))

(defun webkit-scroll-forward (&optional n webkit-id)
  "Scroll webkit horizontally by N chars.
The width of char is calculated with `window-font-width'.
If N is omitted or nil, scroll forwards by one char."
  (interactive "p")
  (webkit--execute-js
   (or webkit-id webkit--id)
   (format "window.scrollBy(%d, 0);"
           (* n (window-font-width)))))

(defun webkit-scroll-backward (&optional n webkit-id)
  "Scroll webkit back by N chars.
The width of char is calculated with `window-font-width'.
If N is omitted or nil, scroll backwards by one char."
  (interactive "p")
  (webkit--execute-js
   (or webkit-id webkit--id)
   (format "window.scrollBy(-%d, 0);"
           (* n (window-font-width)))))

(defun webkit-scroll-top (&optional webkit-id)
  "Scroll webkit to the very top."
  (interactive)
  (webkit--execute-js
   (or webkit-id webkit--id)
   "window.scrollTo(pageXOffset, 0);"))

(defun webkit-scroll-bottom (&optional webkit-id)
  "Scroll webkit to the very bottom."
  (interactive)
  (webkit--execute-js
   (or webkit-id webkit--id)
   "window.scrollTo(pageXOffset, window.document.body.scrollHeight);"))

(defun webkit--copy-selection-callback (selection)
  (let ((print-escape-newlines t)
        (text (elt (json-parse-string selection) 0)))
    (kill-new text)
    (message "copied \"%s\"" text)))

(defun webkit-copy-selection (&optional webkit-id)
  "Copy the webkit selection to the kill ring."
  (interactive)
  (webkit--execute-js
   (or webkit-id webkit--id)
   "[window.getSelection().toString()];" "webkit--copy-selection-callback"))

(defun webkit-copy-url (&optional webkit-id)
  "Copy the webkit url to the kill ring."
  (interactive)
  (let ((uri (webkit--get-uri (or webkit-id webkit--id))))
    (message "Copied %s" uri)
    (kill-new uri)))

(defun webkit-forward (&optional webkit-id)
  "Go forward in history."
  (interactive)
  (webkit--forward (or webkit-id webkit--id)))

(defun webkit-back (&optional webkit-id)
  "Go back in history."
  (interactive)
  (webkit--back (or webkit-id webkit--id)))

(defun webkit-reload (&optional webkit-id)
  "Reload current URL."
  (interactive)
  (webkit--reload (or webkit-id webkit--id)))

(defun webkit-search (query &optional case webkit-id)
  "Search in webkit for QUERY.
Seach is case sensitive if CASE is not nil."
  (interactive (list (read-string "Query: ") nil))
  (webkit--search (or webkit-id webkit--id) query case))

(defun webkit-search-finish (&optional webkit-id)
  "Stop highlighting search results in webkit."
  (interactive)
  (webkit--search-finish (or webkit-id webkit--id)))

(defun webkit-search-next (&optional webkit-id)
  "Go to next search result in webkit."
  (interactive)
  (webkit--search-next (or webkit-id webkit--id)))

(defun webkit-search-previous (&optional webkit-id)
  "Go to previous search result in webkit."
  (interactive)
  (webkit--search-previous (or webkit-id webkit--id)))

(defun webkit-start-web-inspector (&optional webkit-id)
  "Start webkit's webk inspector."
  (interactive)
  (webkit--start-web-inspector (or webkit-id webkit--id)))

(defun webkit-enable-javascript (&optional enable webkit-id)
  "Enable external javascript execution if ENABLE is not nil and
disable it otherwise."
  (interactive "P")
  (webkit--enable-javascript (or webkit-id webkit--id) enable))

(defun webkit-insert-mode (&optional webkit-id)
  (interactive)
  (message "Entering webkit insert mode, press C-g to exit")
  (webkit--focus (or webkit-id webkit--id)))

(defun webkit--callback-unfocus (val)
  (message "C-g pressed in webkit... exiting insert mode")
  (webkit--unfocus webkit--id))

(defun webkit--load-finished (msg)
  (run-hooks 'webkit-load-finished-hook))

(defun webkit--callback-title (title)
  (run-hook-with-args 'webkit-title-changed-functions title))

(defun webkit--callback-uri (uri)
  (run-hook-with-args 'webkit-uri-changed-functions uri))

(defun webkit--callback-progress (progress)
  (run-hook-with-args 'webkit-progress-changed-functions
                      (string-to-number progress)))

(defun webkit--echo-progress (progress)
  (message "%.0f%%" progress))

(defun webkit--callback-new-view (uri)
  (webkit-new uri))

(defun webkit--callback-download-request (uri)
  (message "TODO: implement download request for %s" uri))

(defun webkit-rename-buffer (title)
  (if (string= "" title)
      (let ((uri (webkit--get-uri webkit--id)))
        (if (string= "" uri)
            (rename-buffer "*webkit*" t)
          (rename-buffer uri t)))
    (rename-buffer title t)))

(defun webkit--filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)
      (goto-char 1)
      (while (re-search-forward "\\([^\x00]*\\)\x00\\([^\x00]*\\)\x00" nil t)
        (let ((id (match-string 1))
              (msg (match-string 2)))
          (delete-region 1 (match-end 0))
          ;;(message "id: %s; message: %s" id msg)
          (funcall (intern id) msg))))))

(defun webkit--adjust-size (frame)
  "Adjust webkit size for window in FRAME"
  ;;(message "adjusting size...")
  (dolist (buffer webkit--buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((windows (get-buffer-window-list buffer 'nomini t)))
          (if (not windows)
              (webkit--hide webkit--id)
            (let* ((show-window (if (memq (selected-window) windows)
                                    (selected-window)
                                  (car windows)))
                   (hide-windows (remq show-window windows)))
              (when (eq (window-frame show-window) (selected-frame))
                (webkit--move-to-focused-frame webkit--id))
              (pcase-let ((`(,left ,top ,right ,bottom)
                           (window-inside-pixel-edges show-window)))
                (webkit--show webkit--id)
                (webkit--resize webkit--id left top
                                (- right left) (- bottom top)))
              (dolist (window hide-windows)
                (switch-to-prev-buffer window)))))))))

(defun webkit--close (msg)
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
  (kill-this-buffer))

(defun webkit--kill-buffer ()
  (when (eq major-mode 'webkit-mode)
    ;;(webkit--hide webkit--id)
    (webkit--destroy webkit--id)
    (setq webkit--buffers (delq (current-buffer) webkit--buffers))))

(setq webkit--script (webkit--file-to-string
                         (expand-file-name "script.js" webkit--base)))
(setq webkit--style (webkit--file-to-string
                        (expand-file-name "style.css" webkit--base)))

(defun webkit-new (&optional url buffer-name noquery)
  "Create a new webkit with URL

If called with an argument BUFFER-NAME, the name of the new buffer will
be set to BUFFER-NAME, otherwise it will be `webkit'.
Returns the newly created webkit buffer"
  (let ((buffer (generate-new-buffer (or buffer-name "*webkit*"))))
    (with-current-buffer buffer
      (webkit-mode)
      (setq webkit--id (webkit--new
                           (make-pipe-process :name "webkit"
                                              :buffer buffer
                                              :filter 'webkit--filter
                                              :noquery noquery)
                           webkit-own-window))
      (push buffer webkit--buffers)
      (webkit--register-script-message
       webkit--id "webkit--callback-unfocus")
      (webkit--add-user-script webkit--id webkit--script)
      (webkit--add-user-style webkit--id webkit--style)
      (when url (webkit--load-uri webkit--id url))
      ;; hack necessary to get correct z-ordering
      ;;(when (fboundp 'posframe-delete-all)
      ;;  (posframe-delete-all))
      (run-hooks 'webkit-new-hook)
      (switch-to-buffer buffer))))

;;;###autoload
(defun webkit-browse-url (url &optional new-session)
  "Goto URL with webkit using browse-url.

NEW-SESSION specifies whether to create a new webkit session or use the 
current session."
  (interactive (progn (browse-url-interactive-arg "URL: ")))
  (if (or new-session (not webkit--buffers))
      (webkit-new url)
    (let* ((id (or webkit--id (with-current-buffer (car webkit--buffers)
                               webkit--id)))
          (buffer (seq-find  (lambda (buffer)
                               (with-current-buffer buffer 
                                 (when (eq id webkit--id) buffer)))
                             webkit--buffers)))
      (webkit--load-uri id url)
      (switch-to-buffer buffer))))

;;;###autoload
(defun webkit (url &optional arg)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `webkit-search-prefix'.

If called with a prefix ARG, create a new webkit buffer instead of reusing
the default webkit buffer."
  (interactive
   (let ((prompt "URL or keywords: "))
     (list (webkit-history-completing-read prompt)
           (prefix-numeric-value current-prefix-arg))))
  (let ((eww-search-prefix webkit-search-prefix))
    (webkit-browse-url (eww--dwim-expand-url url) (eq arg 4))))

(define-derived-mode webkit-mode special-mode "WebKit"
  "webkit view mode."
  (setq buffer-read-only nil))

(make-variable-buffer-local 'webkit--id)
(setq webkit--buffers nil)

(unless webkit-own-window
  (add-hook 'window-size-change-functions #'webkit--adjust-size))
;;(remove-hook 'window-size-change-functions #'webkit--adjust-size)

(add-hook 'webkit-title-changed-functions 'webkit-rename-buffer)
(add-hook 'webkit-progress-changed-functions 'webkit--echo-progress)
(add-hook 'kill-buffer-hook #'webkit--kill-buffer)

(provide 'webkit)
;;; webkit.el ends here
