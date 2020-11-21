;;; webkit-history.el --- history for webkit dynamic module -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Akira Kyle

;; Author: Akira Kyle <akira@akirakyle.com>
;; URL: https://github.com/akirakyle/emacs-webkit
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (webkit "0.1"))

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
;; See README.org

;;; Code:

(declare-function webkit--get-title "webkit-module")
(declare-function webkit--get-uri "webkit-module")

(defvar webkit--id)

(defcustom webkit-history-filename "~/.emacs.d/webkit-history"
  "File to store history of `webkit' sessions."
  :type 'file
  :group 'webkit)

(defvar webkit-history-table nil)

(cl-defstruct webkit-history-item uri title last-time (visit-count 1))

(defun webkit-history-item-serialize (item)
  (list (webkit-history-item-uri item)
        (webkit-history-item-title item)
        (webkit-history-item-last-time item)))

(defun webkit-history-item-deserialize (list)
  (make-webkit-history-item
   :uri (car list)
   :title (cadr list)
   :last-time (caddr list)))

(defun webkit-history-completion-text (item)
  (let* ((title (webkit-history-item-title item))
         (uri (webkit-history-item-uri item))
         (text (concat title " (" uri ")")))
    (put-text-property (+ 2 (length title)) (1- (length text)) 'face 'link text)
    text))

(defun webkit-history-completing-read (prompt)
  "Prompt for a URI using COMPLETING-READ from webkit history."
  (let ((completions ())
        (key-to-count (lambda (k) (webkit-history-item-visit-count
                                    (gethash (cdr k) webkit-history-table)))))
    (maphash (lambda (k v)
               (push (cons (webkit-history-completion-text v) k) completions))
             webkit-history-table)
    (setq completions (sort completions (lambda (k1 k2)
                                          (> (funcall key-to-count k1)
                                             (funcall key-to-count k2)))))
    (let* ((completion (completing-read prompt completions))
           (uri (cdr (assoc completion completions))))
      (if uri uri completion))))

(defun webkit-history-add-item (item)
  (let* ((uri (webkit-history-item-uri item))
         (previous-item (gethash uri webkit-history-table)))
    (when previous-item
      (setf (webkit-history-item-visit-count item)
            (+ 1 (webkit-history-item-visit-count previous-item))))
    (puthash uri item webkit-history-table)))

(defun webkit-history-add ()
  (let ((save-silently t)
        (new-item (make-webkit-history-item
                   :title (webkit--get-title webkit--id)
                   :uri (webkit--get-uri webkit--id)
                   :last-time (time-convert (current-time) 'integer))))
    (unless (string= (webkit-history-item-uri new-item) "about:blank")
      (webkit-history-add-item new-item)
      (when webkit-history-filename
        (append-to-file (format "%S\n" (webkit-history-item-serialize new-item))
                        nil webkit-history-filename)))))

(defun webkit-history-load ()
  (with-current-buffer (find-file-noselect webkit-history-filename)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (webkit-history-add-item
           (webkit-history-item-deserialize (read (current-buffer)))))
      (end-of-file nil))
    (kill-buffer)))

(defun webkit-history-initialize ()
  "Setup required data structure and load history from WEBKIT-HISTORY-FILENAME."
  (add-hook 'webkit-load-finished-hook #'webkit-history-add)
  (setq webkit-history-table (make-hash-table :test 'equal))
  (when webkit-history-filename
    (webkit-history-load))
  nil)

(webkit-history-initialize)

(provide 'webkit-history)
;;; webkit-history.el ends here
