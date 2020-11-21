;;; webkit-ace.el --- ace for webkit dynamic module -*- lexical-binding: t; -*-

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

(require 'webkit)

(declare-function webkit--file-to-string "webkit")
(declare-function webkit--execute-js "webkit-module")
(declare-function webkit--focus "webkit-module")
(declare-function webkit--add-user-script "webkit-module")
(declare-function webkit--add-user-style "webkit-module")

(defconst webkit--base (file-name-directory load-file-name))

(defvar webkit--id)

(defvar webkit--hints-script
      (webkit--file-to-string
       (expand-file-name "hints.js" webkit--base)))
(defvar webkit--hints-style
      (webkit--file-to-string
       (expand-file-name "hints.css" webkit--base)))

(defcustom webkit-ace-chars "asdfghjklweio"
  "Link hint characters."
  :type 'string
  :group 'webkit)

(defun webkit-ace--callback (msg)
  (message msg))

(defun webkit-ace (&optional webkit-id)
  "Start a webkit ace jump."
  (interactive)
  (webkit--execute-js
   (or webkit-id webkit--id)
   (format "__WKViewHints('%s');" webkit-ace-chars))
  (webkit--focus (or webkit-id webkit--id)))

(defun webkit-ace-init ()
  ;;(webkit--register-script-message webkit--id "webkit-ace--callback")
  (webkit--add-user-script webkit--id webkit--hints-script)
  (webkit--add-user-style webkit--id webkit--hints-style)
  )

(add-hook 'webkit-new-hook #'webkit-ace-init)
(define-key webkit-mode-map "o" 'webkit-ace)

(provide 'webkit-ace)
;;; webkit-ace.el ends here
