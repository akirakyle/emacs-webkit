;;; webkit-dark.el --- simple dark mode for webkit dynamic module -*- lexical-binding: t; -*-

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

(declare-function webkit-add-style "webkit")

(defvar webkit--dark-style "
html {
  -webkit-filter: hue-rotate(180deg) invert(90%) !important;
}

iframe,img,video {
  -webkit-filter: brightness(80%) invert(100%) hue-rotate(180deg) !important;
}")

(defcustom webkit-dark-mode nil
  "Turn on webkit dark mode globally."
  :type 'bool
  :group 'webkit)

(defun webkit-dark-toggle ()
  (interactive)
  (if webkit-dark-mode
      (webkit-remove-style webkit--dark-style)
    (webkit-add-style webkit--dark-style))
  (setq-local webkit-dark-mode (not webkit-dark-mode)))

(defun webkit-dark-init ()
  (when webkit-dark-mode
    (webkit-add-style webkit--dark-style)))

(add-hook 'webkit-new-hook #'webkit-dark-init)
(define-key webkit-mode-map (kbd "C-c d") 'webkit-dark-toggle)

(provide 'webkit-dark)
;;; webkit-dark.el ends here
