;;; evil-collection-webkit.el --- Evil bindings for webkit -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Kyle

;; Author: Akira Kyle <ak@akirakyle.com>
;; Maintainer: Akira Kyle <ak@akirakyle.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, webkit, tools

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
;; Evil bindings for webkit.

;;; Code:
(require 'webkit)
(require 'evil-collection)

(defvar evil-collection-webkit-maps '(webkit-mode-map))

(defun webkit-scroll-up-half (&optional webkit-id)
  (interactive)
  (webkit-scroll-by-percent 0.5))

(defun webkit-scroll-down-half (&optional webkit-id)
  (interactive)
  (webkit-scroll-by-percent -0.5))

(defun evil-collection-webkit-insert-on-insert ()
  (add-hook 'evil-insert-state-entry-hook 'webkit-insert-mode nil t))

(defun evil-collection-webkit-unfocus-to-normal-mode (val)
  (evil-normal-state))

;;;###autoload
(defun evil-collection-xwidget-setup ()
  "Set up `evil' bindings for `webkit'."
  (evil-collection-define-key 'normal 'webkit-mode-map
    "q" 'quit-window
    "k" 'webkit-scroll-down-line
    "j" 'webkit-scroll-up-line
    "h" 'webkit-scroll-backward
    "l" 'webkit-scroll-forward
    "d" 'webkit-scroll-up-half
    "u" 'webkit-scroll-down-half
    (kbd "C-f") 'webkit-scroll-up
    (kbd "C-b") 'webkit-scroll-down
    "+" 'webkit-zoom-in
    "=" 'webkit-zoom-in
    "-" 'webkit-zoom-out
    "f" 'webkit-ace
    "/" 'webkit-search
    "n" 'webkit-search-next
    "N" 'webkit-search-previous
    "ESC" 'webkit-search-finish
    "R" 'webkit-reload
    "gr" 'webkit-reload
    "H" 'webkit-back
    "L" 'webkit-forward
    "gu" 'webkit
    "gg" 'webkit-scroll-top
    "G" 'webkit-scroll-bottom
    "y" 'webkit-copy-selection
    "Y" 'webkit-copy-url)

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'webkit-mode-map
      (kbd "C-d") 'webkit-scroll-up-half))
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'webkit-mode-map
      (kbd "C-u") 'webkit-scroll-down-half))

  (add-hook 'webkit-mode-hook #'evil-collection-webkit-insert-on-insert)
  (advice-add 'webkit--callback-unfocus
              :after #'evil-collection-webkit-unfocus-to-normal-mode)
  )

;; add advice around  to go back to normal mode

(provide 'evil-collection-webkit)
;;; evil-collection-webkit.el ends here
