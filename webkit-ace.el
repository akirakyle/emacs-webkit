(defcustom webkit-ace-chars "asdfghjklweio"
  "Link hint characters."
  :type 'string
  :group 'webkit)

(setq webkit--hints-script
      (webkit--file-to-string
       (expand-file-name "hints.js" webkit-base)))
(setq webkit--hints-style
      (webkit--file-to-string
       (expand-file-name "hints.css" webkit-base)))

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

(provide 'webkit-ace)
;;; webkit-history.el ends here
