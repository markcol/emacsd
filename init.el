;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The file that starts it all.

;;; Code:

(setq load-prefer-newer t)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system darwin
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))	

(global-hl-line-mode t)

;;; init.el ends here
