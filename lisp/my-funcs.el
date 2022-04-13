;;; my-funcs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal functions, macros, etc.

;;; Code:

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun indent-defun ()
  "Indent the current function."
  (interactive)
  (save-mark-and-excursion
    (mark-defun)
    (indent-region (point) (mark))))

(provide 'my-funcs)
;;; my-funcs.el ends here
