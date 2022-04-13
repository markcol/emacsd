;;; my-funcs.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal functions, macros, etc.

;;; Code:

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(provide 'my-funcs)
;;; my-funcs.el ends here
