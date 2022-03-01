;;; core-utils.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my/recursive-add-to-load-path (dir)
  "Add DIR and all its sub-directories to `load-path'."
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (my/recursive-add-to-load-path name)))))

(defun my/smart-open-line-above ()
  "Insert an empty line above the current line.

Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(defun my/align-region-to-equals (begin end)
  "Align region (specified with BEGIN and END) to equal signs."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun my/align-region-to-opening-brace (begin end)
  "Align region (specified with BEGIN and END) to equal opening brace."
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\){" 1 1 ))

(defun my/yank-pop-forwards (arg)
  "Yank pop in reverse."
  (interactive "p")
  (yank-pop (- arg)))

(defun my/other-window-reverse ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(defun my/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.

Borrowed from: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun my/ignore-error-wrapper (fn)
  "Function return new function that ignore errors.

The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors (funcall fn)))))

;; Shamelessly ripped from textmate.el: https://github.com/defunkt/textmate.el
(defmacro my/allow-line-as-region-for-function (orig-function)
  `(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
       ()
     ,(format "Like `%s', but acts on the current line if mark is not active."
              orig-function)
     (interactive)
     (if mark-active
         (call-interactively (function ,orig-function))
       (save-excursion
         ;; define a region (temporarily) -- so any C-u prefixes etc. are
         ;; preserved.
         (beginning-of-line)
         (set-mark (point))
         (end-of-line)
         (call-interactively (function ,orig-function))))))

(defun my/add-lexical-binding ()
  (beginning-of-buffer)
  (end-of-line)
  (insert " -*- lexical-binding: t; -*-")
  (setq lexical-binding t))

(provide 'core-utils)
;;; core-utils.el ends here
