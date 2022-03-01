;;; my-emacs-lisp.el --- emacs lisp configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for emacs-lisp-mode.

;;; Code:

(require 'my-lisp)
(require 'my-string-inflection)

(defun my/recompile-elc-on-save ()
  "Recompile your ELC when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p user-emacs-directory (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil t))

(defun my/conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun my/emacs-lisp-mode-setup ()
  "Sensible defaults for `emacs-lisp-mode'."
  ;; (run-hooks 'my/lisp-coding-hook)
  ;; (eldoc-mode +1)
  ;; (my/recompile-elc-on-save)
  ;; (rainbow-mode +1)
  ;; (setq mode-name "EL")
  ;; (my/conditional-emacs-lisp-checker)
  )

(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-setup)
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(define-key emacs-lisp-mode-map (kbd "C-c C-u") 'string-inflection-all-cycle)

(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here
