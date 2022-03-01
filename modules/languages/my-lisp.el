;;; my-lisp.el --- lisp mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for lisp-mode.

;;; Code:

(when (not (fboundp 'my/wrap-with))
  (require 'core-utils.el))

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (my/wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-\"") (my/wrap-with "\""))
;; FIXME: Pick terminal-friendly binding.
;;(define-key lisp-mode-shared-map (kbd "M-[") (my/wrap-with "["))

;; a great lisp coding hook
(defun my/lisp-coding-hook ())

;; interactive modes don't need whitespace checks
(defun my/interactive-lisp-coding-hook ()
  (whitespace-mode -1))

(provide 'my-lisp)
;;; my-lisp.el ends here
