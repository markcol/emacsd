;;; my-embark.el --- embark configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for embark.

;;; Code:

(require 'my-marginalia)
(require 'my-consult)

(use-package embark
  :bind
  (("C-."   . embark-act)       ;; pick some comfortable binding
   ("C-,"   . embark-dwim)      ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'embark-keymap-alist '(project-buffer . embark-buffer-map))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'my-embark)
;;; my-embark.el ends here
