;;; init.el --- Emacs initialization file            -*- lexical-binding: t; -*-

;;; Commentary:

;; My Emacs initialization file.

;;; Code:

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-funcs)
(require 'my-config)
(require 'my-package)
(require 'my-theme)
(require 'my-prog)

(unbind-key "s-t")
(bind-key* "C-c /" #'comment-dwim)	
(when window-system
  (unbind-key "C-z"))

(use-package which-key
  :diminish
  :custom
  (which-key-enabled-define-key t)
  (which-key-show-transient-maps t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package yasnippet
  :defer t
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  :hook
  ((
    prog-mode
    feature-mode
    ) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package man
  :defer t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package info-colors
  :defer t
  :commands (info-colors-fontify-mode)
  :defines (Info-selection-mode)
  :hook
  (Info-selection . info-colors-fontify-node))

(use-package helpful
  :defer t
  :commands (helpful-command helpful-at-point helpful-function helpful-callable helpful-key helpful-varaiable)
  :bind
  (
   ("C-h C"   . helpful-command)
   ("C-h C-d" . helpful-at-point)
   ("C-h F"   . helpful-function)
   ("C-h f"   . helpful-callable)
   ("C-h k"   . helpful-key)
   ("C-h v"   . helpful-variable)
   ))

(use-package elisp-demos
  :defer t
  :after helpful
  :commands (elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1)
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(use-package unfill
  ;; fill-paragraph is nice, but emacs weirdly lacks a convenient way to
  ;; unfill paragraphs once they're filled.  This package adds that
  ;; functionality.
  :bind
  ([remap fill-paragraph] . unfill-toggle))

(use-package eldoc
  :straight nil
  :after (paredit)
  ;; :diminish eldoc-mode
  :commands eldoc-mode
  :hook
  ((elisp-mode) . eldoc-mode)
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(use-package restart-emacs
  ;; Restart emacs.
  :defer t)

(use-package tooltip
  :straight nil
  :demand
  :config
  (setq tooltip-resize-echo-area t)
  (tooltip-mode +1)
  )


;;; init.el ends here
