;;; my-package.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure the package system using Stright.

;;; Code:

(setq load-prefer-newer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(put 'use-package 'lisp-indent-function 1)

(require 'bind-key)
(use-package s :defer t)
(use-package dash :defer t)

(use-package diminish
  :config
  (diminish 'visual-line-mode))

(use-package gcmh
  :demand t
  :diminish
  :hook
  ;; collect garbage when emacs has loss focus
  (focus-out-hook . gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600)
  :config
  (gcmh-mode +1))

(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package recentf
  :straight nil
  :after no-littering
 :custom
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 25)
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "\\straight")
  (add-to-list 'recentf-exclude "\\var")
  (add-to-list 'recentf-exclude "\\data")
  (recentf-mode +1))

(use-package saveplace
  ;; Save my place when opening/closing files:
  :config
  (save-place-mode +1))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package system-packages
  :after (exec-path-from-shell)
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :after (system-packages))

(provide 'my-package)
;;; my-package.el ends here
