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

(use-package diminish)

(use-package gcmh
  :demand t
  :diminish
  :config
  (gcmh-mode +1))

(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package recentf :straight nil)


(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package system-packages
  :custom
  (system-packages-noconfirm t))

(use-package use-package-ensure-system-package
  :after (:all system-packages exec-path-from-shell))

(provide 'my-package)
;;; my-package.el ends here
