;;; my-org-mode.el --- org-mode configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for org-mode.

;;; Code:

(require 'my-display-fill-column)
(require 'my-display-line-numbers)
(require 'my-flyspell)
(require 'my-smart-shift)
(require 'my-smartparens)

(use-package org
  :straight (:type built-in)
  :bind
  (:map org-mode-map
        ("C-j" . org-return-indent)
        ("RET" . org-return-indent)
        ("M-{" . org-promote-subtree)
        ("M-}" . org-demote-subtree)
        ("M-P" . org-metaup)
        ("M-N" . org-metadown)
        ("C-M-n" . outline-next-visible-heading)
        ("C-M-p" . outline-previous-visible-heading)
        ("C-c [" . smart-shift-left)
        ("C-c ]" . smart-shift-right))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit))

  :hook
  (org-mode . my/org-mode-setup)

  :custom
  (org-adapt-indentation nil)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (org-catch-invisible-edits 'show)
  (org-ctrl-k-protect-subtree t)
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  (org-hide-leading-stars nil)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)

  (org-directory (if (file-directory-p "~/Dropbox/org")
                     "~/Dropbox/org" "~/org"))

  (org-babel-load-languages
   '((awk . t)
     (calc . t)
     (clojure . t)
     (css . t)
     (dot . t)
     (emacs-lisp . t)
     (forth . t)
     (fortran . t)
     (haskell . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (org . t)
     (perl . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (sass . t)
     (scheme . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))

  :preface
  (defun my/org-mode-setup ()
    (setq fill-column 80
          whitespace-action '(auto-cleanup)
          tab-width 2)

    (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
    (org-set-emph-re 'org-emphasis-regexp-components
                     org-emphasis-regexp-components)

    (auto-fill-mode t)
    (my/display-fill-column)
    (my/display-indentation)
    (my/display-line-numbers)
    (flyspell-mode)
    (smartparens-mode +1)
    (visual-line-mode +1)
    (whitespace-mode +1))

  :config
  (require 'org-mouse)
  (setq org-id-locations-file
        (expand-file-name ".org-id-locations" org-directory)))

(provide 'my-org-mode)
;;; my-org-mode.el ends here
