;;; my-org.el --- Org-mode configuration             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mark Colburn

;;; Commentary:

;; Org-mode configuration.

;;; Code:

(use-package org
  :custom
  (org-directory (expand-file-name "~/Documents/org"))
  ;; :hook (org-mode . my/org-mode-hook)
  ;; :preface
  ;; (defun my/org-mode-hook ()
  ;;   "Mode hook function for org-mode."
  ;;   (variable-pitch-mode +1)
  ;;   (visual-line-mode +1)
  ;;   (org-indent-mode +1)
  ;;   (autopair-mode -1))
  ;; :custom
  ;; (org-adapt-indentation t)
  ;; (org-src-tab-acts-natively t)
  ;; :config
  ;; (setq
  ;;  org-clock-persist 'history
  ;;  org-latex-caption-above nil
  ;;  org-log-done t
  ;;  org-src-fontify-natively t
  ;;  org-src-preserve-indentation t
  ;;  )
  ;; (org-clock-persistence-insinuate))
  )

(use-package org-roam
  :after (org)
  :custom
  (org-roam-directory (expand-file-name "~/Documents/org/roam"))
  (org-roam-completion-everywhere t)
  :init
  (setq org-roam-v2-ack t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  ;; (org-roam-db-autosync-mode)
  )

;; (use-package htmlize
;;   :disabled
;;   :defer t
;;   :after (org-mode)
;;   :custom
;;   (org-html-htmlize-output-type 'css)
;;   (org-html-htmlize-font-prefix "org-"))


;; (use-package org-bullets
;;   :disabled
;;   :after (org-mode)
;;   :custom
;;   ;; org-bullets-bullet-list
;;   ;; default: "◉ ○ ✸ ✿"
;;   ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
;;   ;; Small: ► • ★ ▸
;;   (org-bullets-bullet-list '("•"))
;;   ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
;;   ;; (org-ellipsis "⤵")
;;   (org-ellipsis "…")
;;   :hook
;;   (org-mode . org-bullets-mode))

;; (use-package org-ref
;;   :disabled
;;   :defer t
;;   :ensure-system-package (mactex . "brew install -cask mactex")
;;   :after (org-mode)
;;   :init
;;   (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

;; (use-package org-tree-slide
;;   :after (org)
;;   :defer t
;;   :custom
;;   (org-image-actual-width nil)
;;   (org-tree-slide-activate-message "Presentation started!")
;;   (org-tree-slide-deactivate-message "Presentation finished!")
;;   :hook ((org-tree-slide-play . my/org-tree-slide-setup)
;;          (org-tree-slide-stop . my/org-tree-slide-end))
;;   :bind (:map org-tree-slide-mode-map
;;               ("C-<" . org-tree-slide-move-previous-tree)
;;               ("C->" . org-tree-slide-move-next-tree))
;;   :preface
;;   (defun my/org-tree-slide-setup ()
;;     "Function run before slide mode starts."
;;     (org-display-inline-images)
;;     (hide-mode-line-mode +1))

;;   (defun my/org-tree-slide-end ()
;;     "Function run after slide mode ends."
;;     (org-display-inline-images)
;;     (hide-mode-line-mode 0))
;;   :config
;;   ;; Hide the modeline to allow a clean screen when using org-tree-slide-mode.
;;   (use-package hide-mode-line :defer t))

;; (use-package auctex
;;   :ensure-system-package (mactex . "brew install -cask mactex")
;;   :defer t
;;   :config
;;   (use-package latex-preview-pane :defer t))

(provide 'my-org)
;;; my-org.el ends here
