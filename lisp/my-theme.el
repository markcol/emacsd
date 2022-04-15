;;; my-theme.el --- UI and Theme definitions         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mark Colburn

;; Author: Mark Colburn <mark.colburn@MACY5H2GFY1H9>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; UI and theme settings.

;;; Code:

;; When running in GUI mode.
(when window-system
  ;; Set default font based on priority list
  (let* ((families '("Menlo Nerd Font Mono"
                     "Menlo for Powerline"
                     "Menlo"
                     "Monaco Nerd Font Mono"
                     "Monaco for Powerline"
                     "Monaco"))
         (family (catch 'found
                   (dolist (f families)
		     (when-font-available f
		       (throw 'found f))))))
    (set-face-attribute 'default nil :family family :height 130)))


;; Allow emojis to work.
(when-font-available "Apple Color Emoji"
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

(setq initial-frame-alist '((width . 130)
                            (fullscreen . fullheight)))

(use-package all-the-icons
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))

  (use-package all-the-icons-dired
    :after dired
    :hook
    (dired-mode . all-the-icons-dired-mode)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-1337 t)
  ;; (load-theme 'doom-ayu-mirage t)
  ;; (load-theme 'doom-badger t)
  ;; (load-theme 'doom-material-dark t)
  ;; (load-theme 'doom-dark+ t)
  (load-theme 'doom-oceanic-next t)


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package centaur-tabs
  ;; modern tabs
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward))
  :hook
  ((
    dashboard-mode
    dired-mode
    helpful-mode
    org-agenda-mode
    calendar-mode
    term-mode
    )
   . centaur-tabs-local-mode)
  :config
  (setq
   centaur-tabs-enable-ido-completion nil
   centaur-tabs-modified-marker "*"
   centaur-tabs-set-bar 'left
   centaur-tabs-set-close-button t
   centaur-tabs-set-icons t
   centaur-tabs-set-modified-marker t
   centaur-tabs-show-new-tab-button nil
   centaur-tabs-style "bar"
   )
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package doom-modeline
  :after all-the-icons
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
   doom-modeline-project-detection 'project
   
   ;; If non-nil, a word count will be added to the selection-info modeline segment.
   doom-modeline-enable-word-count t

   ;; Major modes in which to display word count continuously.
   ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
   ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
   ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
   doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
   )

  (when-font-available "Noto Sans"
    (setq doom-modeline-height 1.1)
    (set-face-attribute 'mode-line nil :family "Noto Sans" :height 0.95)
    (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 0.95)))

(provide 'my-theme)
;;; my-theme.el ends here
