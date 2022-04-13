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


;; Allow emojis to work.
(set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

(use-package all-the-icons
  :config
  (use-package all-the-icons-dired
    :hook
    (dired-mode . all-the-icons-dired-mode))
  )

(set-frame-font "Fira Code-13" t t)

(load-theme 'modus-vivendi)
(setq initial-frame-alist '(
			    (width . 130)
			    (height . 90)
			    (fullscreen . fullheight)))

(use-package centaur-tabs
  :bind
  (("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward))
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-set-close-button t)
  (centaur-tabs-enable-ido-completion nil))

(provide 'my-theme)
;;; my-theme.el ends here
