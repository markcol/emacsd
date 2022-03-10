;;; my-themes.el --- Custom themes.

;;; Commentary:

;; Enable loading custom themes.

;;; Code:

(defvar my/themes-dir (expand-file-name "themes" user-emacs-directory)
  "Root directory for Emacs custom themes.")

(add-to-list 'custom-theme-load-path my/themes-dir)

;; Ensure mode-line uses fixed-width font in nightly builds after 2021-11-26
;; when new mode-line faces were introduced:
;; https://github.com/emacs-mirror/emacs/commit/57bb675cde25bc1b54d8eb8716b0024d5c1d5687
(if (get 'mode-line-active 'face-defface-spec)
    (set-face-attribute 'mode-line-active nil :inherit 'mode-line))
(if (get 'mode-line-inactive 'face-defface-spec)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line))

;; Globally disable setting face weight to bold.
(defvar my/set-face-ignore-attributes '(:weight))
(defadvice set-face-attribute
    (before ignore-attributes (face frame &rest args) activate)
  (setq args
        (apply 'nconc
               (mapcar (lambda (i)
                         (let ((attribute (nth i args))
                               (value (nth (1+ i) args)))
                           (if (not (memq attribute
                                          my/set-face-ignore-attributes))
                               (list attribute value))))
                       (number-sequence 0 (1- (length args)) 2)))))

;; Required by all-the-icons
(use-package memoize)

(use-package all-the-icons
  :after memoize
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :defer t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-height 25)
  (doom-modeline-indent-info nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-workspace-name nil)

  :config
  (doom-modeline-mode))

(use-package doom-themes
  :custom
  ;; Global doom-themes options
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)  ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-enable-variable-pitch nil)

  (doom-nord-light-brighter-comments nil)
  (doom-nord-light-brighter-modeline nil)
  (doom-nord-light-comment-bg nil)
  (doom-nord-light-padded-modeline nil)

  (doom-vibrant-brighter-comments nil)
  (doom-vibrant-brighter-modeline nil)
  (doom-vibrant-comment-bg nil)
  (doom-vibrant-padded-modeline nil)

  (nlinum-highlight-current-line t)

  :config
  ;; By default load the doom-vibrant theme.
  (my/doom-themes-load 'doom-vibrant)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Configure treemacs styling
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)

  :init
  (defun my/doom-themes-load (theme)
    (interactive (list (completing-read "Choose theme: "
                                        (my/doom-themes-list))))

    ;; disable all themes
    (dolist (theme custom-enabled-themes)
      (when (not (string= theme "use-package"))
        (disable-theme theme)))

    ;; load doom theme
    (load-theme (if (string= (type-of theme) "string") (intern theme) theme) t)

    ;; load overrides theme: ../../themes/my-doom-themes-overrides-theme.el
    (load-theme 'my-doom-themes-overrides t)

    ;; execute custom function after loading/switching theme
    (with-eval-after-load 'highlight-indent-guides
      (highlight-indent-guides-auto-set-faces)))

  (defun my/doom-themes-vibrant-theme ()
    (interactive)
    (my/doom-themes-load 'doom-vibrant))

  (defun my/doom-themes-nord-light-theme ()
    (interactive)
    (my/doom-themes-load 'doom-nord-light))

  (defun my/doom-themes-list ()
    (seq-filter
     (lambda (n) (string-prefix-p "doom-" (symbol-name n)))
     (custom-available-themes))))

(provide 'my-themes)
;;; my-themes.el ends here
