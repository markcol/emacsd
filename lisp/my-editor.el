;;; my-editor.el --- basic editor settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; but maintain correct appearance
(setq-default tab-width 8)

(setq line-number-display-limit-width 10000)

;; Make GNUTLS a bit safer.
(setq gnutls-min-prime-bits 4096)

;; Newline at end of file
(setq require-final-newline t)

(setq echo-keystrokes 0.4)

(setq large-file-warning-threshold (* 25 1024 1024))

;; It's much easier to move around lines based on how they are
;; displayed, rather than the actual line. This helps a ton with long
;; log file lines that may be wrapped:
(setq line-move-visual t)

;; Separate sentences with a single space instead of two.
(setq sentence-end-double-space nil)

(setq default-directory "~")
(setq command-line-default-directory "~")

;; Fix some weird color escape sequences.
(setq system-uses-terminfo nil)

;; Files must end with a newline
(setq require-final-newline t)

;; Sentances end with a period and single space.
(setq sentence-end-double-space nil)

;; Be quiet about reverting files
(setq auto-revert-verbose nil)

(setq whitespace-line-column 80)

;; Backup files
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 0
      vc-make-backup-files t
      version-control t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backup" my/cache-dir))))

;; Auto-save files
(let ((auto-save-dir (expand-file-name "autosave/" my/cache-dir)))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir))
  (setq auto-save-interval 20
        auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(when (eq system-type 'darwin)
  ;; File notifications seem unreliable on macOS.
  (setq auto-revert-use-notify nil))

;; Automatically revert file if it's changed on disk.
(global-auto-revert-mode 1)

;; Visual Line mode - wrap lines
(visual-line-mode t)

;; Show matching parentheses
(show-paren-mode t)

;; Delete the selection with a keypress
(delete-selection-mode t)

(transient-mark-mode t)

(line-number-mode t)

(column-number-mode t)

;; Save place in files
(setq save-place-file (expand-file-name "saveplace" my/cache-dir))
(save-place-mode 1)

(setq tls-program
      ;; Defaults:
      ;; '("gnutls-cli --insecure -p %p %h"
      ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
      ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
      '("gnutls-cli -p %p %h"
        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  ;; brew install coreutils
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))
  
  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)
  )

;; Electric behavior
;; (electric-layout-mode t)

;; Cursor
(if window-system
  (progn
      (blink-cursor-mode t)
      (setq initial-frame-alist
            (cons '(cursor-type . bar) (copy-alist initial-frame-alist)))
      (setq default-frame-alist
            (cons '(cursor-type . bar) (copy-alist default-frame-alist))))
      (blink-cursor-mode -1))

;; Enable mouse support when running in a console
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  ;; (global-set-key [mouse-4] 'scroll-down-line)
  ;; (global-set-key [mouse-5] 'scroll-up-line)
  )

;; Customize Whitespace Characters
;;  - Newline: \u00AC = ¬
;;  - Tab:     \u2192 = →
;;             \u00BB = »
;;             \u25B6 = ▶
(setq whitespace-display-mappings
      (quote ((newline-mark ?\n [?\u00AC ?\n] [?$ ?\n])
              (tab-mark     ?\t [?\u2192 ?\t] [?\u00BB ?\t] [?\\ ?\t]))))

(setq whitespace-style
      (quote (face tabs trailing space-before-tab newline
                   indentation space-after-tab tab-mark newline-mark
                   empty)))

;; automatically indenting yanked text if in programming-modes
(defun my/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) my/yank-indent-threshold)
      (indent-region beg end nil)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `my/yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode my/indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode my/yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (my/yank-advised-indent-function (region-beginning) (region-end)))))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'my-editor)
;;; my-editor.el ends here
