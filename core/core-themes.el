;;; core-themes.el --- Custom themes.

;;; Commentary:

;; Enable loading custom themes.

;;; Code:

(defvar my/themes-dir (expand-file-name "themes" user-emacs-directory)
  "Root directory for Emacs Siren custom themes.")

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

(provide 'core-themes)
;;; core-themes.el ends here
