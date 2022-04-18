;;; early-init.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; The file before the file that starts it all.

;;; Code:

;; Native-Comp
(setq
 native-comp-speed 5
 comp-speed 5
 native-comp-async-report-warnings-errors nil
 comp-async-report-warnings-errors nil
 native-comp-async-query-on-exit t
 comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

(when (or (boundp 'comp-eln-load-path) (boundp 'native-comp-eln-load-path))
  (let ((eln-cache-dir (expand-file-name "var/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))

    (if (boundp 'native-comp-eln-load-path)
	(setcar native-comp-eln-load-path eln-cache-dir))

    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete"))))

;; Temporarily set the GC threshold to improve startup performance.
;; This will be reset by the GCMH package after it loads.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable Emacs 27's automatic package.el initialization before the init.el
;; file is loaded. I use straight.el instead of package.el.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.

(setq-default
 ;; setting these three values to `#t' may improve startup time, but it mans
 ;; that emacs will appear to hang unil all packages and system packages are
 ;; loaded.
 frame-inhibit-implied-resize nil
 inhibit-message nil
 inhibit-redisplay nil
 
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-message t
 inhibit-startup-screen t
 initial-scratch-message nil
 )

(defun my/window-setup-hook ()
  "Restore some values set during early-init.el."
  (interactive)
  (setq-default
   frame-inhibit-implied-resize nil
   inhibit-message nil
   inhibit-redisplay nil)
  (redisplay))

(add-hook 'window-setup-hook #'my/window-setup-hook)

;;; Early-init.el ends here
