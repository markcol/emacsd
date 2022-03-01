;;; my-flutter.el --- flutter-mode configuration.

;;; Commentary:

;; Basic configuration for flutter-mode.

;;; Code:

(require 'my-dart)

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

(provide 'my-flutter)
;;; my-flutter.el ends here
