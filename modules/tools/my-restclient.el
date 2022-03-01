;;; my-restclient.el --- restclient configuration.

;;; Commentary:

;; Basic configuration for restclient.

;;; Code:

(require 'my-json)
(require 'my-web-mode)
(require 'my-xml)
(require 'my-yaml)

(use-package restclient
  :mode
  ("\\.restclient\\'" . restclient-mode)
  ("\\.rest\\'" . restclient-mode)

  :custom
  (restclient-content-type-modes
   '(("application/json" . json-mode)
     ("application/x-yaml" . yaml-mode)
     ("application/xml" . nxml-mode)
     ("application/yaml" . yaml-mode)
     ("image/gif" . image-mode)
     ("image/jpeg" . image-mode)
     ("image/jpg" . image-mode)
     ("image/png" . image-mode)
     ("text/html" . web-mode)
     ("text/plain" . text-mode)
     ("text/xml" . nxml-mode)
     ("text/yaml" . yaml-mode))))

(use-package restclient-helm
  :after restclient)

(provide 'my-restclient)
;;; my-restclient.el ends here
