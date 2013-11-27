;; Set the repo base of org-mode
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Configuration for org-mode
(unless package-archive-contents		; refresh the packages descriptions
  (package-refresh-contents))

(setq package-load-list '(all))			; list of packages to load
(unless (package-installed-p 'org)		; make sure the Org package is
  (package-install 'org))				; installed, install it if not
;; (unless 
;; 	(package-installed-p 'org-plus-contrib)
;;   (package-install 'org-plus-contrib))
(package-initialize)					; Initialize & Install Package

;; Do some extra settings.

(provide 'init-org)
