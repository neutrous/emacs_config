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
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; The variable is only taking effect under the version 24.1 or later.
(custom-set-variables
 '(org-catch-invisible-edits (quote show)))

;; When TODO items have been done, then the following settings would
;; cause the timestamp to be made.
(setq org-log-done 'time)
(setq org-log-done-with-time t)
;; add another note message?
;; (setq org-log-done 'note)

(provide 'init-org)

