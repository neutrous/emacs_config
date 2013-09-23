;; load go autocomplete module
(require 'go-autocomplete)

;; shortcut for go-remove-unused-imports command
(add-hook 'go-mode-hook (lambda()
						  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;; shortcut for go-goto-imports command
(add-hook 'go-mode-hook (lambda()
						  (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; load the go errcheck module. This module depends on the repo on
;; https://github.com/kisielk/errcheck.git using the command "go get
;; github.com/kisiekl/errcheck" to download it first, if not so.

;; The source files is at https://github.com/dominikh/go-errcheck.el.git
(require 'go-errcheck)

;; shortcut for go-errcheck command
(add-hook 'go-mode-hook (lambda()
						  (local-set-key (kbd "C-c c") 'go-errcheck)))

;; setp the go eldoc
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; customize current argument index face
(set-face-attribute 'eldoc-highlight-function-argument nil
					:underline t :foreground "green"
					:weight 'bold)

(provide 'init-go)
