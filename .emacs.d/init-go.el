;; go-mode.el is shiped with the go source archive.
(require 'go-mode-load)

;; the configuration depends on the godef tool, please using the
;; following command to download it.
;; go get code.google.com/p/rog-go/hg/exp/cmd/godef

;; using `go get -u github.com/nsf/gocode' to get `gocode' tool first,
;; then copy gocode/emacs/go-autocomplete.el to ~/.emacs.d.
;; load go autocomplete module		  
(require 'go-autocomplete)

;; shortcut for go-remove-unused-imports command
(add-hook 'go-mode-hook (lambda()
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;; shortcut for go-goto-imports command
(add-hook 'go-mode-hook (lambda()
			  (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; configuring goflymake for more information, please refer to
;; `https://github.com/dougm/goflymake'
(require 'go-flymake)
(require 'go-flycheck)

;; load the go errcheck module. This module depends on the repo on
;; https://github.com/kisielk/errcheck.git using the command "go get
;; github.com/kisiekl/errcheck" to download it first, if not so.

;; The source files is at https://github.com/dominikh/go-errcheck.el.git
(require 'go-errcheck)

;; shortcut for go-errcheck command
(add-hook 'go-mode-hook (lambda()
			  (local-set-key (kbd "C-c c") 'go-errcheck)))

;; go-eldoc could provide a nice look of the go vairable description.
;; its locaion is: https://github.com/syohex/emacs-go-eldoc
;; setp the go eldoc
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; customize current argument index face
(set-face-attribute 'eldoc-highlight-function-argument nil
		    :underline t :foreground "green"
		    :weight 'bold)

;; auto re-format the code before saving the files.
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-go)
