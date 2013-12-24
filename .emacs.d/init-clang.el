;; Configurations for C/C++ Autocomplete.
(require 'auto-complete-clang)
(require 'google-c-style)

(defun my-ac-cc-mode-setup()
  (progn
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet
											 ac-source-abbrev
											 ac-source-dictionary
											 ac-source-words-in-same-mode-buffers)
						   ac-sources))
  (local-set-key (kbd "<C-tab>") 'ac-complete-clang)
  (yas-minor-mode-on)
  (flymake-mode -1))
  (setq indent-tabs-mode nil)
  (append '(semantic-default-submodes global-semanticdb-minor-mode
									  global-semantic-mru-bookmark-mode
									  global-semantic-highlight-func-mode
									  global-semanticdb-minor-mode))
  (semantic-mode 1))

(require 'semantic/ia)

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)

;; Standard include directories setting
;; TODO: this is my laptop os paths.
(setq ac-clang-flags
	  (mapcar (lambda (item)(concat "-I" item))
			  (split-string
			   "
/usr/lib/gcc/x86_64-redhat-linux/4.7.2/../../../../include/c++/4.7.2
/usr/lib/gcc/x86_64-redhat-linux/4.7.2/../../../../include/c++/4.7.2/x86_64-redhat-linux
/usr/lib/gcc/x86_64-redhat-linux/4.7.2/../../../../include/c++/4.7.2/backward
/usr/lib/gcc/x86_64-redhat-linux/4.7.2/include
/usr/local/include
/usr/include
"
)))

;; add cmake-support
(require 'cmake-mode)
(setq auto-mode-alist
	  (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
			  auto-mode-alist))

(provide 'init-clang)
