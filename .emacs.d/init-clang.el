;; Configurations for C/C++ Autocomplete.
(require 'auto-complete-clang)

(defun my-ac-cc-mode-setup()
  (progn
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet
											 ac-source-abbrev
											 ac-source-dictionary
											 ac-source-words-in-same-mode-buffers)
						   ac-sources))
(local-set-key (kbd "<C-tab>") 'ac-complete-clang))
(yas-minor-mode))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
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
