;; add load path 
(add-to-list 'load-path "~/.emacs.d/")

;; load the protobuf mode
(require 'protobuf-mode)

;; add the extension support
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(provide 'init-protobuf)
