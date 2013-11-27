;; Modify the info directory to support extra info files.
(require 'info)
(setq Info-directory-list
	  (cons (expand-file-name "~/.emacs.d/info")
			Info-directory-list))

(provide 'init-vc)
