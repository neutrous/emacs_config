;; Stable magit version
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Master branch (Developing branch)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-load-list '(all))
(unless (package-installed-p 'magit)
  (package-install 'magit))
(package-initialize)

;; Manually install magit if not so.
;; M-x package-install RET magit RET
(global-set-key (kbd "C-x C-z") 'magit-status)

;; Modify the info directory to support extra info files.
(require 'info)
(setq Info-directory-list
	  (cons (expand-file-name "~/.emacs.d/info")
			Info-directory-list))

(provide 'init-vc)
