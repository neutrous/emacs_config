;; tell the where the add the el-get package.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Check whether the el-get package is installed, or else
;; install it.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by
;; the el-get installer.
(setq el-get-sources		    
      '(el-get							; el-get is self-hosting
		escreen								; screen for emacs, C-\ C-h
		go-mode								; if you're into go...
		switch-window						; takes over C-x o
		auto-complete						; complete as you type with overlays
		zencoding-mode						; http://www.emacswiki.org/emacs/ZenCoding
		(:name buffer-move					; have to add your own keys.
			   :after (progn
						(global-set-key (kbd "<C-S-up>")     'buf-move-up)
						(global-set-key (kbd "<C-S-down>")   'buf-move-down)
						(global-set-key (kbd "<C-S-left>")   'buf-move-left)
						(global-set-key (kbd "<C-S-right>")  'buf-move-right)))
		
		;; (:name evil
		;;        :after (progn
		;; 		'(evil-mode t)))
		
		;; (:name magit							; git meet emacs, and a binding
		;;        :after (progn
		;; 		(global-set-key (kbd "C-x C-z") 'magit-status)))
		
		(:name goto-last-change				; move pointer back to last change
			   :after (progn
						(global-set-key (kbd "C-x C-/") 'goto-last-change)))
		
		(:name go-errcheck
			   :website "https://github.com/dominikh/go-errcheck.el"
			   :description "go-errcheck provides an easy way to
	       invoke errcheck from within Emacs."
			   :type git
			   :url "https://github.com/dominikh/go-errcheck.el.git")
	  
	    (:name go-eldoc
			   :website "https://github.com/syohex/emacs-go-eldoc"
			   :description "go-eldoc.el provides eldoc for go language."
			   :type git
			   :url "https://github.com/syohex/emacs-go-eldoc.git")
	  ))

;; (unless (string-match "apple-darwin" system-configuration)
;;   (loop for p in '(color-theme			; nice looking emacs
;; 		   color-theme-tango	; check out color-theme-solarized
;; 		   )
;; 	do (add-to-list 'el-get-sources p)))

;; Specified the packages which want to synchronized.
;; I don't why when i added the following, the previous set would install.
(setq packages
      (append
       '(el-get evil markdown-mode switch-window escreen)
       (mapcar 'el-get-source-name el-get-sources)))

;; install new packages and init already installed packages
(el-get 'sync packages)

(evil-mode t)

(provide 'init)
