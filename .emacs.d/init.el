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
		escreen							; screen for emacs, C-\ C-h
		go-mode							; if you're into go...
		auto-complete					; complete as you type with overlays
		zencoding-mode					; http://www.emacswiki.org/emacs/ZenCoding
		
		;; If emacs launching becomes slower than before, just comment
		;; the fllowing loading snippets.
		(:name yasnippet				; code templates
			   :after (mapc 'yas/load-directory yas/root-directory))
		
		(:name buffer-move				; have to add your own keys.
			   :after (progn
						(global-set-key (kbd "<C-S-up>")     
										'buf-move-up)
						(global-set-key (kbd "<C-S-down>")   
										'buf-move-down)
						(global-set-key (kbd "<C-S-left>")   
										'buf-move-left)
						(global-set-key (kbd "<C-S-right>")  
										'buf-move-right)))
		
		(:name switch-window
			   :after (progn
						(global-set-key (kbd "C-x o")
										'switch-window)))
		
		;; (:name evil
		;;        :after (progn
		;; 		'(evil-mode t)))
				
		(:name goto-last-change			; move pointer back to last
										; change
			   :after (progn
						(global-set-key (kbd "C-x C-/") 
										'goto-last-change)))
		
		(:name go-errcheck
			   :website "https://github.com/dominikh/go-errcheck.el"
			   :description "go-errcheck provides an easy way to
	       invoke errcheck from within Emacs."
			   :type git
			   :url "https://github.com/dominikh/go-errcheck.el.git")
		
	    (:name go-eldoc
			   :website "https://github.com/syohex/emacs-go-eldoc"
			   :description "go-eldoc.el provides eldoc for go
			   language."
			   :type git
			   :url "https://github.com/syohex/emacs-go-eldoc.git")
		
		(:name fill-column-indicator	; Draw a fill indicator line.
			   :after (progn
						(setq-default fci-rule-column 80)
						(setq fci-handle-truncate-lines nil)
						(define-globalized-minor-mode
						  global-fci-mode fci-mode (lambda()
													 (fci-mode 1)))
						(global-fci-mode t)
						(defun auto-fci-mode (&optional unused)
						  (if (not (eq  major-mode 'doc-view-mode))
							  (if (> (window-width) fci-rule-column)
								  (fci-mode 1)
								(fci-mode 0))
							(fci-mode 0)
							))
						(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
						(add-hook 'window-configuration-change-hook 'auto-fci-mode)))
		
		(:name emacs-deffered			; epc dependencies
			   :website "https://github.com/kiwanami/emacs-deferred"
			   :description "provides facilities to manage asynchronous task."
			   :type git
			   :url "https://github.com/kiwanami/emacs-deferred.git")
		
		(:name ctable 					; epc dependencies
			   :website "https://github.com/kiwanami/emacs-ctable"
			   :description "table content for emacs lisp"
			   :type git
			   :url "https://github.com/kiwanami/emacs-ctable.git")
		
		(:name epc 						; the emacs RPC
			   :website "https://github.com/kiwanami/emacs-epc"
			   :description "The Emacs RPC"
			   :type git
			   :url "https://github.com/kiwanami/emacs-epc.git")

		(:name jedi 					; python completion for emacs
			   :website "https://github.com/tkf/emacs-jedi"
			   :description "Python auto-completion for Emacs"
			   :type git
			   :url "https://github.com/tkf/emacs-jedi.git")
		
		(:name auto-complete-clang		; C/C++ auto complete using clang engine.
			   :website "https://github.com/brianjcj/auto-complate-clang"
			   :description "The AC sources for Clang."
			   :type git
			   :url "https://github.com/brianjcj/auto-complete-clang.git")
		))

;; (unless (string-match "apple-darwin" system-configuration)
;;   (loop for p in '(color-theme			; nice looking emacs
;; 		   color-theme-tango	; check out color-theme-solarized
;; 		   )
;; 	do (add-to-list 'el-get-sources p)))

;; Specified the packages which want to synchronized.  I don't why
;; when i added the following, the previous set would install.
(setq packages
      (append
       '(el-get evil markdown-mode switch-window escreen)
       (mapcar 'el-get-source-name el-get-sources)))

;; install new packages and init already installed packages
(el-get 'sync packages)

(evil-mode t)

(provide 'init)
