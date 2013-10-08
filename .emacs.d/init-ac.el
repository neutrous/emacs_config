;; load auto complete module.
(require 'auto-complete)
(require 'auto-complete-config)

;; The most of following things were directly copied from chenbin's
;; configuration(github.com/redguardtoo/emacs.d/init-auto-complete.el). Thanks
;; for his introducation.
(setq ac-expand-on-auto-complete nil)
(setq ac-auto-start nil)
(setq ac-dwim nil)			; To get pop-ups with docs
							; even if a word is uniqueley
					        ; completed.
(ac-set-trigger-key "TAB")		; AFTER input prefix, press
								; TAB key ASAP

;; extra modes auto-complete must support 
(dolist (mode '(magit-log-edit-mode org-mode text-mode
		html-mode emacs-lisp-mode markdown-mode
		go-mode python-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(ac-config-default)

(provide 'init-ac)
