;; tell the emacs to add the path of .emacs.d, then we could
;; initial/install required packages first.
(add-to-list 'load-path "~/.emacs.d")

;; package.el initialized first
(setq package-enable-at-startup nil)
(package-initialize)

;; Uses el-get to do some magic intialization works!
(require 'init)

;; Customize the ac module
(require 'init-ac)

;; Load the protobuf mode
(require 'init-protobuf)

;; Load the customization of go mode
(require 'init-go)

;; Load the customization of python mode
(require 'init-py)

;; Load the customization of C/C++ mode
(require 'init-clang)

;; Load the info configuration to support extra info files.
(require 'init-vc)

;; Auto load the ido mode itself
(require 'ido)
(ido-mode t)

;; Disable auto-back-files
(setq make-backup-files nil)

;; Disable auto-save-files
(setq auto-save-default nil)

;; Stop emacs's backup changing the file's creation date of the
;; original file.
(setq backup-by-copying t)

;; Allow dired to copy or delete the whole directory.
;; 'always' means no asking. 'top' means ask once. Any other symbol
;; means ask each and every time for a dir ans subdir.
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Copy from one dir to the next dir shown in a split window.
(setq dired-dwim-target t)

;; Let the 'enter' key and '^' key open the new directory in the same
; buffer.
;; (add-hook 'dired-mode-hook
;;  (lambda ()
;;   (define-key dired-mode-map (kbd "<return>")
;;     'dired-find-alternate-file) ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda () (interactive) (find-alternate-file "..")))
;;   ; was dired-up-directory
;;  ))

;; ================== Display Setting =================
;; Display the row number default.
;; The following setting is conflict with doc-view
(defun enable_linum_mode() 
  (if (not (eq major-mode 'doc-view-mode))
	  (linum-mode t)
	(progn
	  (linum-mode -1)
	  (blink-cursor-mode -1))))
(add-hook 'after-change-major-mode-hook 'enable_linum_mode)

;; Display the column number by default.
(column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango)))
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "markdown")
 '(markdown-open-command "markdown")
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 120 :width normal))))
 '(cursor ((t (:background "dark gray")))))
									  
;; Color the shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (lambda() (setq word-wrap t)))
(put 'narrow-to-region 'disabled nil)

(when (eq system-type 'linux)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" 
						:slant normal :weight normal :height 98 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "markdown")
 '(markdown-open-command "markdown"))
)

;; Customize the special key bindings for MacOS
(when (eq system-type 'darwin)			; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))
