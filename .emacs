;; tell the emacs to add the path of .emacs.d, then we could
;; initial/install required packages first.
(add-to-list 'load-path "~/.emacs.d")

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
(global-linum-mode t)

;; Display the column number by default.
(column-number-mode t)

(when (string-match "linux" system-configuration)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "markdown")
 '(markdown-open-command "markdown"))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "markdown")
 '(markdown-open-command "markdown")
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
									  
;; Color the shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (lambda() (setq word-wrap t)))
