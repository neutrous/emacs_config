;; Configuration for python editings.
(add-to-list 'load-path "~/.emacs.d/python-mode-6.1.2")
(require 'python-mode)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
								   interpreter-mode-alist))
(autoload 'python-mode "python mode" "Python editing mode." t)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;; set the color of the argument
(set-face-attribute 'jedi:highlight-function-argument nil
					:underline t :foreground "green"
					:weight 'bold)


;; TODO: add the following region into python-mode hook
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pyflakes configurations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'find-file-hooks 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
					   'flymake-create-temp-inplace))
		   (local-file (file-relative-name temp-file
		   (file-name-directory buffer-file-name))))
	  (list "pycheckers" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
			 '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; please cp/ln $PWD/pychekers into the place where the command could
;; be found.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPython configurations...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use IPython, so on the platform it should be installed first.
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
;; use the ex backend, for both mayavi and matplotlib
(setq py-python-command-args
	  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the intepreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p nil)

;; split windows
(setq py-split-windows-on-execute-p t)

;; try to automatically figure out indentation
(setq py-smart-indentation t)

(provide 'init-py)
