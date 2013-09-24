;; Configuration for python editings.
(setq py-install-directory "~/.emacs.d/python-mode-6.1.2")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

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
