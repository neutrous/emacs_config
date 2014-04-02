;; Using the enhancement js2 mode.
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'js-comint)

(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))
		
(provide 'init-js)
