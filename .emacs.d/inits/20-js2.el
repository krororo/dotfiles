(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(custom-set-variables
 '(js-indent-level 2)
 '(js2-basic-offset 2))
