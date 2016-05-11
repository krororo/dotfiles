(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
          '(lambda()
             (setq js2-global-externs (list "jQuery" "$"))
             (setq js2-additional-externs (list "jQuery" "$"))))

(custom-set-variables
 '(js-indent-level 2)
 '(js2-basic-offset 2))
