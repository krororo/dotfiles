(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
          '(lambda()
             (setq js2-global-externs (list "jQuery" "$"))
             (setq js2-additional-externs (list "jQuery" "$"))
             (setq js2-include-browser-externs nil)
             (setq js2-mode-show-parse-errors nil)
             (setq js2-mode-show-strict-warnings nil)
             (setq js2-highlight-external-variables nil)
             (setq js2-include-jslint-globals nil)
             (dumb-jump-mode)
             (flycheck-mode)))

(setq js-indent-level 2)
(setq js2-basic-offset 2)
