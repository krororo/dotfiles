(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-hook 'groovy-mode-hook
          '(lambda()
             (setq c-basic-offset 4)))
