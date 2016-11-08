(setq web-mode-attr-indent-offset 4)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-script-padding 2)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vm\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "javascript")
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-mode))))
