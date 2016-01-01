(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vm\\'" . web-mode))

(custom-set-variables
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2))
