(setq flycheck-check-syntax-automatically '(mode-enabled save))
(require 'flycheck)

;; keybind
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
