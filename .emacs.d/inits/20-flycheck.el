(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; keybind
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
