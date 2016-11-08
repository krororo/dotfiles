(global-anzu-mode +1)

(setq anzu-mode-lighter "")
(setq anzu-deactivate-region t)
(setq anzu-search-threshold 1000)

(global-set-key (kbd "C-c q") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c Q") 'anzu-query-replace-at-cursor-thing)
