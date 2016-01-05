(global-anzu-mode +1)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))

(global-set-key (kbd "C-c q") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c Q") 'anzu-query-replace-at-cursor-thing)
