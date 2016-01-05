(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cu" 'comment-region)
(global-set-key "\C-cy" 'uncomment-region)
(global-set-key (kbd "C-c r") 'revert-buffer)
;; ウィンドウ逆移動
(global-set-key "\C-xp" (lambda()(interactive)(other-window -1)))
;; 暴発するので無効化
(global-unset-key (kbd "C-x C-p"))
