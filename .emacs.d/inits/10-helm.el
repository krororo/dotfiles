(require 'helm-config)
(helm-mode 1)
(helm-descbinds-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "C-c C-v") 'helm-ls-git-ls)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-;") 'helm-mini)

;; リストのソートをしないように
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))

(custom-set-faces
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold)))))

(custom-set-variables
 '(helm-buffer-max-length 40)
 '(helm-buffer-details-flag nil))
