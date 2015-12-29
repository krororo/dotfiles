;; mozc
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
(require 'mozc)
(setq mozc-candidate-style 'echo-area)
(set-cursor-color "red")
(set-language-environment 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-input-method "japanese-mozc")
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(add-hook 'mozc-mode-hook
          (lambda()
            (define-key mozc-mode-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)))
(add-hook 'input-method-activate-hook
          '(lambda () (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
          '(lambda () (set-cursor-color "red")))
