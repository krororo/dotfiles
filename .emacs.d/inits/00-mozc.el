;; mozc
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
(require 'mozc)
(setq mozc-candidate-style 'echo-area)
(set-cursor-color "red")
(set-language-environment 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-input-method "japanese-mozc")
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
; 変換キーでon
(global-set-key (kbd "<henkan>")
                (lambda () (interactive)
                  (when (null current-input-method) (toggle-input-method))))
; 無変換キーでon
(global-set-key (kbd "<muhenkan>")
                (lambda () (interactive)
                  (inactivate-input-method)))
; 全角半角キーと無変換キーのキーイベントを横取りする
(defadvice mozc-handle-event (around intercept-keys (event))
  "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
  (if (member event (list 'zenkaku-hankaku 'muhenkan))
      (progn
        (mozc-clean-up-session)
        (toggle-input-method))
    (progn ;(message "%s" event) ;debug
      ad-do-it)))
(ad-activate 'mozc-handle-event)
(add-hook 'input-method-activate-hook
          '(lambda () (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
          '(lambda () (set-cursor-color "red")))
