(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(editorconfig-exclude-modes (quote (web-mode)))
 '(flycheck-ruby-rubocop-executable nil)
 '(initial-buffer-choice "~/memo.md")
 '(magit-diff-highlight-hunk-body nil)
 '(magit-log-margin
   (quote
    (t "%Y-%m-%d %H:%M:%S " magit-log-margin-width t 18)))
 '(magit-section-initial-visibility-alist (quote ((unpushed . show) (stashes . hide))))
 '(package-archives (quote (("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (el-get leaf-keywords leaf vue-mode magit-popup editorconfig request let-alist kotlin-mode)))
 '(pug-tab-width 2)
 '(safe-local-variable-values
   (quote
    ((eval setq-local flycheck-command-wrapper-function
           (lambda
             (command)
             (append
              (quote
               ("bundle" "exec"))
              command))))))
 '(scroll-conservatively 1)
 '(scroll-margin 5)
 '(scroll-preserve-screen-position nil)
 '(sh-indentation 4 t)
 '(typescript-indent-level 2)
 '(vue-html-extra-indent 2)
 '(web-mode-attr-indent-offset 4)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-script-padding 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#003300" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "PfEd" :family "HackGen"))))
 '(eruby-standard-face ((t (:background "gray" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "red"))))
 '(font-lock-keyword-face ((t (:foreground "magenta"))))
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold))))
 '(magit-diff-added ((t (:foreground "green"))))
 '(magit-diff-added-highlight ((t (:foreground "green"))))
 '(magit-diff-file-header ((t (:foreground "yellow"))))
 '(magit-diff-removed ((t (:foreground "red"))))
 '(magit-diff-removed-highlight ((t (:foreground "red"))))
 '(magit-hash ((t (:foreground "gold"))))
 '(magit-item-highlight ((t (:background "gray5"))))
 '(markdown-code-face ((t (:inherit default :foreground "medium aquamarine"))))
 '(mmm-default-submode-face ((t nil)))
 '(review-mode-bold-face ((t (:foreground "deep sky blue" :weight bold))))
 '(review-mode-header1-face ((t (:foreground "chartreuse" :weight bold))))
 '(review-mode-header2-face ((t (:foreground "lawn green" :weight bold))))
 '(review-mode-header3-face ((t (:foreground "green" :weight bold))))
 '(review-mode-header4-face ((t (:foreground "#0dd" :weight bold))))
 '(review-mode-italic-face ((t (:foreground "red" :slant italic :weight bold))))
 '(review-mode-title-face ((t (:foreground "cyan" :weight bold))))
 '(review-mode-underline-face ((t (:foreground "cyan" :underline t)))))
