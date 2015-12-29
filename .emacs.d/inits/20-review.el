(autoload 'review-mode "review-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.re_?\\(\\.erb\\)?$" . review-mode))

(custom-set-faces
 '(review-mode-bold-face ((t (:foreground "deep sky blue" :weight bold))))
 '(review-mode-header1-face ((t (:foreground "chartreuse" :weight bold))))
 '(review-mode-header2-face ((t (:foreground "lawn green" :weight bold))))
 '(review-mode-header3-face ((t (:foreground "green" :weight bold))))
 '(review-mode-header4-face ((t (:foreground "#0dd" :weight bold))))
 '(review-mode-italic-face ((t (:foreground "red" :slant italic :weight bold))))
 '(review-mode-title-face ((t (:foreground "cyan" :weight bold))))
 '(review-mode-underline-face ((t (:foreground "cyan" :underline t)))))
