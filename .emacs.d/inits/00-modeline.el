(defvar my-mode-line-format)
(setq my-mode-line-format "%d")
(if size-indication-mode
    (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
(cond ((and (eq line-number-mode t) (eq column-number-mode t))
       (setq my-mode-line-format (concat "(%%l,%%c) " my-mode-line-format)))
      ((eq line-number-mode t)
       (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
      ((eq column-number-mode t)
       (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

(setq mode-line-position
      '(:eval (format my-mode-line-format
                      (+ (count-lines (point-max) (point-min)) 1))))
