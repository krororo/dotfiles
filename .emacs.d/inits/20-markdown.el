;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(require 'org-table)
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))
(add-hook 'markdown-mode-hook
          #'(lambda()
              (add-hook 'before-save-hook 'cleanup-org-tables  nil 'make-it-local)))
