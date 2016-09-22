(setq auto-mode-alist
      (append '(("\\.\\(ru\\|rake\\|plugin\\|gemspec\\)\\'" . ruby-mode)
                ("Rakefile\\'" . ruby-mode)
                ("Gemfile\\'" . ruby-mode))
         auto-mode-alist))
(setq ruby-use-smie nil)
(setq rspec-spec-command "rspec -c")
(setq rspec-use-rake-when-possible nil)
(setq ruby-deep-arglist nil)
(setq ruby-deep-indent-paren-style nil)
(setq ruby-insert-encoding-magic-comment nil))

(add-hook 'ruby-mode-hook
          '(lambda()
             (setq flycheck-checker 'ruby-rubocop)
             (electric-indent-local-mode -1)
             (flycheck-mode)
             (yard-mode)
             (dumb-jump-mode)))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
