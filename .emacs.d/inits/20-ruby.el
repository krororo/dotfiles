(setq auto-mode-alist
      (append '(("\\.\\(ru\\|rake\\|plugin\\|gemspec\\)\\'" . ruby-mode)
                ("Rakefile\\'" . ruby-mode)
                ("Gemfile\\'" . ruby-mode))
         auto-mode-alist))
(setq ruby-use-smie nil)
(add-hook 'ruby-mode-hook
          '(lambda()
             (define-key ruby-mode-map (kbd "C-c d") 'flymake-display-err-minibuf)
             (electric-indent-local-mode -1)))

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

(add-hook 'ruby-mode-hook
          '(lambda()
             ;; flymake setting
             (flymake-ruby-load)
             ;; yard-mode
             (yard-mode)
             (highlight-symbol-mode)))
(custom-set-variables
 '(rspec-spec-command "rspec -c")
 '(rspec-use-rake-when-possible nil)
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren-style nil)
 '(ruby-insert-encoding-magic-comment nil))
