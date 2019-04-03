(add-hook 'typescript-mode-hook
          '(lambda ()
             (unless (string-match "/node_modules/" (or (buffer-file-name) ""))
               (flycheck-mode))))
