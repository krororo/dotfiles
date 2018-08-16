;; kotlin-mode
(setq kotlin-tab-width 4)
(setq auto-mode-alist
      (append '(("\\.kts\\'" . kotlin-mode))
         auto-mode-alist))
