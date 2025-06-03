(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

(prog1 "leaf"
  (prog1 "install leaf"
    (custom-set-variables
     '(package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
     '(package-archive-priorities '(("melpa" . 5)
                                    ("gnu" . 3)
                                    ("jcs-elpa" . 0))))
    (package-initialize)
    (use-package leaf :ensure t))

  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf xdg
  :require t
  :config
  (defvar my-emacs-cache-home (expand-file-name "emacs" (xdg-cache-home)))
  (defvar my-emacs-data-home (expand-file-name "emacs" (xdg-data-home))))

(leaf exec-path-from-shell
  :ensure t
  :custom ((exec-path-from-shell-warn-duration-millis . 2000)
           (exec-path-from-shell-shell-name . "/bin/zsh")
           (exec-path-from-shell-variables . '("PATH" "NODE_EXTRA_CA_CERTS")))
  :config
  (exec-path-from-shell-initialize))

(leaf package-utils :ensure t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(expand-file-name "custom.el" my-emacs-data-home))))

(leaf *initialize-emacs
  :custom ((scroll-conservatively . 1)
           (scroll-margin . 5)
           (scroll-preserve-screen-position . nil)
           (create-lockfiles . nil)
           (indicate-buffer-boundaries . 'left)
           (use-default-font-for-symbols . nil)
           (use-short-answers . t)
           (inhibit-startup-message . t) ;; 起動時のメッセージを非表示
           (visible-bell . t)
           (ring-bell-function . 'ignore)
           (safe-local-variable-values . '((encoding . utf-8)))
           (split-height-threshold . nil))
  :custom-face
  (default . '((t (:background "#003300" :foreground "white" :height 140 :foundry "PfEd" :family "HackGen"))))
  (font-lock-comment-face . '((t (:foreground "gray"))))
  (font-lock-keyword-face . '((t (:foreground "magenta"))))
  (trailing-whitespace . '((t (:background "indian red"))))
  :setq-default
  (indent-tabs-mode . nil)
  :config
  (set-fontset-font nil '(#x1F000 . #x1FAFF) "Noto Color Emoji")

  (set-cursor-color "red")
  (set-language-environment 'utf-8)

  (if window-system (menu-bar-mode 1) (menu-bar-mode -1))
  (set-scroll-bar-mode nil)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(leaf *linux-settings
  :unless (eq system-type 'darwin)
  :hook (window-setup-hook . toggle-frame-maximized))

(leaf *mac-settings
  :if (eq system-type 'darwin)
  :custom
  (default-frame-alist . '((width . 200) (height . 70)))
  (mac-command-modifier . 'meta)
  (mac-option-modifier . 'option)
  :bind ("C-M-¥" . indent-region))

(leaf *keep-scratch-buffer
  :doc "don't remove *scratch* buffer"
  :preface
  (defun my-make-scratch (&optional arg)
    (interactive)
    (progn
      ;; "*scratch*" を作成して buffer-list に放り込む
      (set-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (erase-buffer)
      (when (and initial-scratch-message (not inhibit-startup-message))
        (insert initial-scratch-message))
      (or arg (progn (setq arg 0)
                     (switch-to-buffer "*scratch*")))
      (cond ((= arg 0) (message "*scratch* is cleared up."))
            ((= arg 1) (message "another *scratch* is created")))))
  :hook
  ((kill-buffer-query-functions
    ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
    . (lambda ()
        (if (string= "*scratch*" (buffer-name))
            (progn (my-make-scratch 0) nil)
          t)))
   (after-save-hook
    ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
    . (lambda ()
        (unless (member (get-buffer "*scratch*") (buffer-list))
          (my-make-scratch 1))))))

(leaf *deepl
  :config
  (defun my-trans-deepl (beg end)
    (interactive "r")
    (let ((str (buffer-substring beg end)))
      (browse-url
       (concat "https://www.deepl.com/translator#en/ja/" (url-hexify-string str))))))

(leaf *show-buffer-file-name
  :config
  (defun my-show-buffer-file-name ()
    "Show the full path to the current file in the minibuffer."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (progn
            (message file-name)
            (kill-new file-name))
        (error "Buffer not visiting a file"))))
  (defun my-show-buffer-file-name-relative-vc-root ()
    "Show the relative path of vc root to the current file in the minibuffer."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (let ((root (vc-root-dir)))
            (if root
                (progn
                  (let ((file-rel-name (file-relative-name file-name root)))
                    (message file-rel-name)
                    (kill-new file-rel-name)))
              (progn
                (message file-name)
                (kill-new file-name))))
        (error "Buffer not visiting a file")))))

(leaf project
  :custom `(project-list-file . ,(expand-file-name "projects" my-emacs-data-home)))

(leaf compile
  :custom ((compilation-scroll-output . t)
           (compilation-max-output-line-length . nil)))

(leaf files
  :custom ((auto-save-default . nil)  ;; .#* とかのバックアップファイルを作らない
           (make-backup-files . nil)) ;; *.~ とかのバックアップファイルを作らない
  )

(leaf simple
  :global-minor-mode column-number-mode)

(leaf paren
  :global-minor-mode show-paren-mode)

(leaf tab-bar
  :global-minor-mode t
  :custom ((tab-bar-new-tab-choice . "*dashboard*")))

(leaf tramp
  :defer-config
  (setenv "SHELL" "/bin/bash"))

(leaf whitespace
  :doc "tab に色を付ける"
  :hook ((prog-mode-hook text-mode-hook) . (lambda () (setq show-trailing-whitespace t)))
  :custom
  (whitespace-style . '(face tabs tab-mark))
  (whitespace-display-mappings . '((tab-mark ?\t [?\xBB ?\t])))
  :custom-face
  (whitespace-tab . '((t (:foreground "royal blue" :background unspecified :underline t))))
  :global-minor-mode global-whitespace-mode)

(leaf editorconfig
  :ensure t
  :preface
  (defun my-editorconfig-disable-trim-whitespace-in-read-only-buffers (props)
    (when (and buffer-read-only (gethash 'trim_trailing_whitespace props))
      (remove-hook 'write-file-functions #'delete-trailing-whitespace :local)))

  (defvar my-current-cleanup-state ""
    "delete-trailing-whitespace モードの状態")
  (defun my-toggle-cleanup-spaces ()
    (interactive)
    (cond ((memq 'delete-trailing-whitespace write-file-functions)
           (setq my-current-cleanup-state
                 (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
           (remove-hook 'write-file-functions 'delete-trailing-whitespace))
          (t
           (setq my-current-cleanup-state "")
           (add-hook 'write-file-functions 'delete-trailing-whitespace)))
    (force-mode-line-update))
  :custom ((editorconfig-exclude-modes . '(web-mode)))
  :hook (editorconfig-custom-hooks . my-editorconfig-disable-trim-whitespace-in-read-only-buffers)
  :global-minor-mode t)

(leaf eaw-console
  :vc ( :url "https://github.com/hamano/locale-eaw"
        :lisp-dir "dist")
  :require t)

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf colorful-mode
  :ensure t
  :global-minor-mode t)

(leaf diff-hl
  :ensure t
  :global-minor-mode global-diff-hl-mode
  :custom-face ((diff-hl-change . '((t (:background "purple")))))
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(leaf avy
  :ensure t
  :custom ((avy-timeout-seconds . 1.0))
  :bind (("M-;" . avy-goto-char-timer)))

(leaf mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(leaf rainbow-delimiters
  :ensure t
  :hook prog-mode-hook
  :custom-face
  ((rainbow-delimiters-depth-1-face . '((t (:foreground "#ffa500"))))
   (rainbow-delimiters-depth-2-face . '((t (:foreground "#ff5e5e"))))
   (rainbow-delimiters-depth-3-face . '((t (:foreground "#ffaa77"))))
   (rainbow-delimiters-depth-4-face . '((t (:foreground "#dddd77"))))
   (rainbow-delimiters-depth-5-face . '((t (:foreground "#80ee80"))))
   (rainbow-delimiters-depth-6-face . '((t (:foreground "#66bbff"))))
   (rainbow-delimiters-depth-7-face . '((t (:foreground "#da6bda"))))
   (rainbow-delimiters-depth-8-face . '((t (:foreground "#afafaf"))))
   (rainbow-delimiters-depth-9-face . '((t (:foreground "#f0f0f0"))))
   (rainbow-delimiters-base-error-face . '((t (:foreground "#ff2020"))))))

(leaf beacon
  :ensure t
  :global-minor-mode beacon-mode)

(leaf indent-bars
  :ensure t
  :custom
  (indent-bars-color . '(highlight :face-bg t :blend 0.325))
  ;; The NS build has partial stipple support in Emacs 30.1.
  ;; see: https://github.com/jdtsmith/indent-bars/blob/v0.8.3/README.md#compatibility
  (indent-bars-prefer-character . t)
  (indent-bars-no-stipple-char . ?|)
  :hook yaml-mode-hook haml-mode-hook)

(leaf vundo
  :ensure t
  :bind ("C-x u" . vundo))

(leaf *key-binding
  :preface
  (defun my-reverse-other-window ()
    (interactive)
    (other-window -1))
  :bind (("C-;" . comment-dwim)
         ("C-h" . delete-backward-char)
         ("C-c r" . revert-buffer)
         ("C-x m" . nil) ;; disable compose-mail
         ("M-\"" . insert-pair)
         ("M-'" . insert-pair)
         ("M-/" . xref-find-references))
  :init
  (ffap-bindings))

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf *mode-line
  :config
  (setq-default mode-line-format
                '((:eval my-current-cleanup-state)
                  "%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-position
                  " "
                  ;; mode-line-frame-identification
                  mode-line-buffer-identification
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces))

  (defvar mode-line-cleaner-alist
    '( ;; For minor-mode, first char is 'space'
      (editorconfig-mode . " EC")
      ;; Major modes
      (emacs-lisp-mode . "El")
      (markdown-mode . "Md")
      (ruby-mode   . "Rb")
      (typescript-mode . "Ts")
      (js2-mode . "Js")))

  (defun clean-mode-line ()
    (interactive)
    (cl-loop for (mode . mode-str) in mode-line-cleaner-alist
             do
             (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
               (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
               (when (eq mode major-mode)
                 (setq mode-name mode-str)))))
  (add-hook 'after-change-major-mode-hook 'clean-mode-line)

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
                        (+ (count-lines (point-max) (point-min)) 1)))))

(leaf mozc
  :if (executable-find "mozc_emacs_helper")
  :init
  (leaf popup :ensure t)
  :require mozc-popup
  :preface
  (defun my-mozc-handle-event (event)
    "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
    (when (member event (list 'zenkaku-hankaku 'muhenkan))
      (mozc-clean-up-session)
      (toggle-input-method)
      t))
  :bind
  ("<zenkaku-hankaku>" . toggle-input-method)
  ("<henkan>" . (lambda () (interactive)
                  (when (null current-input-method) (toggle-input-method))))
  ("<muhenkan>" . (lambda () (interactive) (deactivate-input-method)))
  :hook
  (input-method-activate-hook . (lambda () (set-cursor-color "green")))
  (input-method-deactivate-hook . (lambda () (set-cursor-color "red")))
  :advice
  (:before-until mozc-handle-event my-mozc-handle-event)
  :custom
  (default-input-method . "japanese-mozc")
  (mozc-candidate-style . 'popup))

(leaf recentf
  :preface
  (defun my-recentf-inhibit-message:around (orig-func &rest args)
    "recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
(*Messages* バッファには出力される)"
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  :custom `((recentf-max-menu-items . 500)
            (recentf-max-saved-items . 500)
            (recentf-exclude . '("recentf" "COMMIT_EDITMSG"))
            (recentf-auto-cleanup . 'never)
            (recentf-save-file . ,(expand-file-name "recentf" my-emacs-data-home)))
  :advice
  (:around recentf-cleanup my-recentf-inhibit-message:around)
  (:around recentf-save-list my-recentf-inhibit-message:around)
  :config
  (recentf-mode 1)
  (run-with-idle-timer 30 t 'recentf-save-list))

(leaf server
  :require t
  :config
  (unless (server-running-p)
    (server-start)))

(leaf uniquify
  :require t
  :custom ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
           (uniquify-ignore-buffers-re . "*[^*]+*")))

(leaf bookmark
  :custom `((bookmark-default-file . ,(expand-file-name "bookmarks" my-emacs-data-home))))

(leaf savehist
  :custom `(savehist-file . ,(expand-file-name "history" my-emacs-data-home))
  :global-minor-mode t)

(leaf ediff
  :preface
  ;; ref: https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
  (defun my-ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun my-add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'my-ediff-copy-both-to-C))
  :hook (ediff-keymap-setup-hook . my-add-d-to-ediff-mode-map)
  :custom ((ediff-window-setup-function . 'ediff-setup-windows-plain)))

(leaf dired
  :bind (:dired-mode-map
         ("a" . dired-find-file)
         ("RET" . my-dired-open-in-accordance-with-situation))
  :hook (dired-mode-hook . my-dired-append-buffer-name-hint)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my-dired-append-buffer-name-hint ()
    "dired バッファに [dir] 追加"
    (when (eq major-mode 'dired-mode)
      (rename-buffer (concat (buffer-name) " [dir]") t)))
  (defun my-dired-open-in-accordance-with-situation ()
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file)))))

(leaf vertico
  :ensure t
  :require vertico-directory
  :bind (:vertico-map ("C-l" . vertico-directory-up))
  :custom ((vertico-count . 14)
           (vertico-cycle . t)
           (vertico-sort-function . #'vertico-sort-history-alpha))
  :global-minor-mode t)

(leaf consult
  :ensure t
  :custom ((xref-show-xrefs-function . #'consult-xref)
           (xref-show-definitions-function . #'consult-xref))
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s g" . consult-git-grep))
  :config
  (consult-customize
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult-ripgrep consult-git-grep
   :preview-key "M-.")

  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      ;; (define-key map "\C-w" #'my-yank-char) ;; see: isearch--yank-char-or-syntax
      map))
  (consult-customize consult-line :keymap my-consult-line-map))

(leaf consult-ghq
  :if (executable-find "ghq")
  :ensure t
  :bind
  ("C-c C-]" . my-consult-ghq-magit-status)
  :config
  (defun my-consult-ghq-magit-status ()
    "Show magit status from ghq."
    (interactive)
    (let ((repo (consult--read (consult-ghq--list-candidates) :prompt "Repo: ")))
      (magit-status repo))))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless))
  (orderless-component-separator . #'orderless-escapable-split-on-space)
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles . (partial-completion))))))

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf corfu
  :ensure t
  :custom
  (corfu-auto . t)
  (corfu-auto-prefix . 4)
  (corfu-cycle . t)
  (corfu-quit-no-match . 'separator)
  :bind ((:corfu-map
          ("M-s" . corfu-insert-separator)))
  :global-minor-mode global-corfu-mode)

(leaf cape
  :ensure t
  :require t
  :preface
  (defun my-custom-capf ()
    (let* ((capfs (remove t completion-at-point-functions)))
      (add-to-list 'capfs #'cape-dabbrev t)
      (setq-local completion-at-point-functions
                  `(cape-file
                    ,(cape-capf-inside-code #'cape-elisp-symbol)
                    ,(cape-capf-buster (apply #'cape-capf-super capfs))))))
  :hook ((emacs-lisp-mode-hook) . my-custom-capf)
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(leaf kind-icon
  :ensure t
  :after corfu
  :require t
  :custom
  (kind-icon-default-face . 'corfu-default)
  `(svg-lib-icons-dir . ,(expand-file-name "svg-lib/" my-emacs-cache-home))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf embark
  :ensure t
  :custom (embark-help-key . "?")
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim))

(leaf embark-consult
  :ensure t
  :after embark
  :require t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf anzu
  :ensure t
  :custom
  (anzu-mode-lighter . "")
  (anzu-deactivate-region . t)
  (anzu-search-threshold . 1000)
  :bind
  ("M-%" . 'anzu-query-replace)
  ("C-M-%" . 'anzu-query-replace-regexp)
  ("C-c Q" . 'anzu-query-replace-at-cursor-thing)
  :global-minor-mode global-anzu-mode)

(leaf elec-pair
  :preface
  (defun my-inhibit-electric-pair-mode (char)
    (minibufferp))
  :custom
  (electric-pair-inhibit-predicate . #'my-inhibit-electric-pair-mode)
  :global-minor-mode electric-pair-mode
  :config
  (setopt electric-pair-pairs (append electric-pair-pairs '((?` . ?`)))))

(leaf puni
  :ensure t
  :preface
  (defun my-disable-puni-in-minibuffer ()
    "Disable `puni-mode' in minibuffer unless when eval-expression"
    (unless (eq this-command 'eval-expression)
      (puni-disable-puni-mode)))
  :hook
  prog-mode-hook
  (minibuffer-setup-hook . my-disable-puni-in-minibuffer)
  :bind (:puni-mode-map
         ("C-h" . puni-backward-delete-char)
         ("M-D" . puni-splice)
         ("C-)" . puni-slurp-forward)
         ("C-(" . puni-barf-forward)))

(leaf sudo-edit :ensure t)

(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  :require t
  :custom
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-use-pattern-alist . t)
  (migemo-use-frequent-pattern-alist . t)
  (migemo-pattern-alist-length . 1000)
  (migemo-coding-system . 'utf-8-unix)
  :config
  (migemo-init)
  (set-process-query-on-exit-flag (get-process "migemo") nil))

(leaf eglot
  :ensure t
  :hook ((ruby-mode-hook typescript-mode-hook yaml-mode-hook) . eglot-ensure)
  :advice
  (:around save-buffers-kill-emacs
           (lambda (orig-fun &rest args)
             (cl-letf (((symbol-function #'process-list) (lambda ())))
               (apply orig-fun args))))
  :custom ((eglot-autoshutdown . t)
           (eldoc-echo-area-use-multiline-p . nil)
           (eglot-ignored-server-capabilities . '(:documentOnTypeFormattingProvider))))

(leaf open-junk-file
  :ensure t
  :custom
  (open-junk-file-format . "/tmp/emacs-junk/%Y-%m-%d-%H%M%S."))

(leaf sh-mode
  :custom
  (sh-basic-offset . 2)
  (sh-indentation . 2))

(leaf tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         (:tempel-map
          ("C-c C-c" . tempel-done)
          ("<tab>" . tempel-next)
          ("<backtab>" . tempel-previous)))
  :preface
  (defun my-tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((conf-mode-hook prog-mode-hook text-mode-hook) . my-tempel-setup-capf))

(leaf feature-mode
  :ensure t
  :custom
  (feature-default-language . "ja"))

(leaf flycheck
  :ensure t
  :bind ((:flycheck-mode-map
          ("M-n" . flycheck-next-error)
          ("M-p" . flycheck-previous-error)))
  :custom ((flycheck-check-syntax-automatically . '(mode-enabled save)))
  :hook ruby-mode-hook)

(leaf haml-mode
  :ensure t)

(leaf js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook (js2-mode-hook
         . (lambda ()
             (setq js2-global-externs (list "jQuery" "$"))
             (setq js2-additional-externs (list "jQuery" "$"))
             (setq js2-include-browser-externs nil)
             (setq js2-mode-show-parse-errors nil)
             (setq js2-mode-show-strict-warnings nil)
             (setq js2-highlight-external-variables nil)
             (setq js2-include-jslint-globals nil)
             (flycheck-mode)))
  :custom
  (js-indent-level . 2)
  (js2-basic-offset . 2))

(leaf mermaid-mode
  :ensure t
  :mode "\\.mmd\\'"
  :commands mermaid-compile-region)

(leaf markdown-mode
  :ensure t
  :after transient
  :mode ("\\.md\\'" . gfm-mode)
  :bind (:markdown-mode-map
         ("<S-tab>" . markdown-shifttab)
         ("C-c ." . my-markdown-mode-transient))
  :custom ((markdown-asymmetric-header . t)
           (markdown-fontify-code-blocks-natively . t)
           (markdown-gfm-use-electric-backquote . nil)
           (markdown-indent-on-enter . 'indent-and-new-item))
  :custom-face
  (markdown-code-face . '((t (:inherit default :foreground "medium aquamarine"))))
  :transient
  (my-markdown-mode-transient
   ()
   "Transient for markdown-mode"
   [["Styles"
     ("b" "Bold" markdown-insert-bold)
     ("i" "Italic" markdown-insert-italic)
     ("c" "Code" markdown-insert-code)
     ("s" "Strikethrough" markdown-insert-strike-through)
     ("q" "Blockquotes" markdown-insert-blockquote)
     ("B" "GFM Code Block" markdown-insert-gfm-code-block)]
    ["Heading"
     ("1" "Header1" markdown-insert-header-atx-1)
     ("2" "Header2" markdown-insert-header-atx-2)
     ("3" "Header3" markdown-insert-header-atx-3)]
    ["Link & Image"
     ("l" "Link" markdown-insert-link)
     ("p" "Image" markdown-insert-image)]
    ["List"
     ("m" "Insert List item" markdown-insert-list-item)
     ("t" "GFM checkbox" markdown-insert-gfm-checkbox)]
    ["Preview & Export"
     ("P" "Preview" markdown-preview)
     ("r" "Live Export" markdown-live-preview-mode)]])
  :defer-config
  (define-key markdown-mode-map (kbd "C-c C-r") 'mermaid-compile-region))

(leaf ruby-mode
  :preface
  (defun my-ruby-smie-rules (kind token)
    (pcase (cons kind token)
      (`(:before . ,(or "(" "[" "{"))
       (cond
        ;; expect(...).to eq [
        ;;   ...
        ;; ]
        ((and (smie-rule-hanging-p)
              (smie-rule-parent-p " @ "))
         (cons 'column (current-indentation)))))))
  :advice
  (:before-until ruby-smie-rules my-ruby-smie-rules)
  :mode "\\.\\(ruby\\|plugin\\|irbrc\\)\\'"
  :custom ((ruby-block-indent . nil)
           (ruby-bracketed-args-indent . nil)
           (ruby-insert-encoding-magic-comment . nil)
           (ruby-method-call-indent . nil)
           (ruby-method-params-indent . nil))
  :hook
  (ruby-mode-hook
   . (lambda ()
       (setq-local flycheck-command-wrapper-function
                   (lambda (command)
                     (let ((config-dir (locate-dominating-file buffer-file-name
                                                               "Gemfile")))
                       (if (and config-dir
                                (file-exists-p (expand-file-name "Gemfile.lock" config-dir))
                                (ruby-flymake-rubocop--use-bundler-p config-dir))
                           (append '("bundle" "exec") command)
                         command))))))
  :config
  ;; workaround: https://gnu.emacs.bug.narkive.com/H2x8ODth/bug-42841-28-0-50-ruby-mode-ruby-beginning-end-of-block-doesn-t-work-as-is-exepected-if-arguments
  (setq ruby-deep-indent-paren (delete ?\( ruby-deep-indent-paren))
  (font-lock-add-keywords
   'ruby-mode
   `(("\\s *def\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)"
      1 font-lock-function-name-face)
     (,(concat ruby-font-lock-keyword-beg-re
               "\\_<\\(nil\\|true\\|false\\)\\_>")
      1 font-lock-keyword-face)
     ;; ex. if: :hoge
     (,(concat "\\(?:^\\s *\\|[[{(,]\\s *\\|\\sw\\s +\\)\\("
               (regexp-opt '("if" "unless" "in" "format" "class" "retry" "require" "using") "\\(?:")
               ":\\)")
      1 font-lock-constant-face))))

(leaf inf-ruby
  :ensure t
  :hook (after-init-hook . inf-ruby-switch-setup))

(leaf yard-mode
  :ensure t
  :hook ruby-mode-hook
  :config
  (setq yard-tags-re (regexp-opt (append yard-tags '("rbs")))))

(leaf rspec-mode
  :ensure t
  :custom ((rspec-spec-command . "rspec -c")
           (rspec-use-rake-when-possible . nil)
           (rspec-use-spring-when-possible . nil)))

(leaf rubocop
  :ensure t
  :commands my-rubocop-check-project-with-options
  :config
  (defun my-rubocop-check-project-with-options (options)
    "Run check on current project with OPTIONS."
    (interactive "sOptions: ")
    (let ((command (concat rubocop-check-command " " options)))
      (rubocop--dir-command command (rubocop-project-root)))))

(leaf rurema-mode
  :mode "\\.rd\\'" "/refm/api/src/[^.]+\\'")

(leaf css-mode
  :mode "\\.scss\\'"
  :custom
  (css-indent-offset . 2))

(leaf sass-mode :ensure t)

(leaf typescript-mode
  :ensure t
  :custom ((typescript-indent-level . 2))
  :hook (typescript-mode-hook
         . (lambda ()
             (unless (string-match "/node_modules/" (or (buffer-file-name) ""))
               (flycheck-mode)))))

(leaf typescript-tsx-mode
  :mode "\\.tsx\\'"
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx"))

(leaf web-mode
  :ensure t
  :mode "\\.html?\\'" "\\.erb\\'" "\\.jsp\\'" "\\.vue\\'"
  :custom ((web-mode-enable-auto-indentation . nil)
           (web-mode-attr-indent-offset . 2)
           (web-mode-block-padding . 2)
           (web-mode-code-indent-offset . 2)
           (web-mode-css-indent-offset . 2)
           (web-mode-markup-indent-offset . 2)
           (web-mode-script-padding . 2)
           (web-mode-comment-formats . '(("javascript" . "//")
                                         ("css"        . "/*"))))
  :hook
  (web-mode-hook
   . (lambda ()
       (when (equal web-mode-content-type "javascript")
         (flycheck-add-mode 'javascript-eslint 'web-mode)
         (flycheck-mode))
       (when (string-match "\\.vue\\'" (or (buffer-file-name) ""))
         (setq-local web-mode-script-padding nil)
         (setq-local web-mode-style-padding nil)))))

(leaf json-mode
  :ensure t
  :mode "\\.json5\\'"
  :custom (js-indent-level . 2))

(leaf csv-mode
  :ensure t
  :init
  (modify-coding-system-alist 'file "\\.csv\\'" 'cp932-dos))

(leaf rainbow-csv
  :ensure t
  :hook csv-mode-hook)

(leaf lua-mode
  :ensure t
  :custom (lua-indent-level . 2))

(leaf nix-mode
  :ensure t)

(leaf add-node-modules-path
  :ensure t
  :hook (typescript-mode-hook . add-node-modules-path))

(leaf magit
  :ensure t
  :require (magit-mode magit-bookmark)
  :custom ((magit-diff-highlight-hunk-body . nil)
           (magit-log-margin . '(t "%Y-%m-%d %H:%M:%S " magit-log-margin-width t 18))
           (magit-section-initial-visibility-alist . '((unpushed . show) (stashes . show) (untracked . show))))
  :custom-face
  (magit-diff-added . '((t (:foreground "green"))))
  (magit-diff-added-highlight . '((t (:foreground "green"))))
  (magit-diff-file-header . '((t (:foreground "yellow"))))
  (magit-diff-removed . '((t (:foreground "red"))))
  (magit-diff-removed-highlight . '((t (:foreground "red"))))
  (magit-hash . '((t (:foreground "gold"))))
  (magit-item-highlight . '((t (:background "gray5"))))
  :hook (git-commit-mode-hook . display-fill-column-indicator-mode)
  :config
  (with-eval-after-load 'magit-branch
    (defun my-magit-gh-pr-checkout (pr-number detach)
      (let* ((args (append '("pr" "checkout")
                           (when detach '("--detach"))
                           (list (number-to-string pr-number))))
             (cmd (string-join (cons "gh" args) " ")))
        (message "Executing: %s" cmd)
        (apply #'call-process "gh" nil nil nil args)
        (magit-refresh)))

    (defun my-magit-gh-pr-candidates ()
      (let ((command
             (concat "gh api -X GET 'repos/{owner}/{repo}/pulls' "
                     "--paginate -F sort=created -F direction=desc "
                     "--jq '.[] | {number: .number, title: .title}' "
                     "| jq -s -c '.'")))
        (condition-case err
            (let* ((json-string (shell-command-to-string command))
                   (pr-data (if (string-empty-p json-string)
                                nil
                              (json-parse-string
                               json-string :object-type 'alist))))
              (if pr-data
                  (mapcar (lambda (pr-item)
                            (cons (format "%s: %s"
                                          (alist-get 'number pr-item)
                                          (alist-get 'title pr-item))
                                  (alist-get 'number pr-item)))
                          pr-data)
                (error "Pull request not found.")))
          (error (error-message-string err)))))

    (defun my-magit-gh-pr-completion-read (prompt)
      (consult--read
       (consult--slow-operation "Collecting Pull Requests..."
         (my-magit-gh-pr-candidates))
       :prompt prompt
       :lookup #'consult--lookup-cdr))

    (defun my-magit-gh-pr-checkout-detach ()
      (interactive)
      (if-let* ((pr (my-magit-gh-pr-completion-read "GitHub PR number (detach): ")))
        (my-magit-gh-pr-checkout pr t)))

    (defun my-magit-gh-pr-checkout-normal ()
      (interactive)
      (if-let* ((pr (my-magit-gh-pr-completion-read "GitHub PR number (branch): ")))
        (my-magit-gh-pr-checkout pr nil)))

    (transient-append-suffix 'magit-branch "c"
      '("p" "Checkout PR (detach)" my-magit-gh-pr-checkout-detach))
    (transient-append-suffix 'magit-branch "c"
      '("P" "Checkout PR (branch)" my-magit-gh-pr-checkout-normal))))

(leaf xterm-color
  :ensure t
  :require t)

(leaf magit-delta
  :ensure t
  :after xterm-color
  :custom ((magit-delta-delta-args
            . `("--max-line-distance" "0.6"
                "--true-color" ,(if xterm-color--support-truecolor "always" "never")
                "--color-only"
                "--plus-style" "syntax #003345"
                "--plus-emph-style" "syntax #006b6b"
                "--features" "magit-delta"))
           (magit-delta-hide-plus-minus-markers . nil))
  :hook magit-mode-hook)

(leaf browse-at-remote
  :ensure t
  :bind (("C-c g g" . browse-at-remote))
  :custom ((browse-at-remote-prefer-symbolic . nil)
           (browse-at-remote-preferred-remote-name . "upstream")))

(leaf docker
  :ensure t
  :config
  (leaf dockerfile-mode :ensure t))

(leaf yaml-mode :ensure t)

(leaf nxml-mode
  :custom (nxml-child-indent . 4))

(leaf sqlformat
  ;; pip3 install sqlparse
  :if (executable-find "sqlformat")
  :ensure t
  :custom (sqlformat-command . 'sqlformat)
  :bind ((:sql-mode-map
          ("C-c C-f" . sqlformat))))

(leaf tree-sitter
  :ensure (t tree-sitter-langs)
  :require tree-sitter-langs
  :hook (typescript-tsx-mode-hook
         (tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :custom-face
  (tree-sitter-hl-face:property . '((t (:inherit font-lock-constant-face :slant normal))))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

(leaf quickrun
  :ensure t
  :custom
  (quickrun-truncate-lines . nil)
  (quickrun-timeout-seconds . 30))

(leaf ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(leaf auth-source
  :custom
  `(auth-sources . '(,(file-name-concat my-emacs-data-home ".authinfo.gpg"))))

(leaf auth-source-1password
  :if (executable-find "op")
  :ensure t
  :custom
  (auth-source-1password-vault . "Employee")
  :config
  (auth-source-1password-enable))

(leaf copilot
  :if (executable-find "node")
  :ensure t
  :hook (prog-mode-hook yaml-mode-hook)
  ;; ref: https://github.com/copilot-emacs/copilot.el/issues/103
  :bind ((:copilot-completion-map
          ("C-c C-c" . copilot-accept-completion)
          ("M-f" . copilot-accept-completion-by-word)
          ("M-p" . copilot-previous-completion)
          ("M-n" . copilot-next-completion)))
  :custom
  (copilot-indent-offset-warning-disable . t)
  (copilot-max-char . -1)
  (copilot-server-log-level . 3)
  :custom-face
  (copilot-overlay-face . '((t (:background "gray5")))))

(leaf copilot-chat
  :ensure t
  :custom
  (copilot-chat-frontend . 'markdown)
  (copilot-chat-default-model . "gemini-2.5-pro")
  (copilot-chat-markdown-prompt
   . "I'll behave like a familiar, friendly gal, and won't use polite language.
Sometimes I'll express emotions like a human. Please respond in Japanese.")
  :bind (("C-c C-t" . copilot-chat-transient)
         (:git-commit-mode-map
          :package git-commit
          ("C-c m" . copilot-chat-insert-commit-message))
         (:embark-general-map
          :package embark
          ("T" . copilot-chat-transient))))

(leaf dashboard
  :ensure t
  :custom
  (dashboard-items . '((recents   . 10)
                       (bookmarks . 5)
                       (projects  . 5)))
  :config
  (dashboard-setup-startup-hook))

(leaf gptel
  :ensure t
  :bind
  ("C-c g m" . gptel-menu)
  ("C-c g t" . gptel-tools)
  (:gptel-mode-map
   ("C-c C-c" . gptel-send))
  (:embark-general-map
   :package embark
   ("G" . gptel-menu))
  :custom
  (gptel-model . 'claude-sonnet-4)
  (gptel-confirm-tool-calls . t)
  `(gptel-gh-github-token-file . ,(file-name-concat my-emacs-cache-home "gptel/copilot-chat/github-token"))
  `(gptel-gh-token-file . ,(file-name-concat my-emacs-cache-home "gptel/copilot-chat/token"))
  (gptel-directives
   . '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. Please respond in Japanese.")
       (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note. Please respond in Japanese.")
       (writing     . "You are a large language model and a writing assistant. Respond concisely. Please respond in Japanese.")
       (chat        . "You are a large language model and a conversation partner. Respond concisely. Please respond in Japanese.")))
  (gptel-prompt-prefix-alist . '((markdown-mode . "**Prompt**\n")
                                 (text-mode . "**Prompt**\n")))
  (gptel-response-prefix-alist . '((markdown-mode . "**Response**\n")
                                   (text-mode . "**Response**\n")))
  :hook
  (gptel-post-response-functions . gptel-end-of-response)
  :config
  (require 'gptel-integrations)
  (setopt gptel-backend (gptel-make-gh-copilot "Copilot")))

(leaf llm-tool-collection
  :ensure t
  :vc (:url "https://github.com/skissue/llm-tool-collection")
  :after gptel
  :config
  (mapcar (apply-partially #'apply #'gptel-make-tool)
        (llm-tool-collection-get-all)))

(leaf mcp-hub
  :vc (:url "https://github.com/lizqwerscott/mcp.el")
  :bind
  (:mcp-hub-mode-map
   ("?" . my-mcp-hub-tmenu))
  :transient
  (my-mcp-hub-tmenu
   ()
   "MCP Hub Control"
   [["Server Management"
     ("l" "View log" mcp-hub-view-log)
     ("s" "Start Server" mcp-hub-start-server)
     ("k" "Close Server" mcp-hub-close-server)
     ("r" "Restart Server" mcp-hub-restart-server)]
    ["All Server Management"
     ("S" "Start All Servers" mcp-hub-start-all-server)
     ("K" "Close All Servers" mcp-hub-close-all-server)
     ("R" "Restart All Servers" mcp-hub-restart-all-server)]])
  :config
  (setq mcp-hub-servers
        `(("filesystem" . ( :command "npx"
                            :args ("-y"
                                   "@modelcontextprotocol/server-filesystem"
                                   "~/ghq")))
          ("github" . ( :command "docker"
                        :args ("run" "-i" "--rm"
                               "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                               "ghcr.io/github/github-mcp-server:0.4.0")
                        :env (:GITHUB_PERSONAL_ACCESS_TOKEN
                              ,(auth-source-pick-first-password
                                :host "github-api" :user "token")))))))

(leaf request
  :custom
  `(request-storage-directory . ,(file-name-concat my-emacs-cache-home "request")))

(let ((local-init (file-name-concat user-emacs-directory "init_local.el")))
  (if (file-exists-p local-init)
      (load-file local-init)))
