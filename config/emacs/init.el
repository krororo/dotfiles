(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(prog1 "leaf"
  (prog1 "install leaf"
    (custom-set-variables
     '(package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/"))))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf)))

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf el-get :ensure t)

    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf xdg
  :require t
  :config
  (defvar my-emacs-cache-home (expand-file-name "emacs" (xdg-cache-home))))

(leaf exec-path-from-shell
  :ensure t
  :custom ((exec-path-from-shell-warn-duration-millis . 2000)
           (exec-path-from-shell-shell-name . "/usr/bin/zsh"))
  :config
  (exec-path-from-shell-copy-env "PATH"))

(leaf package-utils :ensure t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf *initialize-emacs
  :custom ((initial-buffer-choice . "~/memo.org")
           (scroll-conservatively . 1)
           (scroll-margin . 5)
           (scroll-preserve-screen-position . nil)
           (compilation-scroll-output . t)
           (create-lockfiles . nil)
           (indicate-buffer-boundaries . 'left)
           (use-default-font-for-symbols . nil)
           (make-backup-files . nil) ;; *.~ とかのバックアップファイルを作らない
           (auto-save-default . nil) ;; .#* とかのバックアップファイルを作らない
           (inhibit-startup-message . t) ;; 起動時のメッセージを非表示
           (visible-bell . t)
           (ring-bell-function . 'ignore))
  :custom-face
  (default . '((t (:background "#003300" :foreground "white" :height 100 :foundry "PfEd" :family "HackGen"))))
  (font-lock-comment-face . '((t (:foreground "gray"))))
  (font-lock-keyword-face . '((t (:foreground "magenta"))))
  :setq-default
  (indent-tabs-mode . nil)
  :config
  (set-fontset-font nil '(#x1F000 . #x1FAFF) "Noto Color Emoji")

  (set-cursor-color "red")
  (set-language-environment 'utf-8)

  (fset 'yes-or-no-p 'y-or-n-p)

  (if window-system (menu-bar-mode 1) (menu-bar-mode -1))
  (tool-bar-mode -1)
  (column-number-mode t)
  (show-paren-mode t)
  (set-scroll-bar-mode nil)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  (set-frame-parameter nil 'fullscreen 'maximized))

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
  (whitespace-tab . '((t (:foreground "royal blue" :background nil :underline t))))
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

(leaf eaw
  :el-get (hamano/locale-eaw
           :name eaw)
  :require t
  :config (eaw-fullwidth))

(leaf windmove
  :config
  (windmove-default-keybindings))

(leaf git-gutter
  :ensure t
  :global-minor-mode global-git-gutter-mode)

(leaf avy
  :ensure t
  :bind (("C-M-:" . avy-goto-char-timer)))

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

(leaf *key-binding
  :preface
  (defun my-move-beginning-alt ()
    (interactive)
    (if (bolp)
        (back-to-indentation)
      (beginning-of-line)))
  (defun my-reverse-other-window ()
    (interactive)
    (other-window -1))
  :bind (("C-a" . my-move-beginning-alt)
         ("C-h" . delete-backward-char)
         ("C-c u" . comment-region)
         ("C-c y" . uncomment-region)
         ("C-c r" . revert-buffer)
         ;; ウィンドウ逆移動
         ("C-x p" . my-reverse-other-window)
         ;; 暴発するので無効化
         ("C-x C-p" . nil)
         ;; compose-mail
         ("C-x m" . nil)
         ("M-\"" . insert-pair)
         ("M-'" . insert-pair))
  :init
  (ffap-bindings))

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
      (git-gutter-mode . " GG")
      ;; Major modes
      (emacs-lisp-mode . "El")
      (enh-ruby-mode . "EnhRb")
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
  :ensure t
  :require t
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
  ("<muhenkan>" . (lambda () (interactive) (inactivate-input-method)))
  :hook
  (input-method-activate-hook . (lambda () (set-cursor-color "green")))
  (input-method-inactivate-hook . (lambda () (set-cursor-color "red")))
  :advice
  (:before-until mozc-handle-event my-mozc-handle-event)
  :config
  (setq default-input-method "japanese-mozc"))

(leaf mozc-cand-posframe
  :ensure t
  :after mozc
  :require t
  :custom-face
  (mozc-cand-posframe-normal-face . '((t (:background "#333333" :foreground "#dcd4be"))))
  (mozc-cand-posframe-footer-face . '((t (:foreground "#ededed"))))
  :config
  (setq mozc-candidate-style 'posframe))

(leaf recentf
  :preface
  (defun my-recentf-inhibit-message:around (orig-func &rest args)
    "recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
(*Messages* バッファには出力される)"
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  :custom ((recentf-max-menu-items . 500)
           (recentf-max-saved-items . 500)
           (recentf-exclude . '("recentf" "COMMIT_EDITMSG"))
           (recentf-auto-cleanup . 'never))
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
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "*[^*]+*"))

(leaf *dired
  :preface
  (defun dired-my-append-buffer-name-hint ()
    "dired バッファに [dir] 追加"
    (when (eq major-mode 'dired-mode)
      (rename-buffer (concat (buffer-name) " [dir]") t)))
  :hook (dired-mode-hook . dired-my-append-buffer-name-hint)
  :config
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))
  (define-key dired-mode-map "a" 'dired-find-file)
  (define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation))

(leaf dired-subtree
  :ensure t
  :bind ((:dired-mode-map
          ("i" . dired-subtree-insert)
          ("<tab>" . dired-subtree-remove))))

(leaf vertico
  :ensure t
  :require vertico-directory
  :bind (:vertico-map ("C-l" . vertico-directory-up))
  :custom ((vertico-count . 14)
           (vertico-cycle . t))
  :init
  (vertico-mode)
  (savehist-mode))

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
   :preview-key (kbd "M-.")))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless))
  (orderless-component-separator . #'orderless-escapable-split-on-space)
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles . (partial-completion))))))

(leaf marginalia
  :ensure t
  :config
  (marginalia-mode)
  ;; https://github.com/bbatsov/projectile/issues/1664#issuecomment-934632497
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-file-other-window . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file))
                marginalia-command-categories)))

(leaf corfu
  :ensure t
  :custom
  (corfu-auto . t)
  (corfu-cycle . t)
  (corfu-quit-no-match . 'separator)
  :global-minor-mode corfu-global-mode)

(leaf cape
  :ensure t
  :require t
  :preface
  (defun my-custom-capf ()
    (let* ((capfs (remove t completion-at-point-functions)))
      (add-to-list 'capfs #'cape-dabbrev t)
      (setq-local completion-at-point-functions
                  `(cape-file
                    ,(cape-capf-buster (apply #'cape-super-capf capfs))))))
  :hook ((lsp-completion-mode-hook emacs-lisp-mode-hook) . my-custom-capf)
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
  :bind ("C-." . embark-act))

(leaf embark-consult
  :ensure t
  :after consult embark
  :require t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf projectile
  :ensure t
  :custom (projectile-mode-line-prefix . " Prj")
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(leaf anzu
  :ensure t
  :custom
  (anzu-mode-lighter . "")
  (anzu-deactivate-region . t)
  (anzu-search-threshold . 1000)
  :bind
  ("C-c q" . 'anzu-query-replace-regexp)
  ("C-c Q" . 'anzu-query-replace-at-cursor-thing)
  :global-minor-mode global-anzu-mode)

(leaf company
  :ensure t
  :custom ((compnay-idle-delay . 0.3)
           (company-selection-wrap-around . t)
           (company-require-match . 'never)
           (company-backends . '(company-files
                                 (company-capf :with company-dabbrev-code)
                                 (company-dabbrev-code
                                  company-gtags
                                  company-etags
                                  company-keywords))))
  :bind ((:company-active-map
          ;; disable help
          ("C-h" . nil))))

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

(leaf lsp-mode
  :ensure t
  ;; workaround for `require: Symbol’s value as variable is void: lsp-ada-project-file`
  ;; refs: https://github.com/emacs-lsp/lsp-mode/commit/491d667d1e113bd6b43d1f88d47383e7fb137ddb
  :custom ((lsp-client-packages . '(lsp-solargraph))
           (lsp-completion-provider . :none)
           (lsp-diagnostics-provider . :none)
           (lsp-keymap-prefix . "C-c l"))
  :init
  (leaf lsp-ui :ensure t)
  (leaf consult-lsp :ensure t)
  :hook ((enh-ruby-mode-hook . lsp)
         (ruby-mode-hook . lsp)))

(leaf sh-mode
  :config
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(leaf yasnippet
  :ensure t
  :require t
  :custom (yas-snippet-dirs . `(,(locate-user-emacs-file "snippets")))
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file))

(leaf feature-mode
  :ensure t
  :config
  (setq feature-default-language "ja"))

(leaf flycheck
  :ensure t
  :bind ((:flycheck-mode-map
          ("M-n" . flycheck-next-error)
          ("M-p" . flycheck-previous-error)))
  :custom ((safe-local-variable-values . '((encoding . utf-8)))
           (flycheck-check-syntax-automatically . '(mode-enabled save)))
  :hook ruby-mode-hook)

(leaf eruby-mode
  :el-get petere/emacs-eruby-mode
  :custom-face ((eruby-standard-face . '((t (:background "gray" :foreground "black"))))))

(leaf haml-mode
  :ensure t)

(leaf js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook
            '(lambda()
               (setq js2-global-externs (list "jQuery" "$"))
               (setq js2-additional-externs (list "jQuery" "$"))
               (setq js2-include-browser-externs nil)
               (setq js2-mode-show-parse-errors nil)
               (setq js2-mode-show-strict-warnings nil)
               (setq js2-highlight-external-variables nil)
               (setq js2-include-jslint-globals nil)
               (flycheck-mode)))
  (setq js-indent-level 2)
  (setq js2-basic-offset 2))

(leaf markdown-mode
  :ensure t
  :custom (markdown-fontify-code-blocks-natively . t)
  :custom-face
  (markdown-code-face . '((t (:inherit default :foreground "medium aquamarine")))))

(leaf review-mode
  :ensure t
  :mode "\\.re_?\\(\\.erb\\)?\\'"
  :custom-face
  (review-mode-bold-face . '((t (:foreground "deep sky blue" :weight bold))))
  (review-mode-header1-face . '((t (:foreground "chartreuse" :weight bold))))
  (review-mode-header2-face . '((t (:foreground "lawn green" :weight bold))))
  (review-mode-header3-face . '((t (:foreground "green" :weight bold))))
  (review-mode-header4-face . '((t (:foreground "#0dd" :weight bold))))
  (review-mode-italic-face . '((t (:foreground "red" :slant italic :weight bold))))
  (review-mode-title-face . '((t (:foreground "cyan" :weight bold))))
  (review-mode-underline-face . '((t (:foreground "cyan" :underline t)))))

(leaf ruby-mode
  :preface
  (defun my-ruby-smie-rules (kind token)
    (pcase (cons kind token)
      ('(:before . ".")
       (cond
        ((smie-rule-sibling-p)
         (smie-backward-sexp ".")
         (forward-char -1)
         (if (ruby-smie--bosp)
             0
           (smie-backward-sexp ";")
           (cons 'column (+ (current-column) ruby-indent-level))))
        ((not (smie-rule-parent-p ";"))
         (smie-rule-parent ruby-indent-level))))))
  :advice
  (:before-until ruby-smie-rules my-ruby-smie-rules)
  :mode "\\.\\(ruby\\|plugin\\)\\'"
  :custom (ruby-insert-encoding-magic-comment . nil)
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
  (font-lock-add-keywords
   'ruby-mode
   `(("\\s *def\\s +\\(?:[^( \t\n.]*\\.\\)?\\([^( \t\n]+\\)"
      1 font-lock-function-name-face)
     (,(concat ruby-font-lock-keyword-beg-re
               "\\_<\\(nil\\|true\\|false\\)\\_>")
      1 font-lock-keyword-face)
     ("\\(?:^\\s *\\|[[{(,]\\s *\\|\\sw\\s +\\)\\(\\(?:if\\|unless\\):\\)"
      1 font-lock-constant-face))))

(leaf enh-ruby-mode
  :disabled t
  :if (executable-find "ruby")
  :ensure t
  :require ruby-mode
  :mode "\\.\\(rb\\|ru\\)\\'"
  :custom-face
  (enh-ruby-op-face . '((t (:foreground "gainsboro"))))
  :bind ((:enh-ruby-mode-map
          ("C-c '" . ruby-toggle-string-quotes)
          ("C-M-f" . forward-sexp)
          ("C-M-b" . backward-sexp)))
  :hook (enh-ruby-mode-hook
         . (lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode)
             (add-hook 'electric-indent-functions #'ruby--electric-indent-p nil 'local))))

(leaf yard-mode
  :ensure t
  :hook ruby-mode-hook enh-ruby-mode-hook)

(leaf rspec-mode
  :ensure t
  :custom ((rspec-spec-command . "rspec -c")
           (rspec-use-rake-when-possible . nil)
           (rspec-use-spring-when-possible . nil)))

(leaf rubocop :ensure t)

(leaf rd-mode
  :el-get (rd-mode
           :url "https://github.com/uwabami/rdtool/raw/master/utils/rd-mode.el")
  ;; for rurema
  :mode "/refm/api/src/")

(leaf css-mode
  :mode "\\.scss\\'"
  :config
  (setq css-indent-offset 2))

(leaf sass-mode :ensure t)

(leaf typescript-mode
  :ensure t
  :custom ((typescript-indent-level . 2))
  :config
  (add-hook 'typescript-mode-hook
            '(lambda ()
               (unless (string-match "/node_modules/" (or (buffer-file-name) ""))
                 (flycheck-mode)))))

(leaf web-mode
  :ensure t
  :mode "\\.html?\\'" "\\.vm\\'" "\\.jsp\\'" "\\.vue\\'"
  :custom ((web-mode-enable-auto-indentation . nil)
           (web-mode-attr-indent-offset . 2)
           (web-mode-block-padding . 2)
           (web-mode-code-indent-offset . 2)
           (web-mode-css-indent-offset . 2)
           (web-mode-markup-indent-offset . 2)
           (web-mode-script-padding . 2))
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
  :custom (js-indent-level . 2))

(leaf csv-mode
  :ensure t
  :init
  (modify-coding-system-alist 'file "\\.csv\\'" 'cp932-dos))

(leaf add-node-modules-path
  :ensure t
  :hook (typescript-mode-hook . add-node-modules-path))

(leaf magit
  :ensure t
  :custom ((magit-diff-highlight-hunk-body . nil)
           (magit-log-margin . '(t "%Y-%m-%d %H:%M:%S " magit-log-margin-width t 18))
           (magit-section-initial-visibility-alist . '((unpushed . show) (stashes . show))))
  :custom-face
  (magit-diff-added . '((t (:foreground "green"))))
  (magit-diff-added-highlight . '((t (:foreground "green"))))
  (magit-diff-file-header . '((t (:foreground "yellow"))))
  (magit-diff-removed . '((t (:foreground "red"))))
  (magit-diff-removed-highlight . '((t (:foreground "red"))))
  (magit-hash . '((t (:foreground "gold"))))
  (magit-item-highlight . '((t (:background "gray5")))))

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
                "--plus-emph-style" "syntax #006b6b"))
           (magit-delta-hide-plus-minus-markers . nil))
  :hook magit-mode-hook)

(leaf docker
  :ensure t
  :config
  (leaf dockerfile-mode :ensure t))

(leaf yaml-mode :ensure t)

(leaf electric-pair-local-mode
  :hook emacs-lisp-mode-hook)

(leaf nxml-mode
  :custom (nxml-child-indent . 4))
