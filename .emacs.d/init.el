(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(prog1 "leaf"
  (prog1 "install leaf"
    (custom-set-variables
     '(package-archives '(("melpa" . "https://melpa.org/packages/"))))
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

(leaf package-utils :ensure t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf *initialize-emacs
  :custom ((initial-buffer-choice . "~/memo.md")
           (scroll-conservatively . 1)
           (scroll-margin . 5)
           (scroll-preserve-screen-position . nil)
           (compilation-scroll-output . t))
  :custom-face
  (default . '((t (:background "#003300" :foreground "white" :height 100 :foundry "PfEd" :family "HackGen"))))
  (font-lock-comment-face . '((t (:foreground "gray"))))
  (font-lock-keyword-face . '((t (:foreground "magenta"))))
  :config
  (setq-default indent-tabs-mode nil)
  ;; *.~ とかのバックアップファイルを作らない
  (setq make-backup-files nil)
  ;; .#* とかのバックアップファイルを作らない
  (setq auto-save-default nil)
  ;; yes or no を y or n に
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq save-place-file "~/.emacs.d/.emacs-places")
  ;; menu bar
  (if window-system (menu-bar-mode 1) (menu-bar-mode -1))
  ;; tool bar
  (tool-bar-mode -1)
  ;; モード行に桁数も表示
  (column-number-mode t)
  ;; 起動時のメッセージをでなくする
  (setq inhibit-startup-message t)
  ;; 対応する括弧をハイライト
  (show-paren-mode t)
  ;; スクロールバー無し
  (set-scroll-bar-mode nil)
  ;; bell をならなくする
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  (setq nxml-child-indent 4)

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; スクリーンの最大化
  (set-frame-parameter nil 'fullscreen 'maximized)

  (leaf eaw
    :el-get (hamano/locale-eaw
             :name eaw)
    :require t
    :config (eaw-fullwidth))

  (leaf whitespace
    :doc "tab に色を付ける"
    :require t
    :hook ((prog-mode-hook text-mode-hook) . (lambda () (setq show-trailing-whitespace t)))
    :config
    (setq whitespace-style '(face tabs tab-mark))
    (setq whitespace-display-mappings
          '((tab-mark   ?\t   [?\xBB ?\t])))
    (set-face-foreground 'whitespace-tab "royal blue")
    (set-face-background 'whitespace-tab 'nil)
    (set-face-underline  'whitespace-tab t)
    (global-whitespace-mode 1))

  (leaf editorconfig
    :ensure t
    :custom ((editorconfig-exclude-modes . '(web-mode)))
    :config
    (editorconfig-mode t)
    (defun editorconfig-disable-trim-whitespace-in-read-only-buffers (props)
      (when (and buffer-read-only (gethash 'trim_trailing_whitespace props))
        (remove-hook 'write-file-functions #'delete-trailing-whitespace :local)))
    (add-hook 'editorconfig-custom-hooks #'editorconfig-disable-trim-whitespace-in-read-only-buffers)
    ;; delete-trailing-whitespace モードの状態表示と反転
    (defvar my/current-cleanup-state "")
    (defun toggle-cleanup-spaces ()
      (interactive)
      (cond ((memq 'delete-trailing-whitespace write-file-functions)
             (setq my/current-cleanup-state
                   (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
             (remove-hook 'write-file-functions 'delete-trailing-whitespace))
            (t
             (setq my/current-cleanup-state "")
             (add-hook 'write-file-functions 'delete-trailing-whitespace)))
      (force-mode-line-update)))

  (leaf git-gutter
    :ensure t
    :config (global-git-gutter-mode t))

  (leaf avy
    :ensure t
    :bind (("C-M-:" . avy-goto-char-timer)
           ("M-g M-g" . avy-goto-line)))

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
            (my-make-scratch 1)))))))

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
         ("C-c m" . magit-status)
         ("C-c u" . comment-region)
         ("C-c y" . uncomment-region)
         ("C-c r" . revert-buffer)
         ;; ウィンドウ逆移動
         ("C-x p" . my-reverse-other-window)
         ;; 暴発するので無効化
         ("C-x C-p" . nil)
         ;; compose-mail
         ("C-x m" . nil)))

(leaf *mode-line
  :config
  (setq-default mode-line-format
                '((:eval my/current-cleanup-state)
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
      (magit-delta-mode . " Δ")
      ;; Major modes
      (emacs-lisp-mode . "El")
      (markdown-mode . "Md")
      (ruby-mode   . "Rb")
      (typescript-mode . "Ts")
      (js2-mode . "Js")))

  (defun clean-mode-line ()
    (interactive)
    (loop for (mode . mode-str) in mode-line-cleaner-alist
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
  :init (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
  :require t
  :config
  (setq mozc-candidate-style 'echo-area)
  (set-cursor-color "red")
  (set-language-environment 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (setq default-input-method "japanese-mozc")
  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
  ;; 変換キーでon
  (global-set-key (kbd "<henkan>")
                  (lambda () (interactive)
                    (when (null current-input-method) (toggle-input-method))))
  ;; 無変換キーでon
  (global-set-key (kbd "<muhenkan>")
                  (lambda () (interactive)
                    (inactivate-input-method)))
  ;; 全角半角キーと無変換キーのキーイベントを横取りする
  (defadvice mozc-handle-event (around intercept-keys (event))
    "Intercept keys muhenkan and zenkaku-hankaku, before passing keys
to mozc-server (which the function mozc-handle-event does), to
properly disable mozc-mode."
    (if (member event (list 'zenkaku-hankaku 'muhenkan))
        (progn
          (mozc-clean-up-session)
          (toggle-input-method))
      (progn ;(message "%s" event) ;debug
        ad-do-it)))
  (ad-activate 'mozc-handle-event)
  (add-hook 'input-method-activate-hook
            '(lambda () (set-cursor-color "green")))
  (add-hook 'input-method-inactivate-hook
            '(lambda () (set-cursor-color "red"))))

(leaf recentf
  :require t
  :after cl-lib
  :config
  (defun my-recentf-save-list-inhibit-message:around (orig-func &rest args)
    "recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
(*Messages* バッファには出力される)"
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  (advice-add 'recentf-cleanup   :around 'my-recentf-save-list-inhibit-message:around)
  (advice-add 'recentf-save-list :around 'my-recentf-save-list-inhibit-message:around)

  (setq recentf-max-menu-items 500)
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("recentf" "COMMIT_EDITMSG"))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

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

(leaf anzu
  :ensure t
  :config
  (global-anzu-mode +1)

  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)

  (global-set-key (kbd "C-c q") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-c Q") 'anzu-query-replace-at-cursor-thing))

(leaf auto-complete
  :ensure t
  :require auto-complete-config
  :config
  (ac-config-default)
  (defun ac-common-setup ()
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (add-to-list 'ac-modes 'haml-mode)
  (add-to-list 'ac-modes 'vue-mode)
  (add-to-list 'ac-modes 'typescript-mode))

(leaf helm
  :ensure t
  :require helm-config
  :custom-face
  (helm-source-header . '((t (:background "#22083397778B" :foreground "white" :weight bold))))
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x b") 'helm-buffers-list)
  (define-key global-map (kbd "C-c C-v") 'helm-ls-git-ls)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-;") 'helm-mini)

  ;; リストのソートをしないように
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0)))

  (setq helm-buffer-max-length 40)
  (setq helm-buffer-details-flag nil)

  (leaf helm-ag
    :ensure t)
  (leaf helm-descbinds
    :ensure t
    :config
    (helm-descbinds-mode 1))
  (leaf helm-ls-git
    :ensure t))

(leaf sudo-edit :ensure t)

(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  :require t
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)
  (set-process-query-on-exit-flag (get-process "migemo") nil))

(leaf sh-mode
  :config
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(leaf yasnippet
  :ensure t
  :require t
  :custom ((yas-snippet-dirs . '("~/.emacs.d/snippets")))
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
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
  :require t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-ruby-rubocop-executable . nil)
           (safe-local-variable-values . '((encoding . utf-8)
                                           (eval setq-local flycheck-command-wrapper-function
                                                 (lambda
                                                   (command)
                                                   (append
                                                    (quote
                                                     ("bundle" "exec"))
                                                    command))))))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

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
  :require org-table
  :custom-face
  (markdown-code-face . '((t (:inherit default :foreground "medium aquamarine"))))
  :config
  (add-hook 'markdown-mode-hook 'orgtbl-mode)
  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))
  (add-hook 'markdown-mode-hook
            #'(lambda()
                (add-hook 'before-save-hook 'cleanup-org-tables  nil 'make-it-local))))

(leaf vue-mode
  :ensure t

  :init
  (leaf pug-mode
    :ensure t
    :custom ((pug-tab-width . 2)))

  :custom ((vue-html-extra-indent . 2))
  :custom-face
  (mmm-default-submode-face . '((t nil)))
  :config
  ;; see: https://qiita.com/akicho8/items/58c2ac5d762a2a4479c6
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))

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
  :init
  (leaf rspec-mode :ensure t)
  (leaf rubocop :ensure t)
  (leaf yard-mode :ensure t)

  :mode "\\.\\(ruby\\|plugin\\)\\'"
  :config
  (setq rspec-spec-command "rspec -c")
  (setq rspec-use-rake-when-possible nil)
  (setq ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-hook
            '(lambda()
               (setq flycheck-checker 'ruby-rubocop)
               (electric-indent-local-mode 1)
               (flycheck-mode)
               (yard-mode))))

(leaf css-mode
  :mode "\\.scss\\'"
  :config
  (setq css-indent-offset 2))

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
  :mode "\\.html?\\'" "\\.vm\\'" "\\.jsp\\'"
  :custom ((web-mode-enable-auto-indentation . nil))
  :config
  (setq web-mode-attr-indent-offset 4)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-script-padding 2)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "javascript")
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                (flycheck-mode)))))

(leaf add-node-modules-path
  :ensure t
  :hook (typescript-mode-hook . add-node-modules-path))

(leaf exec-path-from-shell
  :ensure t
  :custom ((exec-path-from-shell-check-startup-files . nil))
  :config
  (exec-path-from-shell-copy-env "PATH"))

(leaf magit
  :ensure t
  :custom ((magit-diff-highlight-hunk-body . nil)
           (magit-log-margin . '(t "%Y-%m-%d %H:%M:%S " magit-log-margin-width t 18))
           (magit-section-initial-visibility-alist . '((unpushed . show) (stashes . hide))))
  :custom-face
  (magit-diff-added . '((t (:foreground "green"))))
  (magit-diff-added-highlight . '((t (:foreground "green"))))
  (magit-diff-file-header . '((t (:foreground "yellow"))))
  (magit-diff-removed . '((t (:foreground "red"))))
  (magit-diff-removed-highlight . '((t (:foreground "red"))))
  (magit-hash . '((t (:foreground "gold"))))
  (magit-item-highlight . '((t (:background "gray5"))))
  :hook (magit-status-mode-hook . magit-delta-mode)
  :config
  (leaf magit-delta
    :ensure t
    :after magit xterm-color
    :custom ((magit-delta-delta-args
              . `("--max-line-distance" "0.6"
                  "--24-bit-color" ,(if xterm-color--support-truecolor "always" "never")
                  "--color-only"
                  "--plus-color" "#003345"
                  "--plus-emph-color" "#006b6b"))
             (magit-delta-hide-plus-minus-markers . nil)))

  (leaf xterm-color :ensure t))

(leaf ag :ensure t)

(leaf docker
  :ensure t
  :config
  (leaf dockerfile-mode :ensure t))

(leaf yaml-mode :ensure t)
