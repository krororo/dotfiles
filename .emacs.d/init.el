(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; load proxy setting
;; example
;;  (setq url-proxy-services '(("http" . "proxy.example.com:8080")
;;                            ("https" . "proxy.example.com:8080")))
;;  (setq url-http-proxy-basic-auth-storage
;;        (list (list "proxy.example.com:8080"
;;                    (cons "description"
;;                          (base64-encode-string "username:password")))))
(when (file-exists-p (expand-file-name "~/.emacs.d/proxy-setting.el"))
  (load-file (expand-file-name "~/.emacs.d/proxy-setting.el")))

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

(add-to-list 'load-path (locate-user-emacs-file "elisp"))

(leaf mb-url
  :el-get t
  :config
  (advice-add 'url-http :around 'mb-url-http-around-advice)
  (setq mb-url-http-backend 'mb-url-http-curl))

(leaf *initialize-emacs
  :config
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file)
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
  ;; 不要な行末の空白を表示
  (setq-default show-trailing-whitespace t)
  ;; 対応する括弧をハイライト
  (show-paren-mode t)
  ;; スクロールバーを右に
  (set-scroll-bar-mode 'right)
  ;; bell をならなくする
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  (setq compilation-scroll-output t)

  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; dired バッファに [dir] 追加
  (defun dired-my-append-buffer-name-hint ()
    "Append a auxiliary string to a name of dired buffer."
    (when (eq major-mode 'dired-mode)
      (rename-buffer (concat (buffer-name) " [dir]") t)))
  (add-hook 'dired-mode-hook 'dired-my-append-buffer-name-hint)

  (leaf eaw
    :el-get (hamano/locale-eaw
             :name eaw)
    :require t
    :config (eaw-fullwidth))

  (leaf whitespace
    :doc "tab に色を付ける"
    :require t
    :config
    (setq whitespace-style '(face tabs tab-mark))
    (setq whitespace-display-mappings
          '((tab-mark   ?\t   [?\xBB ?\t])))
    (set-face-foreground 'whitespace-tab "royal blue")
    (set-face-background 'whitespace-tab 'nil)
    (set-face-underline  'whitespace-tab t)
    (global-whitespace-mode 1))

  (leaf editorconfig
    :require t
    :el-get t
    :config
    (editorconfig-mode t)
    (defun editorconfig-disable-trim-whitespace-in-read-only-buffers (props)
      (when (and buffer-read-only (gethash 'trim_trailing_whitespace props))
        (remove-hook 'write-file-functions #'delete-trailing-whitespace :local)))
    (add-hook 'editorconfig-custom-hooks #'editorconfig-disable-trim-whitespace-in-read-only-buffers)
    ;; delete-trailing-whitespace モードの状態表示と反転
    (defvar my/current-cleanup-state "")
    (setq-default mode-line-format
                  (cons '(:eval my/current-cleanup-state)
                        mode-line-format))
    (defun toggle-cleanup-spaces ()
      (interactive)
      (cond ((memq 'delete-trailing-whitespace write-file-hooks)
             (setq my/current-cleanup-state
                   (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
             (remove-hook 'write-file-hooks 'delete-trailing-whitespace))
            (t
             (setq my/current-cleanup-state "")
             (add-hook 'write-file-hooks 'delete-trailing-whitespace)))
      (force-mode-line-update)))

  (leaf git-gutter
    :el-get t
    :config (global-git-gutter-mode t))

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
  :config
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-c m") 'magit-status)
  (global-set-key (kbd "C-c u") 'comment-region)
  (global-set-key (kbd "C-c y") 'uncomment-region)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  ;; ウィンドウ逆移動
  (global-set-key (kbd "C-x p") (lambda()(interactive)(other-window -1)))
  ;; 暴発するので無効化
  (global-unset-key (kbd "C-x C-p"))
  ;; compose-mail
  (global-unset-key (kbd "C-x m")))

(leaf *mode-line
  :config
  (setq-default mode-line-format
                '("%e"
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
      (kotlin-mode . "Kt")
      (markdown-mode . "Md")
      (ruby-mode   . "Rb")
      (typescript-mode . "Ts")))

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
  (defvar my-recentf-list-prev nil)

  (defadvice recentf-save-list
      (around no-message activate)
    "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
    (unless (equal recentf-list my-recentf-list-prev)
      (cl-flet ((message (format-string &rest args)
                         (eval `(format ,format-string ,@args)))
                (write-file (file &optional confirm)
                            (let ((str (buffer-string)))
                              (with-temp-file file
                                (insert str)))))
        ad-do-it
        (setq my-recentf-list-prev recentf-list))))

  (defadvice recentf-cleanup
      (around no-message activate)
    "suppress the output from `message' to minibuffer"
    (cl-flet ((message (format-string &rest args)
                       (eval `(format ,format-string ,@args))))
      ad-do-it))
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

(leaf anzu
  :el-get t
  :config
  (global-anzu-mode +1)

  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)

  (global-set-key (kbd "C-c q") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-c Q") 'anzu-query-replace-at-cursor-thing))

(leaf auto-complete
  :el-get t
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
  :el-get t
  :require helm-config
  :config
  (helm-mode 1)
  (helm-descbinds-mode 1)
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
    :el-get t)
  (leaf helm-descbinds
    :el-get t)
  (leaf helm-ls-git
    :el-get t))

(leaf migemo
  :if (executable-find "cmigemo")
  :el-get t
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

;; misc
(el-get-bundle ag)
(el-get-bundle color-moccur)
(el-get-bundle dash)
(el-get-bundle docker)
(el-get-bundle exec-path-from-shell)
(el-get-bundle highlight-indentation-guides)
(el-get-bundle init-loader)
(el-get-bundle moccur-edit)
(el-get-bundle yasnippet)

;; magit
(el-get-bundle magit)

;; progmode
(el-get-bundle Groovy-Emacs-Modes/groovy-emacs-modes)
(el-get-bundle dockerfile-mode)
(el-get-bundle feature-mode)
(el-get-bundle haml-mode)
(el-get-bundle jrblevin/markdown-mode)
(el-get-bundle js2-mode)
(el-get-bundle kotlin-mode)
(el-get-bundle review-mode)
(el-get-bundle rspec-mode)
(el-get-bundle typescript-mode)
(el-get-bundle vue-mode)
(el-get-bundle web-mode)
(el-get-bundle yaml-mode)
(el-get-bundle yard-mode)
(el-get-bundle emacs-pug-mode)
(el-get-bundle php-mode)

;; lint check
(el-get-bundle flycheck)
(el-get-bundle rubocop)

;; el-get-lock
(el-get-bundle tarao/el-get-lock)
(el-get-lock)

(let ((envs '("PATH")))
  (exec-path-from-shell-copy-envs envs))

;; eruby-mode
(load-file (locate-user-emacs-file "elisp/eruby-mode.el"))

;; init-loader
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
