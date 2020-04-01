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
  (put 'dired-find-alternate-file 'disabled nil)

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

;; misc
(el-get-bundle ag)
(el-get-bundle anzu)
(el-get-bundle auto-complete)
(el-get-bundle color-moccur)
(el-get-bundle dash)
(el-get-bundle docker)
(el-get-bundle exec-path-from-shell)
(el-get-bundle highlight-indentation-guides)
(el-get-bundle init-loader)
(el-get-bundle migemo)
(el-get-bundle moccur-edit)
(el-get-bundle yasnippet)

;; magit
(el-get-bundle magit)

;; helm
(el-get-bundle helm)
(el-get-bundle helm-ag)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-ls-git)

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

;; mode-line
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

;; init-loader
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
