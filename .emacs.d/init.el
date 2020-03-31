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
(leaf eaw
  :require t
  :config (eaw-fullwidth))

;; mb-url
(el-get-bundle mb-url)
(advice-add 'url-http :around 'mb-url-http-around-advice)
(setq mb-url-http-backend 'mb-url-http-curl)

;; misc
(el-get-bundle ag)
(el-get-bundle anzu)
(el-get-bundle auto-complete)
(el-get-bundle color-moccur)
(el-get-bundle dash)
(el-get-bundle docker)
(el-get-bundle editorconfig)
(el-get-bundle exec-path-from-shell)
(el-get-bundle git-gutter)
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
