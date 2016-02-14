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

;; misc
(el-get-bundle init-loader)
(el-get-bundle migemo)
(el-get-bundle auto-complete)
(el-get-bundle moccur-edit)
(el-get-bundle color-moccur)
(el-get-bundle dash)
(el-get-bundle yasnippet)
(el-get-bundle anzu)
(el-get-bundle exec-path-from-shell)

;; magit
(el-get-bundle magit)

;; helm
(el-get-bundle helm)
(el-get-bundle helm-ls-git)
(el-get-bundle helm-descbinds)

;; progmode
(el-get-bundle feature-mode)
(el-get-bundle haml-mode)
(el-get-bundle js2-mode)
(el-get-bundle jrblevin/markdown-mode)
(el-get-bundle review-mode)
(el-get-bundle rspec-mode)
(el-get-bundle scss-mode)
(el-get-bundle yaml-mode)
(el-get-bundle yard-mode)
(el-get-bundle web-mode)
(el-get-bundle Groovy-Emacs-Modes/groovy-emacs-modes)

;; flymake
(el-get-bundle flymake-easy)
(el-get-bundle flymake-ruby)

(let ((envs '("PATH")))
  (exec-path-from-shell-copy-envs envs))

;; package
(require 'package)
(fset 'package-desc-vers 'package--ac-desc-version)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; init-loader
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
