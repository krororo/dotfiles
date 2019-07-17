(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)
(setq-default indent-tabs-mode nil)
;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)
;;; yes or no を y or n に
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
;; tab に色を付ける
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark))
(setq whitespace-display-mappings
      '((tab-mark   ?\t   [?\xBB ?\t])))
(set-face-foreground 'whitespace-tab "royal blue")
(set-face-background 'whitespace-tab 'nil)
(set-face-underline  'whitespace-tab t)
(global-whitespace-mode 1)

(require 'editorconfig)
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
  (force-mode-line-update))

;; don't remove *scratch* buffer
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
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

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

;; highlight-indent-guides
(setq highlight-indent-guides-method 'column)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0.5)

(global-git-gutter-mode t)
