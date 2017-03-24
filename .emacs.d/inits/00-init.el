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

(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)
(setq recentf-mode t)
(require 'recentf)
(setq compilation-scroll-output t)
