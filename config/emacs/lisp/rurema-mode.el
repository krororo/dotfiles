;;; rurema-mode.el --- Major mode for rurema documentation -*- lexical-binding: t; -*-

;; Author: krororo
;; Version: 0.0.1
;; License: MIT

(defvar rurema-selective-display-ellipses t
  "*Displays ellipses in Rurema-mode if non-nil")

(defvar rurema-mode-hook nil
  "Hooks run when entering `rurema-mode' major mode")

;;;###autoload
(define-derived-mode rurema-mode text-mode "Rurema"
  "Major mode for Rurema editing.
\\{rurema-mode-map}"
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "=+\\|\\++\\|[ \t\n\^L]*$")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "=+\\|\\++\\|[ \t\n\^L]")
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((rurema-font-lock-keywords) t nil))
  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords rurema-font-lock-keywords)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "^\\(=+\\)")
  (outline-minor-mode t)
  (add-hook (make-local-variable 'write-contents-hooks) 'rurema-strip-cr-on-top)
  (add-hook (make-local-variable 'after-save-hook) 'rurema-rehide-endline)
  (rurema-hide-other-block-all)
  (setq indent-tabs-mode nil)
  (setq imenu-create-index-function 'rurema-imenu-create-index)
  (run-hooks 'rurema-mode-hook))

(defvar rurema-heading1-face 'font-lock-keyword-face)
(defvar rurema-heading2-face 'font-lock-type-face)
(defvar rurema-heading3-face 'font-lock-variable-name-face)
(defvar rurema-heading4-face 'font-lock-comment-face)
(defvar rurema-emphasis-face 'font-lock-function-name-face)
(defvar rurema-keyboard-face 'font-lock-function-name-face)
(defvar rurema-variable-face 'font-lock-function-name-face)
(defvar rurema-verbatim-face 'font-lock-function-name-face)
(defvar rurema-term-face 'font-lock-function-name-face)
(defvar rurema-footnote-face 'font-lock-function-name-face)
(defvar rurema-link-face 'font-lock-function-name-face)
(defvar rurema-code-face 'font-lock-function-name-face)
(defvar rurema-description-face 'font-lock-constant-face)
(defvar rurema-method-list-face 'font-lock-function-name-face)
(defvar rurema-todo-face 'font-lock-warning-face)

(defvar rurema-font-lock-keywords
  (list
   '("^= .*$"
     0 rurema-heading1-face)
   '("^== .*$"
     0 rurema-heading2-face)
   '("^=== .*$"
     0 rurema-heading3-face)
   '("^=====* .*$"
     0 rurema-heading4-face)
   '("((\\*[^*]*\\*+\\([^)*][^%]*\\*+\\)*))"    ; ((* ... *))
     0 rurema-emphasis-face)
   '("((%[^%]*%+\\([^)%][^%]*%+\\)*))"      ; ((% ... %))
     0 rurema-keyboard-face)
   '("((|[^|]*|+\\([^)|][^|]*|+\\)*))"      ; ((| ... |))
     0 rurema-variable-face)
   '("(('[^']*'+\\([^)'][^']*'+\\)*))"      ; ((' ... '))
     0 rurema-verbatim-face)
   '("((:[^:]*:+\\([^):][^:]*:+\\)*))"      ; ((: ... :))
     0 rurema-term-face)
   '("((-[^-]*-+\\([^)-][^-]*-+\\)*))"      ; ((- ... -))
     0 rurema-footnote-face)
   '("((<[^>]*>+\\([^)>][^>]*>+\\)*))"      ; ((< ... >))
     0 rurema-link-face)
   '("(({[^}]*}+\\([^)}][^}]*}+\\)*))"      ; (({ ... }))
     0 rurema-code-face)
   '("^:.*$"
     0 rurema-description-face)

   ;; http://redmine.ruby-lang.org/wiki/rurema/ReferenceManualFormatDigest
   ;; リンク
   `(,(concat
       "\\[\\["
       "\\(?:"
       ;; クラス [[c:String]]、[[c:File::Stat]] など
       ;; 定数 [[m:File::SEPARATOR]] など
       "[cm]:[A-Z][A-Za-z_0-9:]*"
       "\\|"
       ;; クラスメソッド [[m:String.new]]
       ;; モジュール関数 [[m:Math.#sin]] (「.#」なのに注意)
       "m:[A-Z][A-Za-z_:]*\\.#?\\(\\[\\] \\|[^][ ]+\\)"
       "\\|"
       ;; インスタンスメソッド
       ;; [[m:String#dump]]、![[m:String#[] ]]など ([]の場合のみ空白必須なのに注意)
       "m:[A-Z][A-Za-z_0-9:]*#\\(\\[\\] \\|[^][ ]+\\)"
       "\\|"
       ;; グローバル変数 [[m:$~]] など
       "m:\\$.+"
       "\\|"
       ;; ライブラリ [[lib:jcode]] など
       "lib:[^][ ]+"
       "\\|"
       ;; ruby-list [[ruby-list:12345]] など
       "ruby-\\(list\\|dev\\|ext\\|talk\\|core\\):[0-9]+"
       "\\|"
       ;; https://bugs.ruby-lang.org/issues/12345 [[feature:12345]]
       "\\|"
       "\\(feature\\|bug\\):[0-9]+"
       "\\|"
       ;; man [[man:tr(1)]] など
       "man:[^()]+([0-9])"
       "\\|"
       ;; RFC [[RFC:2822]] など
       "\\(RFC\\|rfc\\):[0-9]+"
       "\\|"
       ;; URL [[url:http://i.loveruby.net]]
       "url:[^][ ]+"
       "\\)"
       "\\]\\]")
     0 rurema-link-face)
   ;; クラス、モジュールの説明の見出し(typoがあれば色でわかる)
   `(,(concat
       "^== \\("
       (regexp-opt
        '("Class Methods"
          "Singleton Methods"
          "Private Singleton Methods"
          "Protected Singleton Methods"
          "Instance Methods"
          "Private Instance Methods"
          "Protected Instance Methods"
          "Module Functions"
          "Constants"
          "Special Variables"))
       "\\)$")
     (0 rurema-heading2-face)
     (1 font-lock-builtin-face t))
   ;; クラス、モジュールの冒頭、ファイル全体の冒頭
   `(,(concat
       "^\\("
       (regexp-opt
        '("alias"
          "extend"
          "include"
          "require"))
       "\\)\\s +\\([A-Za-z0-9_:]+\\)")
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face))
   ;; メソッドの引数の情報、発生する例外
   `(,(concat
       "^\\(@"
       (regexp-opt
        '("param"
          "raise"))
       "\\)\\s +\\([A-Za-z0-9_:]+\\)")
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face))
   ;; 返り値の情報、他に参照すべきメソッドなど
   `(,(concat
       "^@"
       (regexp-opt
        '("return"
          "see")))
     0 font-lock-builtin-face)
   ;; メソッドシグネチャ
   '("^--- .*$" 0 rurema-method-list-face)
   ;; BitClust のコメント
   '("^#@#.*$" 0 font-lock-comment-face)
   ;; BitClust への命令
   `(,(concat
       "^#@"
       (regexp-opt
        '("include"
          "samplecode"
          "since"
          "end"
          "else"
          "until"
          "if"))
       ".*$")
     0 font-lock-keyword-face)
   ;; 書きかけの印
   '("^#@todo.*$" 0 rurema-todo-face)
   ))

(defun rurema-strip-cr-on-top ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((mod (buffer-modified-p)))
      (while (re-search-forward "^\r=end\\>" nil t)
        (beginning-of-line)
        (delete-char 1)
        (forward-line))
      (set-buffer-modified-p mod)))
  nil)

(defun rurema-rehide-endline ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (let ((mod (buffer-modified-p)))
      (while (re-search-forward "^=end\\>.*\r" nil t)
        (beginning-of-line)
        (insert "\r")
        (forward-line))
      (set-buffer-modified-p mod))))

(defun rurema-hide-other-block ()
  "Hides following lines not in Rurema format."
  (interactive)
  (let (end (mod (buffer-modified-p)))
    (save-excursion
      (widen)
      (and (setq end (re-search-forward "^=begin\\>" nil t))
           (re-search-backward "^=end\\>" nil t))
      (insert "\r")
      (while (search-forward "\n" end t)
        (replace-match "\r" t t)))
    (set-buffer-modified-p mod))
  (setq selective-display t
        selective-display-ellipses rurema-selective-display-ellipses))

(defun rurema-hide-other-block-all ()
  "Hides all lines not in Rurema format."
  (interactive)
  (let (beg end (mod (buffer-modified-p)))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (while (and (re-search-forward "^=end\\>" nil t)
                  (setq beg (progn (beginning-of-line) (point)))
                  (setq end (re-search-forward "^=begin\\>" nil t)))
        (goto-char beg)
        (insert "\r")
        (while (search-forward "\n" end t)
          (replace-match "\r" t t))))
    (set-buffer-modified-p mod))
  (setq selective-display t
        selective-display-ellipses rurema-selective-display-ellipses))

(defun rurema-show-other-block ()
  "Shows lines not in Rurema format before current point."
  (interactive)
  (if selective-display
      (save-excursion
        (let (end (mod (buffer-modified-p)))
          (widen)
          (if (re-search-forward "^\r=end\\>" nil t)
              (progn
                (end-of-line)
                (setq end (point))
                (beginning-of-line)
                (delete-char 1)
                (while (search-forward "\r" end t)
                  (replace-match "\n" t t))))
          (set-buffer-modified-p mod)))))

(defun rurema-show-other-block-all ()
  "Shows all lines not in Rurema format."
  (interactive)
  (if selective-display
      (save-excursion
        (let (end (mod (buffer-modified-p)))
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^\r=end\\>" nil t)
            (end-of-line)
            (setq end (point))
            (beginning-of-line)
            (delete-char 1)
            (while (search-forward "\r" end t)
              (replace-match "\n" t t)))
          (set-buffer-modified-p mod))))
  (setq selective-display nil selective-display-ellipses t))

(defun rurema-imenu-create-index ()
  (let ((root '(nil . nil))
        cur-alist
        (cur-level 0)
        (pattern "^\\(=+\\|---\\)[ \t\v\f]*\\(.*?\\)[ \t\v\f]*$")
        (empty-heading "-")
        (self-heading ".")
        pos level heading alist)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern (point-max) t)
        (setq heading (match-string-no-properties 2)
              level (min 6 (length (match-string-no-properties 1)))
              pos (match-beginning 1))
        (if (= (length heading) 0)
            (setq heading empty-heading))
        (setq alist (list (cons heading pos)))
        (cond
         ((= cur-level level) ; new sibling
          (setcdr cur-alist alist)
          (setq cur-alist alist))
         ((< cur-level level) ; first child
          (dotimes (i (- level cur-level 1))
            (setq alist (list (cons empty-heading alist))))
          (if cur-alist
              (let* ((parent (car cur-alist))
                     (self-pos (cdr parent)))
                (setcdr parent (cons (cons self-heading self-pos) alist)))
            (setcdr root alist)) ; primogenitor
          (setq cur-alist alist
                cur-level level))
         (t ; new sibling of an ancestor
          (let ((sibling-alist (last (cdr root))))
            (dotimes (i (1- level))
              (setq sibling-alist (last (cdar sibling-alist))))
            (setcdr sibling-alist alist)
            (setq cur-alist alist
                  cur-level level))))))
    (cdr root)))

(provide 'rurema-mode)

;;; rurema-mode.el ends here
