;;; -*- lexical-binding: t -*-

(with-eval-after-load 'tool-bar
  (tool-bar-mode -1))

(when (require 'warnings nil t)
  (add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie)))
