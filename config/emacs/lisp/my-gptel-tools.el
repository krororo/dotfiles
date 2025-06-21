;;; -*- lexical-binding: t; -*-

;; https://github.com/karthink/gptel/wiki/Tools-collection

(gptel-make-tool
 :function (lambda (pattern &optional include path)
             "Search for PATTERN in files, optionally including INCLUDE and PATH."
             (let* ((default-directory (or path default-directory))
                    (include-arg (if include
                                     (format "--include=\"%s\"" include)
                                   ""))
                    (command (format "grep -r -n -E %s %s ."
                                     (shell-quote-argument pattern)
                                     include-arg))
                    (result (shell-command-to-string command)))
               (if (string-empty-p result)
                   "No matches found"
                 result)))
 :name "grep_tool"
 :description "Content search using regex"
 :args (list
        '( :name "pattern"
           :type string
           :description "Regex pattern to search in file contents")
        '( :name "include"
           :type string
           :optional t
           :description "File pattern to include in search")
        '( :name "path"
           :type string
           :optional t
           :description "Directory to search in"))
 :category "command"
 :confirm t)

(gptel-make-tool
 :function (lambda (url)
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min))
               (forward-paragraph)
               (let ((dom (libxml-parse-html-region (point) (point-max))))
                 (run-at-time 0 nil #'kill-buffer (current-buffer))
                 (with-temp-buffer
                   (shr-insert-document dom)
                   (buffer-substring-no-properties (point-min) (point-max))))))
 :name "read_url"
 :description "Fetch and read the contents of a URL"
 :args (list
        '( :name "url"
           :type string
           :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :name "read_documentation"
 :function
 (lambda (symbol)
   "Read the documentation for SYMBOL, which can be a function or variable."
   (let ((sym (intern symbol)))
     (cond
      ((fboundp sym)
       (documentation sym))
      ((boundp sym)
       (documentation-property sym 'variable-documentation))
      (t
       (format "No documentation found for %s" symbol)))))
 :description "Read the documentation for a given function or variable"
 :args (list
        '( :name "name"
           :type string
           :description "The name of the function or variable whose documentation is to be retrieved"))
 :category "emacs")

(provide 'my-gptel-tools)
