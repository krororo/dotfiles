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
 :category "command")

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

(provide 'my-gptel-tools)
