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

(gptel-make-tool
 :function (lambda (ref &optional format)
             "Show git object using git show command."
             (let* ((format-arg (if format
                                    (format "--format=%s" format)
                                  ""))
                    (command (format "git show %s %s"
                                     format-arg
                                     (shell-quote-argument ref)))
                    (result (shell-command-to-string command)))
               (if (string-match-p "^fatal:" result)
                   (format "Error: %s" result)
                 result)))
 :name "git_show"
 :description "Show git commits, trees, tags, or files using git show"
 :args (list
        '( :name "ref"
           :type string
           :description "Git reference (commit hash, branch, tag, or file path)")
        '( :name "format"
           :type string
           :optional t
           :description "Format string for commit display (e.g., 'oneline', 'short', 'full')"))
 :category "git")

(gptel-make-tool
 :function (lambda (&optional ref1 ref2 file)
             "Show differences between git references or files."
             (let* ((refs (cond
                          ((and ref1 ref2) (format "%s %s" ref1 ref2))
                          (ref1 ref1)
                          (t "")))
                    (file-arg (if file (shell-quote-argument file) ""))
                    (command (format "git diff %s %s" refs file-arg))
                    (result (shell-command-to-string command)))
               (if (string-match-p "^fatal:" result)
                   (format "Error: %s" result)
                 (if (string-empty-p result)
                     "No differences found"
                   result))))
 :name "git_diff"
 :description "Show differences between commits, branches, or files using git diff"
 :args (list
        '( :name "ref1"
           :type string
           :optional t
           :description "First git reference (commit, branch, tag)")
        '( :name "ref2"
           :type string
           :optional t
           :description "Second git reference (commit, branch, tag)")
        '( :name "file"
           :type string
           :optional t
           :description "Specific file to show diff for"))
 :category "git")

(gptel-make-tool
 :function (lambda (&optional count oneline author since until)
             "Show git commit history using git log."
             (let* ((args (list))
                    (command-parts (list "git" "log")))
               (when count
                 (push (format "-n %s" count) args))
               (when oneline
                 (push "--oneline" args))
               (when author
                 (push (format "--author=%s" (shell-quote-argument author)) args))
               (when since
                 (push (format "--since=%s" (shell-quote-argument since)) args))
               (when until
                 (push (format "--until=%s" (shell-quote-argument until)) args))
               (let* ((command (mapconcat 'identity
                                         (append command-parts (reverse args))
                                         " "))
                      (result (shell-command-to-string command)))
                 (if (string-match-p "^fatal:" result)
                     (format "Error: %s" result)
                   result))))
 :name "git_log"
 :description "Show git commit history using git log"
 :args (list
        '( :name "count"
           :type string
           :optional t
           :description "Number of commits to show")
        '( :name "oneline"
           :type boolean
           :optional t
           :description "Show one line per commit")
        '( :name "author"
           :type string
           :optional t
           :description "Filter commits by author")
        '( :name "since"
           :type string
           :optional t
           :description "Show commits since date (e.g., '2023-01-01')")
        '( :name "until"
           :type string
           :optional t
           :description "Show commits until date (e.g., '2023-12-31')"))
 :category "git")

(gptel-make-tool
 :function (lambda (pattern &optional file-pattern)
             "Search for pattern in git-tracked files using git grep."
             (let* ((file-arg (if file-pattern
                                 (format " -- %s" (shell-quote-argument file-pattern))
                               ""))
                    (command (format "git grep -n -E %s%s"
                                     (shell-quote-argument pattern)
                                     file-arg))
                    (result (shell-command-to-string command)))
               (if (string-match-p "^fatal:" result)
                   (format "Error: %s" result)
                 (if (string-empty-p result)
                     "No matches found"
                   result))))
 :name "git_grep"
 :description "Search for patterns in git-tracked files using git grep"
 :args (list
        '( :name "pattern"
           :type string
           :description "Pattern to search for (supports regex)")
        '( :name "file_pattern"
           :type string
           :optional t
           :description "File pattern to limit search (e.g., '*.py', '*.el')"))
 :category "git")

(provide 'my-gptel-tools)
