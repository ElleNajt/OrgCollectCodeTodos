;;; org-collect-code-todos.el --- Collect TODOs from source code into org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: org-mode, todo
;; Version: 0.1

;;; Commentary:

;; This package collects TODO comments from source code files,
;; organizes them in a structured org-mode file, and synchronizes
;; state changes between the org file and source code.

;;; Code:

(require 'org)
(require 'org-id)

;; Debugging function
(defun org-collect-code-todos--debug (message &rest args)
  "Write debug MESSAGE with ARGS to debug log file."
  (with-temp-buffer
    (insert (format "%s: " (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (apply #'format message args))
    (insert "\n")
    (append-to-file (point-min) (point-max) ".aider-debug-logs")))

(defun org-collect-code-todos--generate-uuid ()
  "Generate a unique ID for a TODO item."
  (org-collect-code-todos--debug "Generating new UUID")
  (let ((id (org-id-new)))
    (org-collect-code-todos--debug "Generated UUID: %s" id)
    id))

(defun org-collect-code-todos--get-comment-prefix ()
  "Get the comment prefix for the current buffer's major mode."
  (org-collect-code-todos--debug "Getting comment prefix for mode: %s" major-mode)
  (let ((prefix (cond
                 ((derived-mode-p 'emacs-lisp-mode) ";;")
                 ((derived-mode-p 'python-mode) "#")
                 ((derived-mode-p 'c-mode 'c++-mode 'java-mode 'js-mode) "//")
                 ((derived-mode-p 'css-mode) "/*")
                 ((derived-mode-p 'html-mode) "<!--")
                 (t (comment-start)))))
    (org-collect-code-todos--debug "Comment prefix: %s" prefix)
    prefix))

(defun org-collect-code-todos--org-to-source (org-heading org-properties)
  "Convert ORG-HEADING and ORG-PROPERTIES to source code TODO format.
Returns a list of strings, one for each line of the TODO comment."
  (org-collect-code-todos--debug "Converting org to source: %s" org-heading)
  (let* ((todo-id (cdr (assoc "TODO_ID" org-properties)))
         (todo-state (if (string-match "^\\(TODO\\|DONE\\) " org-heading)
                         (match-string 1 org-heading)
                       "TODO"))
         (todo-text (replace-regexp-in-string "^\\(TODO\\|DONE\\) " "" org-heading))
         (scheduled (cdr (assoc "SCHEDULED" org-properties)))
         (deadline (cdr (assoc "DEADLINE" org-properties)))
         (comment-prefix (org-collect-code-todos--get-comment-prefix))
         (result (list (format "%s %s[%s] %s" 
                              comment-prefix 
                              todo-state 
                              todo-id 
                              todo-text))))
    
    (when scheduled
      (push (format "%s SCHEDULED: %s" comment-prefix scheduled) (cdr result)))
    
    (when deadline
      (push (format "%s DEADLINE: %s" comment-prefix deadline) (cdr result)))
    
    (org-collect-code-todos--debug "Converted to source format: %s" result)
    result))

(defun org-collect-code-todos--source-to-org (todo-line &optional following-lines)
  "Convert source code TODO-LINE and optional FOLLOWING-LINES to org format.
Returns a cons cell with (heading . properties-alist)."
  (org-collect-code-todos--debug "Converting source to org: %s" todo-line)
  (let ((properties nil)
        heading
        todo-id
        scheduled
        deadline)
    
    ;; Extract TODO ID and text from the main line
    (when (string-match "\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\] \\(.*\\)" todo-line)
      (setq todo-id (match-string 2 todo-line))
      (setq heading (format "%s %s" 
                           (match-string 1 todo-line)
                           (match-string 3 todo-line)))
      (push (cons "TODO_ID" todo-id) properties))
    
    ;; Process following lines for scheduling info
    (when following-lines
      (dolist (line following-lines)
        (cond
         ((string-match "SCHEDULED: \\(<[^>]+>\\)" line)
          (setq scheduled (match-string 1 line))
          (push (cons "SCHEDULED" scheduled) properties))
         ((string-match "DEADLINE: \\(<[^>]+>\\)" line)
          (setq deadline (match-string 1 line))
          (push (cons "DEADLINE" deadline) properties)))))
    
    (org-collect-code-todos--debug "Converted to org format: %s, %s" heading properties)
    (cons heading properties)))

(defun org-collect-code-todos--with-writable-buffer (buffer-or-name fn)
  "Execute FN with BUFFER-OR-NAME temporarily writable."
  (org-collect-code-todos--debug "Making buffer writable: %s" buffer-or-name)
  (with-current-buffer buffer-or-name
    (let ((inhibit-read-only t))
      (funcall fn))))

(defun org-collect-code-todos--extract-todo-info (line-start line-end)
  "Extract TODO information from region between LINE-START and LINE-END.
Returns a list of (todo-line following-lines) or nil if no TODO found."
  (org-collect-code-todos--debug "Extracting TODO info from lines %d-%d" line-start line-end)
  (let ((lines (split-string (buffer-substring-no-properties line-start line-end) "\n"))
        todo-line following-lines)
    (when lines
      (setq todo-line (car lines))
      (when (and todo-line (string-match "\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\]" todo-line))
        (setq following-lines (cdr lines))
        ;; Filter following lines to only include those with scheduling info
        (setq following-lines 
              (seq-filter (lambda (line) 
                            (or (string-match "SCHEDULED:" line)
                                (string-match "DEADLINE:" line)))
                          following-lines))
        (org-collect-code-todos--debug "Found TODO: %s" todo-line)
        (org-collect-code-todos--debug "Following lines: %s" following-lines)
        (list todo-line following-lines)))))

(defun org-collect-code-todos--collect-todos-in-buffer ()
  "Collect all TODOs in the current buffer.
Returns a list of (todo-line following-lines) for each TODO found.
Only works in programming modes."
  (org-collect-code-todos--debug "Collecting TODOs in buffer: %s" (buffer-name))
  (if (not (derived-mode-p 'prog-mode))
      (progn
        (org-collect-code-todos--debug "Buffer is not in prog-mode, skipping")
        nil)
    (let ((todos nil)
          (comment-prefix (org-collect-code-todos--get-comment-prefix))
          (case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\]") nil t)
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (next-line-start (1+ line-end))
                 (following-lines-end line-end))
            
            ;; Look for following comment lines with scheduling info
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp))
                          (looking-at (concat comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                          (not (looking-at (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
                (setq following-lines-end (line-end-position))
                (forward-line 1)))
            
            (let ((todo-info (org-collect-code-todos--extract-todo-info 
                              line-start following-lines-end)))
              (when todo-info
                (push todo-info todos))))))
      
      (setq todos (nreverse todos))
      (org-collect-code-todos--debug "Found %d TODOs in buffer" (length todos))
      todos)))

(provide 'org-collect-code-todos)
;;; org-collect-code-todos.el ends here
