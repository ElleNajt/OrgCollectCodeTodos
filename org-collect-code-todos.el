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

(defun org-collect-code-todos--get-org-file-path ()
  "Get the path to the org file for storing TODOs."
  (expand-file-name "code-todos.org" (or (projectile-project-root) default-directory)))

(defun org-collect-code-todos--ensure-org-file-exists ()
  "Ensure the org file for TODOs exists with proper structure."
  (let ((file-path (org-collect-code-todos--get-org-file-path)))
    (unless (file-exists-p file-path)
      (org-collect-code-todos--debug "Creating new org file at %s" file-path)
      (with-temp-file file-path
        (insert "#+TITLE: Code TODOs\n")
        (insert "#+STARTUP: overview\n")
        (insert "#+TODO: TODO DONE\n\n")
        (insert "* Code TODOs\n")))
    file-path))

(defun org-collect-code-todos--find-or-create-heading (file heading-path)
  "In FILE, find or create heading at HEADING-PATH.
HEADING-PATH is a list of headings, from parent to child.
Returns the point at the end of the heading line."
  (org-collect-code-todos--debug "Finding or creating heading: %s" heading-path)
  (with-current-buffer (find-file-noselect file)
    (org-collect-code-todos--with-writable-buffer (current-buffer)
      (lambda ()
        (goto-char (point-min))
        (let ((current-level 1))
          (dolist (heading heading-path)
            (let ((heading-regexp (format "^\\*\\{%d\\} %s$" current-level (regexp-quote heading))))
              (if (re-search-forward heading-regexp nil t)
                  (progn
                    (org-collect-code-todos--debug "Found existing heading: %s" heading)
                    (end-of-line))
                (progn
                  (org-collect-code-todos--debug "Creating new heading: %s" heading)
                  (if (= current-level 1)
                      (progn
                        (goto-char (point-max))
                        (unless (bolp) (insert "\n")))
                    (org-end-of-subtree t t)
                    (insert "\n"))
                  (insert (make-string current-level ?*) " " heading)
                  (end-of-line))))
            (setq current-level (1+ current-level))))
        (point)))))

(defun org-collect-code-todos--find-todo-by-id (file todo-id)
  "Find a TODO with TODO-ID in FILE.
Returns the point at the beginning of the heading, or nil if not found."
  (org-collect-code-todos--debug "Finding TODO with ID: %s" todo-id)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward org-heading-regexp nil t))
          (let ((properties (org-entry-properties)))
            (when (string= (cdr (assoc "TODO_ID" properties)) todo-id)
              (setq found (match-beginning 0)))))
        found))))

(defun org-collect-code-todos--update-or-create-todo (file file-path todo-info)
  "In FILE, update or create a TODO from TODO-INFO for source at FILE-PATH.
TODO-INFO is (todo-line following-lines)."
  (org-collect-code-todos--debug "Updating or creating TODO from: %s" todo-info)
  (let* ((todo-line (car todo-info))
         (following-lines (cadr todo-info))
         (org-data (org-collect-code-todos--source-to-org todo-line following-lines))
         (heading (car org-data))
         (properties (cdr org-data))
         (todo-id (cdr (assoc "TODO_ID" properties)))
         (scheduled (cdr (assoc "SCHEDULED" properties)))
         (deadline (cdr (assoc "DEADLINE" properties)))
         (todo-point (org-collect-code-todos--find-todo-by-id file todo-id)))
    
    (with-current-buffer (find-file-noselect file)
      (org-collect-code-todos--with-writable-buffer (current-buffer)
        (lambda ()
          (if todo-point
              ;; Update existing TODO
              (progn
                (org-collect-code-todos--debug "Updating existing TODO: %s" heading)
                (goto-char todo-point)
                (org-edit-headline heading)
                (when scheduled
                  (org-schedule nil scheduled))
                (when deadline
                  (org-deadline nil deadline)))
            ;; Create new TODO
            (progn
              (org-collect-code-todos--debug "Creating new TODO: %s" heading)
              ;; Find or create file heading
              (let ((file-heading (file-name-nondirectory file-path)))
                (org-collect-code-todos--find-or-create-heading 
                 file (list "Code TODOs" file-heading))
                (insert "\n")
                (insert "** " heading)
                (org-set-property "TODO_ID" todo-id)
                (org-set-property "FILE" file-path)
                (when scheduled
                  (org-schedule nil scheduled))
                (when deadline
                  (org-deadline nil deadline))))))))))

(defun org-collect-code-todos--update-todos-on-save ()
  "Update TODOs in the org file when saving a source file."
  (when (derived-mode-p 'prog-mode)
    (org-collect-code-todos--debug "Updating TODOs on save for: %s" (buffer-file-name))
    (let ((todos (org-collect-code-todos--collect-todos-in-buffer))
          (file-path (buffer-file-name))
          (org-file (org-collect-code-todos--ensure-org-file-exists)))
      
      (when todos
        (org-collect-code-todos--debug "Found %d TODOs to update" (length todos))
        (dolist (todo-info todos)
          (org-collect-code-todos--update-or-create-todo org-file file-path todo-info))))))

;;;###autoload
(define-minor-mode org-collect-code-todos-mode
  "Minor mode for collecting code TODOs into an org file."
  :lighter " OrgTODO"
  :global t
  (if org-collect-code-todos-mode
      (progn
        (add-hook 'after-save-hook #'org-collect-code-todos--update-todos-on-save)
        (org-collect-code-todos--debug "Enabled org-collect-code-todos-mode"))
    (remove-hook 'after-save-hook #'org-collect-code-todos--update-todos-on-save)
    (org-collect-code-todos--debug "Disabled org-collect-code-todos-mode")))

(provide 'org-collect-code-todos)
;;; org-collect-code-todos.el ends here
