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
  (message (apply #'format message args))
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

(defun org-collect-code-todos--create-todo-with-id (todo-text)
  "Create a new TODO with TODO-TEXT and a generated ID.
Returns the TODO line with ID."
  (org-collect-code-todos--debug "Creating new TODO with text: %s" todo-text)
  (let* ((id (org-collect-code-todos--generate-uuid))
         (comment-prefix (org-collect-code-todos--get-comment-prefix))
         (todo-line (format "%s TODO[%s] %s" comment-prefix id todo-text)))
    (org-collect-code-todos--debug "Created TODO line: %s" todo-line)
    todo-line))

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
        ;; First, look for TODOs with IDs
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
                (push todo-info todos)))))
        
        ;; Then, look for regular TODOs without IDs and assign IDs to them
        (goto-char (point-min))
        (let ((todo-regexp (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)\\s-+\\([^[].*\\)$")))
          (while (re-search-forward todo-regexp nil t)
            (let* ((todo-state (match-string 1))
                   (todo-text (string-trim (match-string 2)))
                   (line-start (line-beginning-position))
                   (line-end (line-end-position))
                   (following-lines-end line-end)
                   (id (org-collect-code-todos--generate-uuid))
                   (new-todo-line (format "%s %s[%s] %s" 
                                          comment-prefix
                                          todo-state
                                          id
                                          todo-text)))
              
              ;; Look for following comment lines with scheduling info
              (save-excursion
                (forward-line 1)
                (while (and (not (eobp))
                            (looking-at (concat comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                            (not (looking-at (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
                  (setq following-lines-end (line-end-position))
                  (forward-line 1)))
              
              ;; Replace the old TODO line with the new one that has an ID
              (org-collect-code-todos--debug "Converting TODO without ID: %s" todo-text)
              (delete-region line-start (1+ line-end))
              (goto-char line-start)
              (insert new-todo-line "\n")
              
              ;; Collect the new TODO
              (let ((todo-info (list new-todo-line 
                                     (split-string 
                                      (buffer-substring-no-properties 
                                       (1+ line-start) following-lines-end) 
                                      "\n"))))
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
                                                            ;; Use org-entry-properties-from-alist instead of org-set-property for FILE
                                                            (org-entry-put (point) "FILE_PATH" file-path)
                                                            (when scheduled
                                                              (org-schedule nil scheduled))
                                                            (when deadline
                                                              (org-deadline nil deadline))))))))))

(defun org-collect-code-todos--find-todo-in-source-file (file-path todo-id)
  "Find a TODO with TODO-ID in FILE-PATH.
Returns a cons cell (point . end-point) or nil if not found."
  (org-collect-code-todos--debug "Finding TODO with ID %s in file %s" todo-id file-path)
  (when (and file-path (file-exists-p file-path))
    (with-current-buffer (find-file-noselect file-path)
      (save-excursion
        (goto-char (point-min))
        (let ((comment-prefix (org-collect-code-todos--get-comment-prefix))
              (case-fold-search nil)
              (todo-regexp (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)\\[" 
                                   (regexp-quote todo-id) "\\]"))
              start end)
          (when (re-search-forward todo-regexp nil t)
            (setq start (line-beginning-position))
            (setq end (line-end-position))
            
            ;; Look for following comment lines with scheduling info
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp))
                          (looking-at (concat comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                          (not (looking-at (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
                (setq end (line-end-position))
                (forward-line 1)))
            
            (cons start (1+ end))))))))

(defun org-collect-code-todos--update-todo-in-source-file (file-path todo-id)
  "Update TODO with TODO-ID in FILE-PATH from current org entry."
  (org-collect-code-todos--debug "Updating TODO %s in file %s" todo-id file-path)
  (when (and file-path (file-exists-p file-path))
    (let* ((heading (org-get-heading t t t t))
           (todo-state (org-get-todo-state))
           (properties (org-entry-properties))
           (scheduled (cdr (assoc "SCHEDULED" properties)))
           (deadline (cdr (assoc "DEADLINE" properties)))
           (org-heading (if todo-state
                            (concat todo-state " " heading)
                          heading))
           (org-props (append
                       (list (cons "TODO_ID" todo-id))
                       (when scheduled (list (cons "SCHEDULED" scheduled)))
                       (when deadline (list (cons "DEADLINE" deadline))))))
      
      (with-current-buffer (find-file-noselect file-path)
        (let (
              (source-lines (org-collect-code-todos--org-to-source org-heading org-props))

              (todo-pos (org-collect-code-todos--find-todo-in-source-file file-path todo-id)))
          (when todo-pos
            (let ((start (car todo-pos))
                  (end (cdr todo-pos)))
              ;; Replace the old TODO with the new one
              (delete-region start end)
              (goto-char start)
              (insert (string-join source-lines "\n") "\n")
              (save-buffer))))))))

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

(defun org-collect-code-todos--sync-todo-to-source ()
  "Sync TODO from org file to source file."
  (org-collect-code-todos--debug "Syncing TODO to source")
  (let* ((todo-id (org-entry-get (point) "TODO_ID"))
         (file-path (org-entry-get (point) "FILE_PATH")))
    
    (when (and todo-id file-path)
      (org-collect-code-todos--debug "Syncing TODO %s to file %s" todo-id file-path)
      (org-collect-code-todos--update-todo-in-source-file file-path todo-id))))

(defun org-collect-code-todos--make-org-file-read-only ()
  "Make the org TODOs file read-only."
  (let ((file-path (org-collect-code-todos--get-org-file-path)))
    (when (and (buffer-file-name) 
               (string= (expand-file-name (buffer-file-name)) 
                        (expand-file-name file-path)))
      (setq buffer-read-only t)
      (org-collect-code-todos--debug "Made org file read-only: %s" file-path))))

(defun org-collect-code-todos--setup-org-hooks ()
  "Set up hooks for org-mode synchronization."
  (org-collect-code-todos--debug "Setting up org hooks")
  (add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
  (add-hook 'org-after-schedule-hook #'org-collect-code-todos--sync-todo-to-source)
  (add-hook 'org-after-deadline-hook #'org-collect-code-todos--sync-todo-to-source)
  (add-hook 'find-file-hook #'org-collect-code-todos--make-org-file-read-only))

(defun org-collect-code-todos--remove-org-hooks ()
  "Remove hooks for org-mode synchronization."
  (org-collect-code-todos--debug "Removing org hooks")
  (remove-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
  (remove-hook 'org-after-schedule-hook #'org-collect-code-todos--sync-todo-to-source)
  (remove-hook 'org-after-deadline-hook #'org-collect-code-todos--sync-todo-to-source)
  (remove-hook 'find-file-hook #'org-collect-code-todos--make-org-file-read-only))

;;;###autoload
(define-minor-mode org-collect-code-todos-mode
  "Minor mode for collecting code TODOs into an org file."
  :lighter " OrgTODO"
  :global t
  (if org-collect-code-todos-mode
      (progn
        (add-hook 'after-save-hook #'org-collect-code-todos--update-todos-on-save)
        (org-collect-code-todos--setup-org-hooks)
        (org-collect-code-todos--debug "Enabled org-collect-code-todos-mode"))
    (remove-hook 'after-save-hook #'org-collect-code-todos--update-todos-on-save)
    (org-collect-code-todos--remove-org-hooks)
    (org-collect-code-todos--debug "Disabled org-collect-code-todos-mode")))

(provide 'org-collect-code-todos)
;;; org-collect-code-todos.el ends here
