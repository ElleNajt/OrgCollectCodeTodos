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

;; Debugging function
(defun org-collect-code-todos--debug (message &rest args)
  "Write debug MESSAGE with ARGS to debug buffer if debugging is enabled."
  (when org-collect-code-todos-debug
    (let ((formatted-message (apply #'format message args))
          (buffer-name "*org-collect-code-todos-debug*"))
      ;; Create or get the debug buffer
      (with-current-buffer (get-buffer-create buffer-name)
        (goto-char (point-max))
        (insert (format "%s: %s\n" 
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        formatted-message))))))

(defcustom org-collect-code-todos-verbose nil
  "Whether to show informational messages during operations.
When non-nil, shows messages about TODO updates and state changes."
  :type 'boolean
  :group 'org-collect-code-todos)

(defun org-collect-code-todos--message (message &rest args)
  "Display MESSAGE with ARGS if verbose mode is enabled.
Always passes the message to the debug function."
  (let ((formatted-message (apply #'format message args)))
    ;; Always log to debug
    (org-collect-code-todos--debug formatted-message)
    ;; Only show in minibuffer if verbose
    (when org-collect-code-todos-verbose
      (message "%s" formatted-message))))


(defun org-collect-code-todos--base62-encode-random (n)
  "Generate a random sequence of alphanumerics"
  (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (mapconcat (lambda (_)
                 (string (aref chars (random 62))))
               (make-string n ?x)
               "")))

(defcustom org-collect-code-todos-id-length 10
  "Length of randomly generated TODO IDs.
Higher values reduce collision probability but take up more real estate:
- 8 chars: ~10^-9 collision prob for 1000 TODOs
- 10 chars: ~10^-12 collision prob (default)
- 16 chars: ~10^-23 collision prob (paranoid)"
  :type 'integer
  :group 'org-collect-code-todos)

(defun org-collect-code-todos--generate-uuid ()
  "Generate a unique ID for a TODO item."
  (org-collect-code-todos--base62-encode-random org-collect-code-todos-id-length))

(defcustom org-collect-code-todos-comment-prefixes
  '((emacs-lisp-mode . ";;")
    (python-mode . "#")
    (c-mode . "//")
    (yaml-mode . "#")
    (dockerfile-mode . "#")
    (gitignore-mode . "#")
    (terraform-mode . "#")
    (conf-mode . "#")
    (markdown-mode . "<!--")
    ;; (markdown-mode . "#") ;; To support org mode TODOs in markdown
    ;; TODO[e5q2Hn059z] find a way to handle multiple prefixes
    (c++-mode . "//")
    (java-mode . "//")
    (js-mode . "//")
    (css-mode . "/*")
    (html-mode . "<!--"))
  "Alist of major modes and their comment prefixes.
If a mode is not listed, falls back to `comment-start'."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-collect-code-todos)

(defun org-collect-code-todos--get-comment-prefix ()
  "Get the comment prefix for the current buffer's major mode."
  (org-collect-code-todos--debug "Getting comment prefix for mode: %s" major-mode)
  (let ((prefix (or (cdr (assq major-mode org-collect-code-todos-comment-prefixes))
                    comment-start)))
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
         ;; Extract text from between the link brackets if present
         (todo-text (if (string-match "\\[\\[file:.*?\\]\\[\\(.*?\\)\\]\\]" org-heading)
                        (match-string 1 org-heading)
                      (replace-regexp-in-string "^[ \t]*\\(TODO\\|DONE\\) " "" org-heading)))
         (_ (org-collect-code-todos--message "todo text: %s" todo-text))
         (scheduled (cdr (assoc "SCHEDULED" org-properties)))
         (deadline (cdr (assoc "DEADLINE" org-properties)))
         (comment-prefix (org-collect-code-todos--get-comment-prefix))
         (result (list (format "%s %s[%s] %s" 
                               comment-prefix
                               todo-state
                               todo-id
                               todo-text))))
    
    (org-collect-code-todos--debug "Org heading: '%s'" org-heading)
    (org-collect-code-todos--debug "Extracted todo state: '%s'" todo-state)
    (org-collect-code-todos--debug "Extracted todo text: '%s'" todo-text)
    (org-collect-code-todos--debug "Constructed source line: '%s'" (car result))
    
    ;; Add scheduling and deadline info on a single line if either exists
    (when (or scheduled deadline)
      (let ((schedule-line (format "%s" comment-prefix)))
        (when scheduled
          (setq schedule-line (concat schedule-line " SCHEDULED: " scheduled)))
        (when deadline
          (setq schedule-line (concat schedule-line 
                                      (if scheduled " " "")
                                      "DEADLINE: " deadline)))
        (push schedule-line (cdr result))))
    
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
        deadline
        (file-path (buffer-file-name)))
    
    ;; Extract TODO ID and text from the main line
    (when (string-match "\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\] \\(.*\\)" todo-line)
      (let ((state (match-string 1 todo-line))
            (id (match-string 2 todo-line))
            (text (match-string 3 todo-line)))
        (setq todo-id id)
        ;; Create heading with file link
        (setq heading (format "%s [[file:%s][%s]]" state file-path text))
        (org-collect-code-todos--debug "Extracted heading from source: '%s'" heading)
        (org-collect-code-todos--debug "Extracted TODO ID from source: '%s'" todo-id)
        (org-collect-code-todos--message "heading: %s" heading)
        (push (cons "TODO_ID" todo-id) properties)))
    
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
Returns a list of (todo-line following-lines todo-start) or nil if no TODO found,
where todo-start is the position where the TODO comment starts."
  (org-collect-code-todos--debug "Extracting TODO info from lines %d-%d" line-start line-end)
  (let ((lines (split-string (buffer-substring-no-properties line-start line-end) "\n"))
        todo-line following-lines todo-start)
    (when lines
      (setq todo-line (car lines))
      (when (and todo-line 
                 (string-match (concat "\\(.*?\\)\\(" 
                                       (regexp-quote (org-collect-code-todos--get-comment-prefix))
                                       "\\s-*\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\].*\\)") 
                               todo-line))
        (let ((prefix (match-string 1 todo-line))
              (todo-part (match-string 2 todo-line)))
          ;; Calculate the position where the TODO comment starts
          (setq todo-start (+ line-start (length prefix)))
          ;; Update todo-line to only include the TODO part
          (setq todo-line todo-part)
          (setq following-lines (cdr lines))
          ;; Filter following lines to only include those with scheduling info
          (setq following-lines 
                (seq-filter (lambda (line) 
                              (or (string-match "SCHEDULED:" line)
                                  (string-match "DEADLINE:" line)))
                            following-lines))
          (org-collect-code-todos--debug "Found TODO: %s" todo-line)
          (org-collect-code-todos--debug "Following lines: %s" following-lines)
          (org-collect-code-todos--debug "TODO starts at position: %d" todo-start)
          (list todo-line following-lines todo-start))))))

(defun org-collect-code-todos--create-todo-with-id (todo-text)
  "Create a new TODO with TODO-TEXT and a generated ID.
Returns the TODO line with ID."
  (org-collect-code-todos--debug "Creating new TODO with text: %s" todo-text)
  (let* ((id (org-collect-code-todos--generate-uuid))
         (comment-prefix (org-collect-code-todos--get-comment-prefix))
         (todo-line (format "%s TODO[%s] %s" comment-prefix id todo-text)))
    (org-collect-code-todos--debug "Created TODO line: %s" todo-line)
    todo-line))

(defun org-collect-code-todos--buffer-supported-p ()
  "Return non-nil if the current buffer is supported for TODO collection.
This means it's either in a programming mode or a mode with defined comment prefixes."
  (or (derived-mode-p 'prog-mode)
      (assq major-mode org-collect-code-todos-comment-prefixes)))

(defun org-collect-code-todos--collect-todos-in-buffer ()
  "Collect all TODOs in the current buffer.
Returns a list of (todo-line following-lines) for each TODO found.
Only works in programming modes."
  (org-collect-code-todos--debug "Collecting TODOs in buffer: %s" (buffer-name))
  (if (not (org-collect-code-todos--buffer-supported-p))
      (progn
        (org-collect-code-todos--debug "Buffer is not in a supported mode, skipping")
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
                          (looking-at (concat "^"  comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                          (not (looking-at (concat "^"  comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
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
                   (prefix-text (buffer-substring-no-properties line-start (match-beginning 1)))
                   (line-end (line-end-position))
                   (following-lines-end line-end)
                   (_ (message "prefix %s" prefix-text))
                   (id (org-collect-code-todos--generate-uuid))
                   (new-todo-line (format "%s%s[%s] %s"
                                          prefix-text
                                          todo-state
                                          id
                                          todo-text)))

              ;; Look for following comment lines with scheduling info
              (save-excursion
                (forward-line 1)
                (while (and (not (eobp))
                            (looking-at (concat "^" comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                            (not (looking-at (concat "^" comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
                  (setq following-lines-end (line-end-position))
                  (forward-line 1)))
              
              ;; Replace the old TODO line with the new one that has an ID
              (org-collect-code-todos--debug "Converting TODO without ID: %s" todo-text)
              (org-collect-code-todos--debug "%s %s" line-start line-end)
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

(defcustom org-collect-code-todos-file nil
  "File for storing code TODOs.
If nil, defaults to code-todos.org in the project root or current directory."
  :type '(choice (file :tag "File path")
          (const :tag "Default location" nil))
  :group 'org-collect-code-todos)

(defcustom org-collect-code-todos-debug nil
  "Whether to enable debug logging for org-collect-code-todos.
When non-nil, debug messages will be logged to the debug buffer."
  :type 'boolean
  :group 'org-collect-code-todos)

(defun org-collect-code-todos--get-org-file-path ()
  "Get the path to the org file for storing TODOs."
  (or org-collect-code-todos-file
      (expand-file-name "code-todos.org"
                        (or (projectile-project-root) default-directory))))

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
    
    (org-collect-code-todos--debug "Source to org conversion: heading='%s', todo-id='%s'" 
                                   heading todo-id)
    
    (with-current-buffer (find-file-noselect file)
      (org-collect-code-todos--with-writable-buffer 
       (current-buffer)
       (lambda ()
         ;; Temporarily remove hooks and advice to prevent recursive updates
         (org-collect-code-todos--debug "Temporarily removing org hooks and advice")
         (remove-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
         (advice-remove 'org-schedule #'org-collect-code-todos--sync-todo-to-source-advice)
         (advice-remove 'org-deadline #'org-collect-code-todos--sync-todo-to-source-advice)
         
         (unwind-protect
             (progn
               (if todo-point
                   ;; Update existing TODO
                   (progn
                     (org-collect-code-todos--debug "Updating existing TODO: %s" heading)
                     (goto-char todo-point)
                     ;; Extract the TODO state and text from the heading
                     (if (string-match "^\\(TODO\\|DONE\\) \\(.*\\)" heading)
                         (let ((todo-state (match-string 1 heading))
                               (todo-text (match-string 2 heading)))
                           (org-collect-code-todos--debug 
                            "Updating org entry: state='%s', text='%s'" 
                            todo-state todo-text)
                           ;; Update the headline first, because updating schedule and todo will trigger an update back
                           (org-edit-headline todo-text)
                           ;; Set the TODO state first
                           (org-todo todo-state))
                       ;; If no TODO state in heading, just update the headline
                       (progn
                         (org-collect-code-todos--debug 
                          "Updating org entry with just heading: '%s'" heading)
                         (org-edit-headline heading)))
                     (when scheduled
                       (org-schedule nil scheduled))
                     (when deadline
                       (org-deadline nil deadline)))
                 ;; Create new TODO
                 (progn
                   (org-collect-code-todos--debug "Creating new TODO: %s" heading)
                   ;; Find or create the main "Code TODOs" heading
                   (org-collect-code-todos--find-or-create-heading file (list "Code TODOs"))
                   (insert "\n")
                   (insert "** " heading)
                   (org-set-property "TODO_ID" todo-id)
                   (org-entry-put (point) "FILE_PATH" file-path)
                   (when scheduled
                     (org-schedule nil scheduled))
                   (when deadline
                     (org-deadline nil deadline)))))
           
           ;; Restore hooks and advice
           (org-collect-code-todos--debug "Restoring org hooks and advice")
           (add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
           (advice-add 'org-schedule :after #'org-collect-code-todos--sync-todo-to-source-advice)
           (advice-add 'org-deadline :after #'org-collect-code-todos--sync-todo-to-source-advice)))))))

(defun org-collect-code-todos--find-todo-in-source-file (file-path todo-id)
  "Find a TODO with TODO-ID in FILE-PATH.
Returns (point . end-point) and prefix text, or nil if not found."
  (org-collect-code-todos--debug "Finding TODO with ID %s in file %s" todo-id file-path)
  (when (and file-path (file-exists-p file-path))
    (with-current-buffer (find-file-noselect file-path)
      (save-excursion
        (goto-char (point-min))
        (let* ((comment-prefix (org-collect-code-todos--get-comment-prefix))
               (case-fold-search nil)
               (todo-regexp (concat comment-prefix "\\s-*\\(TODO\\|DONE\\)\\["
                                    (regexp-quote todo-id) "\\]"))
               start end prefix-text)
          (when (re-search-forward todo-regexp nil t)
            ;; Get all text before the TODO
            (save-excursion
              (beginning-of-line)
              (setq start (point))
              (re-search-forward "\\(TODO\\|DONE\\)\\[" (line-end-position) t)
              (setq prefix-text (buffer-substring-no-properties start (match-beginning 0))))

            (setq end (line-end-position))

            ;; Look for following comment lines with scheduling/deadline info
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp))
                          (looking-at (concat "^\\s-*" comment-prefix "\\s-*\\(SCHEDULED\\|DEADLINE\\):"))
                          (not (looking-at (concat "^\\s-*" comment-prefix "\\s-*\\(TODO\\|DONE\\)"))))
                (setq end (line-end-position))
                (forward-line 1)))

            (list start (1+ end) prefix-text)))))))


(defun org-collect-code-todos--update-todo-in-source-file (file-path todo-id)
  "Update TODO with TODO-ID in FILE-PATH from current org entry."
  (org-collect-code-todos--debug "Updating TODO %s in file %s" todo-id file-path)
  (when (and file-path (file-exists-p file-path))
    (let* ((heading (org-get-heading t t t t))
           (todo-state (org-get-todo-state))
           (properties (org-entry-properties))
           (scheduled (cdr (assoc "SCHEDULED" properties)))
           (deadline (cdr (assoc "DEADLINE" properties)))
           ;; Extract the text from the link if present
           (todo-text (if (string-match "\\[\\[file:.*?\\]\\[\\(.*?\\)\\]\\]" heading)
                          (match-string 1 heading)
                        heading))
           (org-heading (if todo-state
                            (concat todo-state " " heading)
                          heading))
           (org-props (append
                       (list (cons "TODO_ID" todo-id))
                       (when scheduled (list (cons "SCHEDULED" scheduled)))
                       (when deadline (list (cons "DEADLINE" deadline))))))

      (with-current-buffer (find-file-noselect file-path)
        (let* ((todo-info (org-collect-code-todos--find-todo-in-source-file file-path todo-id))
               (start (nth 0 todo-info))
               (end (nth 1 todo-info))
               (prefix-text (nth 2 todo-info))
               (comment-prefix (org-collect-code-todos--get-comment-prefix))
               ;; Extract just the indentation from prefix-text
               (indentation (if (string-match "^\\([ \t]*\\)" prefix-text)
                                (match-string 1 prefix-text)
                              "")))

          (when todo-info
            (let ((after-save-hook nil)
                  (source-lines (list
                                 ;; First line with TODO - use full prefix and extracted text
                                 (format "%s%s[%s] %s"
                                         prefix-text
                                         todo-state
                                         todo-id
                                         todo-text))))

              ;; Add scheduling/deadline info with just indentation + comment prefix
              (when (or scheduled deadline)
                (let ((schedule-line (format "%s%s" indentation comment-prefix)))
                  (when scheduled
                    (setq schedule-line (concat schedule-line " SCHEDULED: " scheduled)))
                  (when deadline
                    (setq schedule-line (concat schedule-line
                                                (if scheduled " " "")
                                                "DEADLINE: " deadline)))
                  (setq source-lines (append source-lines (list schedule-line)))))

              ;; Replace the content
              (delete-region start end)
              (goto-char start)
              (insert (string-join source-lines "\n") "\n")
              (save-buffer))))))))


(defun org-collect-code-todos--delete-orphaned-todos (file file-path todos)
  "Delete TODOs in FILE for FILE-PATH that are not in TODOS list.
TODOS is a list of (todo-line following-lines) for each TODO found in the source file."
  (org-collect-code-todos--debug "Checking for orphaned TODOs from file: %s" file-path)
  (with-current-buffer (find-file-noselect file)
    (org-collect-code-todos--with-writable-buffer
     (current-buffer)
     (lambda ()
       (let ((active-ids (mapcar (lambda (todo-info)
                                   (let* ((todo-line (car todo-info))
                                          (following-lines (cadr todo-info))
                                          (org-data (org-collect-code-todos--source-to-org todo-line following-lines))
                                          (properties (cdr org-data)))
                                     (cdr (assoc "TODO_ID" properties))))
                                 todos))
             (orphaned-todos nil))

         ;; Scan the entire file for TODOs belonging to this file path
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let ((todo-id (org-entry-get nil "TODO_ID"))
                  (todo-file-path (org-entry-get nil "FILE_PATH")))
              ;; Check if TODO belongs to current file and isn't active
              (when (and todo-id
                         todo-file-path
                         (string= todo-file-path file-path)
                         (not (member todo-id active-ids)))
                (org-collect-code-todos--debug "Found orphaned TODO: %s" todo-id)
                (push (point) orphaned-todos))))
          nil 'file)

         ;; Delete orphaned TODOs in reverse order
         (setq orphaned-todos (nreverse orphaned-todos))
         (dolist (pos orphaned-todos)
           (goto-char pos)
           (let ((id (org-entry-get nil "TODO_ID")))
             (org-collect-code-todos--debug "Archiving orphaned TODO: %s" id)
             (org-archive-subtree))))))))


(defun org-collect-code-todos--update-todos-on-save ()
  "Update TODOs in the org file when saving a source file."
  (when (org-collect-code-todos--buffer-supported-p)
    (org-collect-code-todos--debug "Updating TODOs on save for: %s" (buffer-file-name))
    (let ((todos (org-collect-code-todos--collect-todos-in-buffer))
          (file-path (buffer-file-name))
          (org-file (org-collect-code-todos--ensure-org-file-exists)))
      
      ;; First, delete orphaned TODOs
      (org-collect-code-todos--delete-orphaned-todos org-file file-path todos)
      
      ;; Then update or create TODOs
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

(defun org-collect-code-todos--sync-todo-to-source-advice (&rest _args)
  "Wrapper for `org-collect-code-todos--sync-todo-to-source' to use as advice.
Ignores any arguments passed to it."
  (org-collect-code-todos--sync-todo-to-source))

(defun org-collect-code-todos--make-org-file-read-only ()
  "Make the org TODOs file read-only."
  (let ((file-path (org-collect-code-todos--get-org-file-path)))
    (when (and (buffer-file-name) 
               (string= (expand-file-name (buffer-file-name)) 
                        (expand-file-name file-path)))
      (setq buffer-read-only t)
      (org-collect-code-todos--debug "Made org file read-only: %s" file-path))))

(defun org-collect-code-todos--with-writable-org-file (fn)
  "Execute FN with the org TODOs file temporarily writable.
Restores the read-only state after execution."
  (org-collect-code-todos--debug "Making org file temporarily writable")
  (let ((was-read-only buffer-read-only)
        (inhibit-read-only t))
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (funcall fn))
      (when was-read-only
        (setq buffer-read-only t)
        (org-collect-code-todos--debug "Restored org file read-only state")))))

(defun org-collect-code-todos--setup-org-hooks ()
  "Set up hooks and advice for org-mode synchronization."
  (org-collect-code-todos--debug "Setting up org hooks and advice")
  (add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
  (advice-add 'org-schedule :after #'org-collect-code-todos--sync-todo-to-source-advice)
  (advice-add 'org-deadline :after #'org-collect-code-todos--sync-todo-to-source-advice)
  
  ;; Add advice to temporarily make the org file writable for these operations
  (advice-add 'org-todo :around #'org-collect-code-todos--make-writable-advice)
  (advice-add 'org-collect-code-todos--todo-done-swap :around #'org-collect-code-todos--make-writable-advice)
  (advice-add 'org-schedule :around #'org-collect-code-todos--make-writable-advice)
  (advice-add 'org-deadline :around #'org-collect-code-todos--make-writable-advice)
  
  (add-hook 'find-file-hook #'org-collect-code-todos--make-org-file-read-only))

(defun org-collect-code-todos--remove-org-hooks ()
  "Remove hooks and advice for org-mode synchronization."
  (org-collect-code-todos--debug "Removing org hooks and advice")
  (remove-hook 'org-after-todo-state-change-hook #'org-collect-code-todos--sync-todo-to-source)
  (advice-remove 'org-schedule #'org-collect-code-todos--sync-todo-to-source-advice)
  (advice-remove 'org-deadline #'org-collect-code-todos--sync-todo-to-source-advice)
  
  ;; Remove the writable advice
  (advice-remove 'org-todo #'org-collect-code-todos--make-writable-advice)
  (advice-remove 'org-collect-code-todos--todo-done-swap #'org-collect-code-todos--make-writable-advice)
  (advice-remove 'org-schedule #'org-collect-code-todos--make-writable-advice)
  (advice-remove 'org-deadline #'org-collect-code-todos--make-writable-advice)
  
  (remove-hook 'find-file-hook #'org-collect-code-todos--make-org-file-read-only))

(defun org-collect-code-todos--get-todo-id-at-point ()
  "Get the TODO ID at point in a source file.
Returns the TODO ID or nil if not found."
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (todo-id nil))
    ;; Extract the TODO ID from the current line
    (when (string-match "\\(TODO\\|DONE\\)\\[\\([^]]+\\)\\]" line)
      (setq todo-id (match-string 2 line))
      (org-collect-code-todos--debug "Found TODO ID: %s" todo-id))
    todo-id))

(defun org-collect-code-todos--find-and-goto-org-todo (todo-id)
  "Find the org TODO with TODO-ID.
Returns the buffer and position if found, nil otherwise."
  (when todo-id
    (org-collect-code-todos--debug "Finding org TODO with ID: %s" todo-id)
    ;; Open the org file without selecting it
    (let* ((org-file (org-collect-code-todos--ensure-org-file-exists))
           (buffer (find-file-noselect org-file))
           (found nil))
      (with-current-buffer buffer
        (widen)
        (goto-char (point-min))
        
        ;; Search for the TODO
        (while (and (not found)
                    (re-search-forward org-heading-regexp nil t))
          (let ((properties (org-entry-properties)))
            (when (string= (cdr (assoc "TODO_ID" properties)) todo-id)
              (setq found (match-beginning 0))
              (goto-char found)
              (org-collect-code-todos--debug "Successfully found org TODO"))))
        
        (if found
            (cons buffer (point))
          (org-collect-code-todos--debug "Could not find corresponding TODO in org file")
          (message "Could not find corresponding TODO in org file")
          (org-collect-code-todos--debug "Failed to find TODO with ID: %s" todo-id)
          nil)))))

;;;###autoload
(defun org-collect-code-todos-goto-org-todo ()
  "Jump from a TODO in source code to the corresponding TODO in the org file.
This should be called when point is on a TODO line in a source file."
  (interactive)
  (org-collect-code-todos--debug "Attempting to jump to org TODO from source")
  (let ((todo-id (org-collect-code-todos--get-todo-id-at-point)))
    (if todo-id
        (let ((result (org-collect-code-todos--find-and-goto-org-todo todo-id)))
          (when result
            (let ((buffer (car result))
                  (pos (cdr result)))
              ;; For this function, we actually want to switch to the buffer
              (switch-to-buffer buffer)
              (goto-char pos)
              ;; Reveal and show the entry
              (org-back-to-heading t)  ; Added this line
              (org-reveal)
              (org-show-entry)
              (org-show-children)
              (recenter))))
      (org-collect-code-todos--debug "No TODO found at point")
      (message "No TODO found at point")
      (org-collect-code-todos--debug "No TODO ID found at current line"))))

;;;###autoload
(defun org-collect-code-todos-toggle-todo-state ()
  "Toggle the TODO state of the org entry corresponding to the source code TODO at point."
  (interactive)
  (org-collect-code-todos--debug "Toggling TODO state from source")
  (let ((todo-id (org-collect-code-todos--get-todo-id-at-point)))
    (if todo-id
        (let ((result (org-collect-code-todos--find-and-goto-org-todo todo-id)))
          (when result
            (let ((buffer (car result))
                  (pos (cdr result)))
              (with-current-buffer buffer
                (goto-char pos)
                (org-back-to-heading t)  ; Added this line
                (org-collect-code-todos--message "INFO: %s %s %s" todo-id result buffer)
                (org-collect-code-todos--with-writable-org-file
                 (lambda ()
                   (org-collect-code-todos--todo-done-swap)
                   (org-collect-code-todos--message "Toggled TODO state")))))))
      (message "No TODO found at point")
      (org-collect-code-todos--debug "No TODO ID found at current line"))))

;;;###autoload
(defun org-collect-code-todos-schedule ()
  "Schedule the org entry corresponding to the source code TODO at point."
  (interactive)
  (org-collect-code-todos--debug "Scheduling TODO from source")
  (let ((todo-id (org-collect-code-todos--get-todo-id-at-point)))
    (if todo-id
        (let ((result (org-collect-code-todos--find-and-goto-org-todo todo-id)))
          (when result
            (let ((buffer (car result)))
              (with-current-buffer buffer
                ;; Schedule the TODO
                (org-collect-code-todos--with-writable-org-file
                 (lambda ()
                   (call-interactively 'org-schedule)
                   (org-collect-code-todos--message "Scheduled TODO")))))))
      (message "No TODO found at point")
      (org-collect-code-todos--debug "No TODO ID found at current line"))))

;;;###autoload
(defun org-collect-code-todos-deadline ()
  "Set deadline for the org entry corresponding to the source code TODO at point."
  (interactive)
  (org-collect-code-todos--debug "Setting deadline for TODO from source")
  (let ((todo-id (org-collect-code-todos--get-todo-id-at-point)))
    (if todo-id
        (let ((result (org-collect-code-todos--find-and-goto-org-todo todo-id)))
          (when result
            (let ((buffer (car result)))
              (with-current-buffer buffer
                ;; Set deadline for the TODO
                (org-collect-code-todos--with-writable-org-file
                 (lambda ()
                   (call-interactively 'org-deadline)
                   (org-collect-code-todos--message "Set deadline for TODO")))))))
      (message "No TODO found at point")
      (org-collect-code-todos--debug "No TODO ID found at current line"))))

(defun org-collect-code-todos--make-writable-advice (orig-fun &rest args)
  "Advice to make the org file temporarily writable during execution of ORIG-FUN with ARGS."
  (if (and (buffer-file-name)
           (string= (expand-file-name (buffer-file-name))
                    (expand-file-name (org-collect-code-todos--get-org-file-path))))
      (org-collect-code-todos--with-writable-org-file
       (lambda () (apply orig-fun args)))
    (apply orig-fun args)))

(defun org-collect-code-todos--todo-done-swap ()
  "Swap between TODO and DONE states for the current org heading."
  (org-collect-code-todos--debug "Swapping TODO/DONE state")
  (let* ((context (org-element-context))
         (todo-type (org-element-property :todo-type context)))
    (org-collect-code-todos--message "todo-type: %s" todo-type)
    (org-todo
     (if (eq todo-type 'done)
         (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
             'todo)
       'done))))

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
