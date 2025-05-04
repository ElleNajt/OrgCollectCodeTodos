;;; org-collect-code-todos.el --- Collect TODOs from code into org files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: April 29, 2025
;; Modified: April 29, 2025
;; Version: 0.0.1
;; Keywords: convenience tools
;; Homepage: https://github.com/elle/org-collect-code-todos
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package collects TODO-comments from code files and organizes them
;; in an org-mode file. It tracks TODOs with unique IDs and synchronizes
;; state changes between the org file and source code.
;;
;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup org-collect-code-todos nil
  "Collect TODO comments from code files into an org file."
  :group 'org
  :prefix "org-collect-code-todos-")

(defcustom org-collect-code-todos-file (expand-file-name "~/org/code-todos.org")
  "File path where code TODOs will be collected."
  :type 'file
  :group 'org-collect-code-todos)

(defcustom org-collect-code-todos-archive-file nil
  "File path where archived TODOs will be stored.
If nil, defaults to code-todos.archive.org in the same directory."
  :type '(choice (const :tag "Default" nil)
          (file :tag "Custom file"))
  :group 'org-collect-code-todos)

(defcustom org-collect-code-todos-read-only t
  "Whether the code-todos.org file should be read-only by default.
When enabled, the file is read-only except when marking TODOs as done or archiving."
  :type 'boolean
  :group 'org-collect-code-todos)

;;; Variables

(defvar-local org-collect-code-todos-keep-writable nil
  "When non-nil, prevents the buffer from being made read-only.
This is used during operations like changing TODO states or archiving.")

;;; Helper functions

(defun org-collect-code-todos--debug-log (message &rest args)
  "Write a debug log message to .aider-debug-logs file."
  (let ((log-file (expand-file-name ".aider-debug-logs")))
    (with-temp-buffer
      (insert (format "[%s] [%s] " 
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (or (buffer-file-name) "no-file")))
      (insert (apply #'format message args))
      (insert "\n")
      (append-to-file (point-min) (point-max) log-file))))

(defun org-collect-code-todos--generate-id ()
  "Generate a unique ID for a TODO item."
  (format "%08x%08x" (random #xffffffff) (random #xffffffff)))

(defun org-collect-code-todos--log-buffer-content (prefix)
  "Log the content of the current buffer for debugging.
PREFIX is added to the log message."
  (let ((content (buffer-substring-no-properties 
                  (point-min) 
                  (min (point-max) (+ (point-min) 1000)))))
    (org-collect-code-todos--debug-log 
     "%s buffer content (first 1000 chars):\n%s" 
     prefix content)))

(defun org-collect-code-todos--find-todo-at-point ()
  "Find a TODO comment at point in a source file.
Returns a plist with :id, :state, :text, :scheduled, and :deadline properties,
or nil if no TODO is found."
  (save-excursion
    (beginning-of-line)
    (let ((comment-start-regex (concat "^\\s-*" (regexp-quote (string-trim comment-start)))))
      (when (looking-at comment-start-regex)
        (let ((todo-regex "\\(TODO\\|DONE\\)\\(\\[\\([0-9a-f]+\\)\\]\\)?[ \t]+\\(.*\\)"))
          (when (re-search-forward todo-regex (line-end-position) t)
            (let* ((todo-id (match-string 3))
                   (todo-state (match-string 1))
                   (todo-text (match-string 4))
                   (existing-scheduled nil)
                   (existing-deadline nil))
              
              ;; Check for existing scheduled and deadline
              (save-excursion
                (forward-line 1)
                (while (and (< (point) (point-max))
                            (looking-at (format "^\\s-*[%s]+\\s-*\\(.*\\)" 
                                                (regexp-quote (string-trim comment-start)))))
                  (let ((comment-text (match-string-no-properties 1)))
                    (when (string-match "SCHEDULED:\\s-*\\(<[^>]+>\\)" comment-text)
                      (setq existing-scheduled (match-string 1 comment-text)))
                    (when (string-match "DEADLINE:\\s-*\\(<[^>]+>\\)" comment-text)
                      (setq existing-deadline (match-string 1 comment-text)))
                    (forward-line 1))))
              
              (list :id todo-id 
                    :state todo-state 
                    :text todo-text
                    :scheduled existing-scheduled
                    :deadline existing-deadline))))))))

(defun org-collect-code-todos--is-todos-buffer-p ()
  "Check if current buffer is the code-todos file."
  (and (buffer-file-name)
       (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file))))

(defun org-collect-code-todos--with-writable-buffer (fn)
  "Execute FN with the buffer temporarily writable if it's the todos buffer."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (funcall fn)
    (let ((was-read-only (and org-collect-code-todos-read-only buffer-read-only))
          (inhibit-read-only t))
      (when was-read-only
        (read-only-mode -1))
      (unwind-protect
          (progn
            (setq-local org-collect-code-todos-keep-writable t)
            (funcall fn))
        (setq-local org-collect-code-todos-keep-writable nil)
        (when was-read-only
          (read-only-mode 1))))))

(defun org-collect-code-todos--get-archive-file ()
  "Get the archive file path for code TODOs."
  (or org-collect-code-todos-archive-file
      (concat (file-name-sans-extension
               (expand-file-name org-collect-code-todos-file))
              ".archive.org")))

(defun org-collect-code-todos--extract-todo-properties ()
  "Extract TODO properties from current heading.
Returns a plist with :id, :path, :last-text, :scheduled, and :deadline properties."
  (save-excursion
    (condition-case error-obj
        (progn
          (org-back-to-heading t)
          (let* ((next-heading-pos (save-excursion
                                     (condition-case nil
                                         (outline-next-heading)
                                       (error (goto-char (point-max))))
                                     (point)))
                 (heading-content (buffer-substring-no-properties (point) next-heading-pos))
                 (todo-id nil)
                 (path nil)
                 (last-text nil)
                 (scheduled nil)
                 (deadline nil))
            
            (org-collect-code-todos--debug-log 
             "Extracting properties from heading at pos %d, content length: %d" 
             (point) (length heading-content))
            
            ;; Extract file path from link
            (when (string-match "\\[\\[\\(.+?\\)\\]" heading-content)
              (setq path (match-string 1 heading-content))
              (org-collect-code-todos--debug-log "Found path: %s" path))
            
            ;; Extract TODO_ID property
            (when (string-match ":TODO_ID:\\s-*\\([0-9a-f]+\\)" heading-content)
              (setq todo-id (match-string 1 heading-content))
              (org-collect-code-todos--debug-log "Found TODO_ID: %s" todo-id))
            
            ;; Extract LAST property
            (when (string-match ":LAST:\\s-*\\(.*\\)" heading-content)
              (setq last-text (match-string 1 heading-content))
              (org-collect-code-todos--debug-log "Found LAST: %s" last-text))
            
            ;; Get scheduled and deadline timestamps
            (let ((scheduled-time (org-entry-get (point) "SCHEDULED"))
                  (deadline-time (org-entry-get (point) "DEADLINE")))
              (when scheduled-time
                (setq scheduled scheduled-time)
                (org-collect-code-todos--debug-log "Found SCHEDULED: %s" scheduled-time))
              (when deadline-time
                (setq deadline deadline-time)
                (org-collect-code-todos--debug-log "Found DEADLINE: %s" deadline-time)))
            
            (list :id todo-id :path path :last-text last-text 
                  :scheduled scheduled :deadline deadline)))
      (error
       (org-collect-code-todos--debug-log 
        "Error extracting TODO properties: %s at point %d" 
        (error-message-string error-obj) (point))
       (list :id nil :path nil :last-text nil)))))

;;; Core functionality

(defun org-collect-code-todos-collect-and-add ()
  "Collect TODOs from current buffer, add them to the org file, and remove deleted ones."
  (when (derived-mode-p 'prog-mode)
    (condition-case err
        (org-collect-code-todos--do-collect-and-add)
      (error
       (org-collect-code-todos--debug-log 
        "Error in org-collect-code-todos-collect-and-add: %s" 
        (error-message-string err))
       (message "Error collecting TODOs: %s" (error-message-string err))))))

(defun org-collect-code-todos--do-collect-and-add ()
  "Internal function that does the actual TODO collection work."
  (let ((file-path (buffer-file-name))
        (comment-start (string-trim comment-start))
        todos)
    
    (org-collect-code-todos--debug-log 
     "Starting TODO collection for file: %s with comment-start: '%s'" 
     file-path comment-start)

    ;; Find TODOs and DONEs in the current buffer
    (save-excursion
      (goto-char (point-min))
      (let ((todo-regex (format "^\\s-*[%s]*\\s-*\\(\\(?:TODO\\|DONE\\)\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\)[ \t]+\\(.*\\)"
                                (regexp-quote comment-start))))
        (org-collect-code-todos--debug-log "Using regex: %s" todo-regex)
        (while (re-search-forward todo-regex nil t)
          (let* ((existing-id (match-string-no-properties 2))
                 (todo-state (match-string-no-properties 1))
                 (todo-text (string-trim (match-string-no-properties 3)))
                 (file-name (replace-regexp-in-string "[.-]" "_"
                                                      (file-name-nondirectory file-path)))
                 (id (or existing-id (format "%08x%08x" (random #xffffffff) (random #xffffffff))))
                 (org-state (if (string-match-p "^DONE" todo-state) "DONE" "TODO"))
                 ;; Check for scheduling line after the TODO line
                 (scheduled nil)
                 (deadline nil)
                 (line-end (line-end-position))
                 (next-line-pos (save-excursion (forward-line 1) (point))))
            
            ;; Look for SCHEDULED and DEADLINE comments in the next lines
            (save-excursion
              (forward-line 1)
              (while (and (< (point) (point-max))
                          (looking-at (format "^\\s-*[%s]+\\s-*\\(.*\\)" 
                                              (regexp-quote comment-start))))
                (let ((comment-text (match-string-no-properties 1)))
                  ;; Extract SCHEDULED if not already found
                  (when (and (not scheduled)
                             (string-match "SCHEDULED:\\s-*\\(<[^>]+>\\)" comment-text))
                    (setq scheduled (match-string 1 comment-text)))
                  
                  ;; Extract DEADLINE if not already found
                  (when (and (not deadline)
                             (string-match "DEADLINE:\\s-*\\(<[^>]+>\\)" comment-text))
                    (setq deadline (match-string 1 comment-text)))
                  
                  (forward-line 1))))
            
            ;; Construct a clean scheduling line
            (let ((scheduling-line ""))
              (when scheduled
                (setq scheduling-line (concat scheduling-line "SCHEDULED: " scheduled)))
              (when (and scheduled deadline)
                (setq scheduling-line (concat scheduling-line " ")))
              (when deadline
                (setq scheduling-line (concat scheduling-line "DEADLINE: " deadline)))

              (let ((entry (format "* %s %s :%s:\n:PROPERTIES:\n:TODO_ID: %s\n:LAST: %s\n:END:\n"
                                   org-state
                                   todo-text
                                   file-name
                                   id
                                   todo-text)))

                ;; Add scheduling information if present
                (when (or scheduled deadline)
                  (let ((planning-line ""))
                    (when scheduled
                      (setq planning-line (concat planning-line "SCHEDULED: " scheduled)))
                    (when (and scheduled deadline)
                      (setq planning-line (concat planning-line " ")))
                    (when deadline
                      (setq planning-line (concat planning-line "DEADLINE: " deadline)))
                    (setq entry (concat entry planning-line "\n"))))

                ;; Add the file link
                (setq entry (concat entry (format "[[%s][%s]]\n" file-path todo-text)))

                (org-collect-code-todos--debug-log
                 "Found TODO: state=%s, id=%s, text='%s'"
                 todo-state (or existing-id "new") todo-text)

                ;; If no ID exists, add one to the source file
                (unless existing-id
                  (let ((original-prefix (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (match-beginning 1)))
                        (todo-with-id (format "%s[%s] %s"
                                              (if (string-match-p "^DONE" todo-state) "DONE" "TODO")
                                              id todo-text)))
                    (org-collect-code-todos--debug-log
                     "Adding ID to TODO: prefix='%s', new-text='%s'"
                     original-prefix todo-with-id)
                    (replace-match (concat original-prefix todo-with-id))))

                (push entry todos)))))

        ;; Process collected TODOs
        (with-current-buffer (find-file-noselect org-collect-code-todos-file)
          (org-mode)
          (org-collect-code-todos--with-writable-buffer
           (lambda ()
             ;; First, collect all TODO IDs from the current source file
             (let ((source-todo-ids (mapcar
                                     (lambda (todo)
                                       (let* ((todo-lines (split-string todo "\n"))
                                              (id-line (nth 2 todo-lines)))
                                         (when (string-match ":TODO_ID:\\s-*\\(.*\\)" id-line)
                                           (match-string 1 id-line))))
                                     todos)))

               (org-collect-code-todos--debug-log
                "Processing file: %s with %d TODOs, IDs: %s"
                file-path (length todos) (mapconcat #'identity source-todo-ids ", "))

               ;; Then, find and archive TODOs that reference this file but aren't in the source anymore
               (org-collect-code-todos--archive-deleted-todos file-path source-todo-ids)

               ;; Now add/update TODOs from the source file
               (dolist (todo todos)
                 (let* ((todo-lines (split-string todo "\n"))
                        (heading-line (car todo-lines))
                        (id-line (nth 2 todo-lines))
                        (todo-id (when (string-match ":TODO_ID:\\s-*\\(.*\\)" id-line)
                                   (match-string 1 id-line)))
                        (todo-text (when (string-match "\\* \\(?:TODO\\|DONE\\) \\(.*\\) :" heading-line)
                                     (match-string 1 heading-line)))
                        (existing-entry-found nil))

                   ;; Check if we have an entry with the same ID
                   (save-excursion
                     (goto-char (point-min))
                     (when (and todo-id
                                (re-search-forward (format ":TODO_ID:\\s-*%s"
                                                           (regexp-quote todo-id)) nil t))
                       (setq existing-entry-found t)
                       (condition-case nil
                           (org-back-to-heading t)
                         (error
                          (setq existing-entry-found nil)))

                       (when existing-entry-found
                         ;; Update existing entry if needed
                         (let* ((props (org-collect-code-todos--extract-todo-properties))
                                (current-last (plist-get props :last-text))
                                (current-heading-text (org-get-heading t t t t)))

                           (when (and current-last
                                      (string= current-last current-heading-text)
                                      (not (string= current-heading-text todo-text)))
                             (org-edit-headline todo-text)
                             (org-entry-put (point) "LAST" todo-text)))))

                     ;; Add new entry if needed
                     (unless existing-entry-found
                       (org-collect-code-todos--debug-log
                        "Adding new TODO entry with ID %s: %s"
                        todo-id (substring todo 0 (min 50 (length todo))))
                       (goto-char (point-max))
                       (insert "\n" todo)))))

               (save-buffer)))))))))

(defun org-collect-code-todos--archive-deleted-todos (file-path active-todo-ids)
  "Archive TODOs from the org file that reference FILE-PATH but aren't in ACTIVE-TODO-IDS."
  (org-collect-code-todos--debug-log 
   "Starting archive check for %s with active IDs: %s" 
   file-path (mapconcat #'identity active-todo-ids ", "))
  
  (save-excursion
    (goto-char (point-min))
    (let ((file-path-regexp (regexp-quote file-path))
          (archived-count 0)
          (org-archive-location (concat (org-collect-code-todos--get-archive-file)
                                        "::* Deleted TODOs")))
      
      ;; Simpler, more robust approach to finding and processing entries
      (goto-char (point-min))
      (while (re-search-forward file-path-regexp nil t)
        (org-collect-code-todos--debug-log "Found file path match at position %d" (point))
        (condition-case err
            (progn
              (org-back-to-heading t)
              (let* ((heading-pos (point))
                     (props (org-collect-code-todos--extract-todo-properties))
                     (todo-id (plist-get props :id))
                     (path (plist-get props :path)))
                
                (org-collect-code-todos--debug-log 
                 "Checking heading at pos %d with ID %s, path %s" 
                 heading-pos todo-id path)
                
                ;; If this entry references our file but its ID isn't in active-todo-ids, archive it
                (when (and path 
                           (string= path file-path)
                           todo-id
                           (not (member todo-id active-todo-ids)))
                  (org-collect-code-todos--debug-log 
                   "Archiving TODO with ID %s (not found in source)" todo-id)
                  
                  ;; Add a note about why it was archived
                  (org-entry-put (point) "ARCHIVED_REASON" "Deleted from source code")
                  
                  ;; Archive the subtree
                  (org-archive-subtree)
                  (setq archived-count (1+ archived-count)))
                
                ;; Always move past this subtree to avoid infinite loops
                (org-end-of-subtree t t)))
          (error 
           (org-collect-code-todos--debug-log 
            "Error during TODO archiving: %s at position %d" 
            (error-message-string err) (point))
           ;; On error, move forward a bit to avoid getting stuck
           (forward-line 1))))
      
      (when (> archived-count 0)
        (message "Archived %d TODOs that no longer exist in %s" 
                 archived-count (file-name-nondirectory file-path))))))

(defun org-collect-code-todos--find-todo-by-id (todo-id)
  "Find a TODO entry in the org file by its ID.
Returns a cons cell (buffer . position) if found, nil otherwise."
  (when (file-exists-p org-collect-code-todos-file)
    (with-current-buffer (find-file-noselect org-collect-code-todos-file)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (format ":TODO_ID:\\s-*%s" 
                                         (regexp-quote todo-id)) nil t)
          (condition-case nil
              (progn
                (org-back-to-heading t)
                (cons (current-buffer) (point)))
            (error nil)))))))

(defun org-collect-code-todos-jump-to-org-entry ()
  "Jump to the corresponding org entry for the TODO at point."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let ((todo (org-collect-code-todos--find-todo-at-point)))
      (if todo
          (let ((todo-id (plist-get todo :id)))
            (if todo-id
                (let ((entry (org-collect-code-todos--find-todo-by-id todo-id)))
                  (if entry
                      (progn
                        (pop-to-buffer (car entry))
                        (goto-char (cdr entry))
                        (org-show-context)
                        (org-show-entry))
                    (message "TODO entry not found in org file")))
              (message "This TODO doesn't have an ID yet. Save the file first.")))
        (message "No TODO found at point")))))

(defun org-collect-code-todos-toggle-state-at-point ()
  "Toggle the TODO/DONE state of the TODO comment at point."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (org-collect-code-todos--debug-log "Toggling TODO state at point in %s" (buffer-file-name))
    (let ((todo (org-collect-code-todos--find-todo-at-point)))
      (when todo
        (let* ((todo-id (or (plist-get todo :id) (org-collect-code-todos--generate-id)))
               (todo-state (plist-get todo :state))
               (todo-text (plist-get todo :text))
               (new-state (if (string= todo-state "TODO") "DONE" "TODO")))
          
          ;; Replace the TODO/DONE state
          (save-excursion
            (beginning-of-line)
            (when (re-search-forward (concat (regexp-quote todo-state) 
                                             (if (plist-get todo :id)
                                                 (concat "\\[" (regexp-quote todo-id) "\\]") 
                                               "")) 
                                     (line-end-position) t)
              (replace-match (concat new-state "[" todo-id "]"))))
          
          ;; Update the org file entry if it exists
          (org-collect-code-todos--update-org-entry-state todo-id new-state)
          
          ;; Save the buffer
          (save-buffer)
          (message "Toggled %s to %s" todo-state new-state))))))

(defun org-collect-code-todos--update-org-entry-state (todo-id new-state)
  "Update the state of the org entry with TODO-ID to NEW-STATE."
  (when (file-exists-p org-collect-code-todos-file)
    (org-collect-code-todos--debug-log 
     "Updating org entry state for TODO[%s] to %s" todo-id new-state)
    (with-current-buffer (find-file-noselect org-collect-code-todos-file)
      (org-mode)
      (org-collect-code-todos--with-writable-buffer
       (lambda ()
         (save-excursion
           (goto-char (point-min))
           (let ((search-pattern (format ":TODO_ID:\\s-*%s" (regexp-quote todo-id))))
             (org-collect-code-todos--debug-log "Searching for pattern: %s" search-pattern)
             (when (re-search-forward search-pattern nil t)
               (condition-case err
                   (progn
                     (org-back-to-heading t)
                     (let ((current-state (org-get-todo-state)))
                       (org-collect-code-todos--debug-log 
                        "Found entry with current state: %s" current-state)
                       (when (and current-state
                                  (not (string= current-state new-state)))
                         (org-collect-code-todos--debug-log 
                          "Changing state from %s to %s" current-state new-state)
                         (org-todo new-state))))
                 (error 
                  (org-collect-code-todos--debug-log 
                   "Error updating org entry state: %s" (error-message-string err))
                  nil))))))))))

(defun org-collect-code-todos--update-scheduling-comments (comment-start todo-id scheduled deadline)
  "Update or add scheduling comments for TODO with TODO-ID.
Uses COMMENT-START as the comment character.
SCHEDULED and DEADLINE are the timestamp strings or nil."
  (org-collect-code-todos--debug-log 
   "Updating scheduling comments for TODO[%s] with comment-start='%s', SCHEDULED=%s, DEADLINE=%s" 
   todo-id comment-start (or scheduled "nil") (or deadline "nil"))
  
  (save-excursion
    (let ((line-start (line-beginning-position))
          (indent (make-string (current-indentation) ? )))
      
      ;; First, delete all scheduling lines after this TODO
      (forward-line 1)
      (let ((delete-count 0)
            (start-pos (point)))
        (org-collect-code-todos--debug-log 
         "Looking for scheduling lines to delete starting at position %d" start-pos)
        
        (while (and (< (point) (point-max))
                    (or (looking-at (format "^\\s-*[%s]+\\s-*\\(SCHEDULED:\\|DEADLINE:\\)" 
                                            (regexp-quote comment-start)))
                        (looking-at (format "^\\s-*[%s]+\\s-*.*\\(SCHEDULED:\\|DEADLINE:\\)"
                                            (regexp-quote comment-start)))))
          (org-collect-code-todos--debug-log 
           "Found scheduling line to delete: '%s'" 
           (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (forward-line 1)
          (setq delete-count (1+ delete-count)))
        
        (when (> delete-count 0)
          (org-collect-code-todos--debug-log 
           "Deleting %d scheduling lines from %d to %d" 
           delete-count start-pos (point))
          (delete-region start-pos (point))))
      
      ;; Add scheduling comments as a single clean line
      (when (or scheduled deadline)
        (goto-char line-start)
        (end-of-line)
        (let ((planning-line ""))
          (when scheduled
            (setq planning-line (concat planning-line "SCHEDULED: " scheduled)))
          (when (and scheduled deadline)
            (setq planning-line (concat planning-line " ")))
          (when deadline
            (setq planning-line (concat planning-line "DEADLINE: " deadline)))
          
          ;; Ensure comment-start is not empty and has proper spacing
          (let ((comment-prefix (if (string-empty-p comment-start) 
                                    "# " 
                                  (concat comment-start " "))))
            (org-collect-code-todos--debug-log 
             "Adding scheduling line: '%s' with comment prefix '%s'" 
             planning-line comment-prefix)
            (insert "\n" indent comment-prefix planning-line)
            (org-collect-code-todos--debug-log 
             "Added scheduling line at position %d" (point))))))))

(defun org-collect-code-todos-update-source-file-by-id (path todo-id org-state org-todo-text last-text)
  "Update TODO state in source file by searching for its ID.
PATH is the source file path.
TODO-ID is the unique identifier for the TODO.
ORG-STATE is the new state (TODO or DONE).
ORG-TODO-TEXT is the text of the TODO item.
LAST-TEXT is the previous text of the TODO item."
  (org-collect-code-todos--debug-log 
   "Updating source file %s for TODO[%s] to state %s" 
   path todo-id org-state)
  (condition-case error-obj
      (with-current-buffer (find-file-noselect path)
        (let ((text-changed (and last-text org-todo-text (not (string= org-todo-text last-text))))
              (found nil)
              (result nil))
          
          ;; Search for the TODO[c40ac004] with the specific ID
          (goto-char (point-min))
          (let ((search-pattern (format "\\(^\\|[ \t]\\)\\s-*\\(TODO\\|DONE\\)\\[%s\\][ \t]+\\(.*\\)"
                                        (regexp-quote todo-id))))
            (org-collect-code-todos--debug-log "Searching with pattern: %s" search-pattern)
            (while (and (not found)
                        (re-search-forward search-pattern nil t))
              (setq found t)
              (let ((leading-space (match-string 1))
                    (current-state (match-string 2))
                    (current-text (match-string 3)))
                (org-collect-code-todos--debug-log 
                 "Found TODO[%s] with state %s and text '%s'" 
                 todo-id current-state current-text)
                (if text-changed
                    ;; Text changed - generate new UUID and update everything
                    (let ((new-uuid (format "%08x%08x" (random #xffffffff) (random #xffffffff))))
                      (org-collect-code-todos--debug-log 
                       "Text changed from '%s' to '%s', generating new UUID: %s" 
                       current-text org-todo-text new-uuid)
                      (replace-match (concat leading-space org-state "[" new-uuid "] " org-todo-text))
                      (setq result (cons new-uuid org-todo-text)))
                  ;; Just update the state
                  (org-collect-code-todos--debug-log 
                   "Updating state from %s to %s" current-state org-state)
                  (replace-match (concat leading-space org-state "[" todo-id "] " current-text))))))
          
          ;; Save the buffer if we made changes
          (when found
            (save-buffer))
          
          result))
    (error
     (org-collect-code-todos--debug-log 
      "Error updating source file: %s" (error-message-string error-obj))
     nil)))

(defun org-collect-code-todos-mark-source-todo-state ()
  "Update TODO/DONE state in source file when changed in code-todos.org."
  (when (and (eq major-mode 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p)
             (or (and (boundp 'org-state) (member org-state '("TODO" "DONE")))
                 (org-entry-get nil "SCHEDULED")
                 (org-entry-get nil "DEADLINE")))
    (org-collect-code-todos--debug-log 
     "Org TODO state or scheduling changed to %s in %s" 
     (if (boundp 'org-state) org-state "scheduling only") 
     (buffer-file-name))
    (condition-case error-obj
        (let* ((props (org-collect-code-todos--extract-todo-properties))
               (todo-id (plist-get props :id))
               (path (plist-get props :path))
               (last-text (plist-get props :last-text))
               (org-todo-text (org-get-heading t t t t))
               (current-state (org-get-todo-state))
               (scheduled (org-entry-get nil "SCHEDULED"))
               (deadline (org-entry-get nil "DEADLINE")))
          
          (when (and todo-id path)
            ;; Get the scheduling line directly from the org file
            (let ((scheduling-line ""))
              (when (or scheduled deadline)
                (when scheduled
                  (setq scheduling-line (concat scheduling-line "SCHEDULED: " scheduled)))
                (when (and scheduled deadline)
                  (setq scheduling-line (concat scheduling-line " ")))
                (when deadline
                  (setq scheduling-line (concat scheduling-line "DEADLINE: " deadline))))
              
              ;; Update the source file
              (with-current-buffer (find-file-noselect path)
                (save-buffer) ;; Save any changes first
                (let ((modified-buffer (buffer-modified-p)))
                  (save-excursion
                    (goto-char (point-min))
                    (let ((search-pattern (format "\\(^\\|[ \t]\\)\\s-*\\(TODO\\|DONE\\)\\[%s\\]"
                                                  (regexp-quote todo-id))))
                      (when (re-search-forward search-pattern nil t)
                        ;; Remove existing scheduling comments
                        (let ((todo-line-pos (line-beginning-position))
                              (indent (make-string (current-indentation) ? )))
                          
                          ;; First, delete all scheduling lines after this TODO
                          (forward-line 1)
                          (let ((delete-count 0)
                                (start-pos (point)))
                            (while (and (< (point) (point-max))
                                        (or (looking-at (format "^\\s-*[%s]+\\s-*\\(SCHEDULED:\\|DEADLINE:\\)" 
                                                                (regexp-quote (string-trim comment-start))))
                                            (looking-at (format "^\\s-*[%s]+\\s-*.*\\(SCHEDULED:\\|DEADLINE:\\)"
                                                                (regexp-quote (string-trim comment-start))))))
                              (forward-line 1)
                              (setq delete-count (1+ delete-count)))
                            
                            (when (> delete-count 0)
                              (delete-region start-pos (point))))
                          
                          ;; Add new scheduling line if needed
                          (when (not (string-empty-p scheduling-line))
                            (goto-char todo-line-pos)
                            (end-of-line)
                            ;; Ensure comment-start is properly formatted
                            (let ((comment-prefix (if (string-empty-p (string-trim comment-start))
                                                      "# "
                                                    (concat (string-trim comment-start) " "))))
                              (insert "\n" indent comment-prefix scheduling-line)))))))
                  
                  ;; Only save if we made changes or the buffer was already modified
                  (when (or modified-buffer (buffer-modified-p))
                    (save-buffer))))))

          ;; Update the TODO state and text
          (let ((update-result (org-collect-code-todos-update-source-file-by-id
                                path todo-id
                                (if (boundp 'org-state) org-state current-state)
                                org-todo-text last-text)))
            
            ;; If update-result is non-nil, we need to update the TODO_ID property
            (when update-result
              (org-collect-code-todos--with-writable-buffer
               (lambda ()
                 (let ((new-uuid (car update-result))
                       (new-text (cdr update-result)))
                   (org-entry-put (point) "TODO_ID" new-uuid)
                   (org-entry-put (point) "LAST" new-text)
                   (save-buffer))))))))
    (error (message "Error updating source TODO state: %s" (error-message-string error-obj)))))

;;; Safe wrappers for org functions

(defun org-collect-code-todos-safe-archive-subtree (orig-fun &rest args)
  "Safely execute org-archive-subtree with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--with-writable-buffer
     (lambda ()
       (let ((org-archive-location (or org-archive-location
                                       (concat (org-collect-code-todos--get-archive-file)
                                               "::* Archived Tasks"))))
         (org-back-to-heading t)
         (apply orig-fun args)
         (save-buffer))))))

(defun org-collect-code-todos-safe-toggle-tag (orig-fun &rest args)
  "Safely execute org-toggle-tag with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--with-writable-buffer
     (lambda () (apply orig-fun args)))))

(defun org-collect-code-todos-safe-schedule (orig-fun &rest args)
  "Safely execute org-schedule with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--with-writable-buffer
     (lambda () (apply orig-fun args)))))

(defun org-collect-code-todos-safe-deadline (orig-fun &rest args)
  "Safely execute org-deadline with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--with-writable-buffer
     (lambda () (apply orig-fun args)))))


;;; Interactive functions for scheduling

(defun org-collect-code-todos-set-planning-at-point (planning-type)
  "Add or modify a planning timestamp for the TODO at point.
PLANNING-TYPE should be either 'scheduled or 'deadline."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (org-collect-code-todos--debug-log 
     "Setting %s at point in %s with comment-start: '%s'" 
     planning-type (buffer-file-name) comment-start)
    
    (let ((todo (org-collect-code-todos--find-todo-at-point)))
      (when todo
        (let* ((todo-id (or (plist-get todo :id) (org-collect-code-todos--generate-id)))
               (todo-state (plist-get todo :state))
               (todo-text (plist-get todo :text))
               (existing-scheduled (plist-get todo :scheduled))
               (existing-deadline (plist-get todo :deadline))
               (date (org-read-date nil t))
               (date-str (format-time-string "%Y-%m-%d %a" date))
               (new-timestamp (format "<%s>" date-str))
               (new-scheduled (if (eq planning-type 'scheduled) new-timestamp existing-scheduled))
               (new-deadline (if (eq planning-type 'deadline) new-timestamp existing-deadline))
               (current-value (if (eq planning-type 'scheduled) existing-scheduled existing-deadline)))
          
          ;; If no ID exists, update the source line
          (unless (plist-get todo :id)
            (save-excursion
              (beginning-of-line)
              (when (re-search-forward (concat (regexp-quote todo-state) "\\s-+") 
                                       (line-end-position) t)
                (replace-match (concat todo-state "[" todo-id "] ")))))
          
          ;; Log buffer content before update
          (org-collect-code-todos--log-buffer-content 
           (format "Before %s update" (symbol-name planning-type)))
          
          ;; Check if the date is actually changing
          (if (and current-value (string= current-value new-timestamp))
              (message "%s already set to %s" 
                       (capitalize (symbol-name planning-type)) date-str)
            
            ;; Update planning comment
            (org-collect-code-todos--update-scheduling-comments 
             (string-trim comment-start) todo-id new-scheduled new-deadline)
            
            ;; Log buffer content after update
            (org-collect-code-todos--log-buffer-content 
             (format "After %s update" (symbol-name planning-type)))
            
            ;; Update the org file entry if it exists
            (org-collect-code-todos--update-org-scheduling todo-id new-scheduled new-deadline)
            
            ;; Save the buffer
            (org-collect-code-todos--debug-log 
             "About to save buffer after setting %s" (symbol-name planning-type))
            (save-buffer)
            
            ;; Capture apheleia output after save
            (run-with-timer 0.2 nil #'org-collect-code-todos--capture-apheleia-output)
            
            (message "%s set for %s" 
                     (capitalize (symbol-name planning-type)) date-str)))))))

(defun org-collect-code-todos-schedule-at-point ()
  "Add or modify a SCHEDULED timestamp for the TODO at point."
  (interactive)
  (org-collect-code-todos-set-planning-at-point 'scheduled))

(defun org-collect-code-todos-set-deadline-at-point ()
  "Add or modify a DEADLINE timestamp for the TODO at point."
  (interactive)
  (org-collect-code-todos-set-planning-at-point 'deadline))

(defun org-collect-code-todos--update-org-scheduling (todo-id scheduled deadline)
  "Update scheduling information in the org entry with TODO-ID.
SCHEDULED and DEADLINE are timestamp strings or nil."
  (when (file-exists-p org-collect-code-todos-file)
    (org-collect-code-todos--debug-log 
     "Updating org scheduling for TODO[%s]: SCHEDULED=%s, DEADLINE=%s" 
     todo-id (or scheduled "nil") (or deadline "nil"))
    (with-current-buffer (find-file-noselect org-collect-code-todos-file)
      (org-mode)
      (org-collect-code-todos--with-writable-buffer
       (lambda ()
         (save-excursion
           (goto-char (point-min))
           (let ((search-pattern (format ":TODO_ID:\\s-*%s" (regexp-quote todo-id))))
             (org-collect-code-todos--debug-log "Searching for pattern: %s" search-pattern)
             (when (re-search-forward search-pattern nil t)
               (condition-case error-obj
                   (progn
                     (org-back-to-heading t)
                     (org-collect-code-todos--debug-log 
                      "Found heading for TODO[%s] at position %d" todo-id (point))
                     
                     ;; Get current values for comparison
                     (let ((current-scheduled (org-entry-get (point) "SCHEDULED"))
                           (current-deadline (org-entry-get (point) "DEADLINE")))
                       
                       (org-collect-code-todos--debug-log 
                        "Current values: SCHEDULED=%s, DEADLINE=%s" 
                        (or current-scheduled "nil") (or current-deadline "nil"))
                       
                       ;; Handle scheduling changes
                       (if scheduled
                           (progn
                             (org-collect-code-todos--debug-log 
                              "Setting SCHEDULED to %s" scheduled)
                             (org-schedule nil scheduled))
                         (when current-scheduled
                           (org-collect-code-todos--debug-log "Removing SCHEDULED")
                           (org-schedule '(4)))) ;; C-u prefix to remove
                       
                       ;; Handle deadline changes
                       (if deadline
                           (progn
                             (org-collect-code-todos--debug-log 
                              "Setting DEADLINE to %s" deadline)
                             (org-deadline nil deadline))
                         (when current-deadline
                           (org-collect-code-todos--debug-log "Removing DEADLINE")
                           (org-deadline '(4))))) ;; C-u prefix to remove
                     
                     ;; Verify the changes were applied
                     (let ((new-scheduled (org-entry-get (point) "SCHEDULED"))
                           (new-deadline (org-entry-get (point) "DEADLINE")))
                       (org-collect-code-todos--debug-log 
                        "After update: SCHEDULED=%s, DEADLINE=%s" 
                        (or new-scheduled "nil") (or new-deadline "nil")))
                     
                     ;; Save the buffer
                     (save-buffer))
                 (error 
                  (org-collect-code-todos--debug-log 
                   "Error updating org scheduling: %s" (error-message-string error-obj))
                  nil))))))))))

;;; Setup hooks and advice

(defun org-collect-code-todos--capture-apheleia-output ()
  "Capture apheleia formatter output for debugging."
  (when (get-buffer "*apheleia-ruff-log*")
    (with-current-buffer "*apheleia-ruff-log*"
      (let ((log-content (buffer-substring-no-properties (point-min) (point-max))))
        (org-collect-code-todos--debug-log 
         "Apheleia ruff log content:\n%s" log-content)))))

(defun org-collect-code-todos-set-read-only ()
  "Set the code-todos buffer to read-only when opened."
  (when (and org-collect-code-todos-read-only
             (org-collect-code-todos--is-todos-buffer-p))
    (read-only-mode 1)))

(defun org-collect-code-todos-set-archive-location ()
  "Set up the archive location for the code-todos file."
  (when (org-collect-code-todos--is-todos-buffer-p)
    (setq-local org-archive-location
                (concat (org-collect-code-todos--get-archive-file)
                        "::* Archived Tasks"))))


;; Add hooks
(add-hook 'after-save-hook #'org-collect-code-todos-collect-and-add)
(add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos-mark-source-todo-state)
(add-hook 'org-after-schedule-hook #'org-collect-code-todos-mark-source-todo-state)
(add-hook 'org-after-deadline-hook #'org-collect-code-todos-mark-source-todo-state)
(add-hook 'find-file-hook #'org-collect-code-todos-set-read-only)
(add-hook 'find-file-hook #'org-collect-code-todos-set-archive-location)

;; Add advice
(advice-add 'org-todo  :around #'org-collect-code-todos-safe-toggle-tag)
(advice-add 'org-archive-subtree :around #'org-collect-code-todos-safe-archive-subtree)
(advice-add 'org-archive-subtree-default :around #'org-collect-code-todos-safe-archive-subtree)
(advice-add 'org-toggle-tag :around #'org-collect-code-todos-safe-toggle-tag)
(advice-add 'org-schedule :around #'org-collect-code-todos-safe-schedule)
(advice-add 'org-deadline :around #'org-collect-code-todos-safe-deadline)

;; Add advice to capture apheleia errors
(when (fboundp 'apheleia--run-formatter)
  (advice-add 'apheleia--run-formatter :after 
              (lambda (&rest _) 
                (run-with-timer 0.1 nil #'org-collect-code-todos--capture-apheleia-output))))

(provide 'org-collect-code-todos)

;;; org-collect-code-todos.el ends here
