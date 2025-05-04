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
      (insert (format "[%s] %s\n" 
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (apply #'format message args)))
      (append-to-file (point-min) (point-max) log-file))))

(defun org-collect-code-todos--generate-id ()
  "Generate a unique ID for a TODO item."
  (format "%08x%08x" (random #xffffffff) (random #xffffffff)))

(defun org-collect-code-todos--make-planning-line (scheduled deadline)
  "Create a planning line from SCHEDULED and DEADLINE timestamps."
  (let ((planning-line ""))
    (when scheduled
      (setq planning-line (concat planning-line "SCHEDULED: " scheduled)))
    (when (and scheduled deadline)
      (setq planning-line (concat planning-line " ")))
    (when deadline
      (setq planning-line (concat planning-line "DEADLINE: " deadline)))
    planning-line))

(defun org-collect-code-todos--find-todo-at-point ()
  "Find a TODO comment at point in a source file.
Returns a plist with :id, :state, :text, :scheduled, and :deadline properties,
or nil if no TODO is found."
  (save-excursion
    (beginning-of-line)
    (let ((comment-start-regex (concat "^\\s-*" (regexp-quote (string-trim comment-start)))))
      (when (looking-at comment-start-regex)
        (let ((todo-regex "\\(TODO\\|DONE\\)\\(?:\\[\\([0-9a-f]+\\)\\]\\)?[ \t]+\\(.*\\)"))
          (when (re-search-forward todo-regex (line-end-position) t)
            (let* ((todo-id (match-string-no-properties 2))
                   (todo-state (match-string-no-properties 1))
                   (todo-text (match-string-no-properties 3))
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
              
              (org-collect-code-todos--debug-log 
               "Found TODO at point: id=%s, state=%s, text=%s" 
               (or todo-id "nil") todo-state todo-text)
              
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
    (let ((was-read-only buffer-read-only)
          (inhibit-read-only t))
      (org-collect-code-todos--debug-log 
       "Making buffer writable (was read-only: %s)" was-read-only)
      (when was-read-only
        (read-only-mode -1))
      (unwind-protect
          (progn
            (setq-local org-collect-code-todos-keep-writable t)
            (funcall fn))
        (progn
          (setq-local org-collect-code-todos-keep-writable nil)
          (when was-read-only
            (org-collect-code-todos--debug-log "Restoring read-only mode")
            (read-only-mode 1)))))))

(defun org-collect-code-todos--get-archive-file ()
  "Get the archive file path for code TODOs."
  (or org-collect-code-todos-archive-file
      (concat (file-name-sans-extension
               (expand-file-name org-collect-code-todos-file))
              ".archive.org")))

(defun org-collect-code-todos--extract-todo-properties ()
  "Extract TODO properties from current heading.
Returns a plist with :id, :path, :scheduled, and :deadline properties."
  (save-excursion
    (condition-case err
        (progn
          ;; Only try to use org functions in org-mode buffers
          (unless (derived-mode-p 'org-mode)
            (error "Not in an org buffer"))
            
          (org-back-to-heading t)
          (let* ((heading-content (buffer-substring-no-properties 
                                   (line-beginning-position)
                                   (save-excursion (outline-next-heading) (point))))
                 (todo-id nil)
                 (path nil)
                 (scheduled nil)
                 (deadline nil))
            
            ;; Extract file path from link
            (when (string-match "\\[\\[\\(.+?\\)\\]" heading-content)
              (setq path (match-string 1 heading-content)))
            
            ;; Extract TODO_ID property
            (setq todo-id (org-entry-get (point) "TODO_ID"))
            
            ;; Only try to get scheduling info if we're in org-mode
            (when (derived-mode-p 'org-mode)
              (setq scheduled (org-entry-get (point) "SCHEDULED"))
              (setq deadline (org-entry-get (point) "DEADLINE")))
            
            (list :id todo-id :path path
                  :scheduled scheduled :deadline deadline)))
      (error
       (org-collect-code-todos--debug-log 
        "Error extracting TODO properties at point %d: %s" 
        (point) (error-message-string err))
       (list :id nil :path nil)))))

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
    
    ;; Find TODOs and DONEs in the current buffer
    (save-excursion
      (goto-char (point-min))
      (let ((todo-regex (format "^\\s-*[%s]*\\s-*\\(\\(?:TODO\\|DONE\\)\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\)[ \t]+\\(.*\\)"
                                (regexp-quote comment-start))))
        (while (re-search-forward todo-regex nil t)
          (let* ((existing-id (match-string-no-properties 2))
                 (todo-state (match-string-no-properties 1))
                 (todo-text (string-trim (match-string-no-properties 3)))
                 (file-name (replace-regexp-in-string "[.-]" "_"
                                                      (file-name-nondirectory file-path)))
                 (id (or existing-id (org-collect-code-todos--generate-id)))
                 (org-state (if (string-match-p "^DONE" todo-state) "DONE" "TODO"))
                 (scheduled nil)
                 (deadline nil))
            
            ;; Look for SCHEDULED and DEADLINE comments in the next lines
            (save-excursion
              (forward-line 1)
              (while (and (< (point) (point-max))
                          (looking-at (format "^\\s-*[%s]+\\s-*\\(.*\\)" 
                                              (regexp-quote comment-start))))
                (let ((comment-text (match-string-no-properties 1)))
                  (when (string-match "SCHEDULED:\\s-*\\(<[^>]+>\\)" comment-text)
                    (setq scheduled (match-string 1 comment-text)))
                  (when (string-match "DEADLINE:\\s-*\\(<[^>]+>\\)" comment-text)
                    (setq deadline (match-string 1 comment-text)))
                  (forward-line 1))))
            
            ;; Construct the org entry
            (let ((entry (format "* %s %s :%s:\n:PROPERTIES:\n:TODO_ID: %s\n:END:\n"
                                 org-state todo-text file-name id)))
              
              ;; Add scheduling information if present
              (when (or scheduled deadline)
                (setq entry (concat entry 
                                    (org-collect-code-todos--make-planning-line scheduled deadline)
                                    "\n")))

              ;; Add the file link
              (setq entry (concat entry (format "[[%s][%s]]\n" file-path todo-text)))
              
              ;; If no ID exists, add one to the source file
              (unless existing-id
                (let ((original-prefix (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (match-beginning 1)))
                      (todo-with-id (format "%s[%s] %s"
                                            (if (string-match-p "^DONE" todo-state) "DONE" "TODO")
                                            id todo-text)))
                  (replace-match (concat original-prefix todo-with-id))))
              
              (push entry todos))))))
    
    ;; Process collected TODOs
    (with-current-buffer (find-file-noselect org-collect-code-todos-file)
      (org-mode)
      (org-collect-code-todos--with-writable-buffer
       (lambda ()
         ;; Make sure the file has a title if it's empty
         (when (= (buffer-size) 0)
           (insert "#+TITLE: Code TODOs\n#+STARTUP: overview\n\n"))
           
         ;; First, collect all TODO IDs from the current source file
         (let ((source-todo-ids (mapcar
                                 (lambda (todo)
                                   (let* ((todo-lines (split-string todo "\n"))
                                          (id-line (nth 2 todo-lines)))
                                     (when (string-match ":TODO_ID:\\s-*\\(.*\\)" id-line)
                                       (match-string 1 id-line))))
                                 todos)))
           
           ;; Archive TODOs that reference this file but aren't in the source anymore
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
                     ;; Update the heading text if it has changed
                     (let ((current-heading-text (org-get-heading t t t t)))
                       (when (and todo-text
                                  (not (string= current-heading-text todo-text)))
                         (org-edit-headline todo-text))))))

               ;; Add new entry if needed
               (unless existing-entry-found
                 (goto-char (point-max))
                 (insert "\n" todo)))))

         (save-buffer))))))

(defun org-collect-code-todos--archive-deleted-todos (file-path active-todo-ids)
  "Archive TODOs from the org file that reference FILE-PATH but aren't in ACTIVE-TODO-IDS."
  (save-excursion
    (goto-char (point-min))
    (let ((file-path-regexp (regexp-quote file-path))
          (archived-count 0)
          (org-archive-location (concat (org-collect-code-todos--get-archive-file)
                                        "::* Deleted TODOs")))
      
      (goto-char (point-min))
      (while (re-search-forward file-path-regexp nil t)
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (let* ((props (org-collect-code-todos--extract-todo-properties))
                     (todo-id (plist-get props :id))
                     (path (plist-get props :path)))
                
                (when (and path 
                           (string= path file-path)
                           todo-id
                           (not (member todo-id active-todo-ids)))
                  ;; Add a note about why it was archived
                  (org-entry-put (point) "ARCHIVED_REASON" "Deleted from source code")
                  (org-archive-subtree)
                  (setq archived-count (1+ archived-count)))
                
                (org-end-of-subtree t t)))
          (error (forward-line 1))))
      
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
  "Toggle the TODO/DONE state of the TODO comment at point by operating on org file."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (org-collect-code-todos--debug-log "Toggling TODO state at point in %s" (buffer-file-name))
    (let ((todo (org-collect-code-todos--find-todo-at-point)))
      (when todo
        (let* ((todo-id (or (plist-get todo :id) (org-collect-code-todos--generate-id)))
               (todo-state (plist-get todo :state))
               (todo-text (plist-get todo :text))
               (source-buffer (current-buffer))
               (source-point (point)))
          
          ;; If no ID exists, update the source line with ID
          (unless (plist-get todo :id)
            (save-excursion
              (beginning-of-line)
              (when (re-search-forward (concat (regexp-quote todo-state) "\\s-+")
                                       (line-end-position) t)
                (replace-match (concat todo-state "[" todo-id "] ")))
              (save-buffer)))
          
          ;; Find or create the org entry
          (let ((entry (org-collect-code-todos--find-todo-by-id todo-id)))
            (if entry
                ;; Jump to the org entry (invisibly)
                (with-current-buffer (car entry)
                  (save-window-excursion
                    (goto-char (cdr entry))
                    
                    ;; Toggle the TODO state in org
                    (let ((current-state (org-get-todo-state)))
                      (org-collect-code-todos--debug-log 
                       "Toggling org entry state from %s to %s" 
                       current-state (if (string= current-state "TODO") "DONE" "TODO"))
                      (org-todo (if (string= current-state "TODO") "DONE" "TODO")))
                    
                    ;; Save the org buffer
                    (save-buffer)))
              
              ;; If entry not found, create it first
              (message "Creating org entry for this TODO first...")
              (org-collect-code-todos-collect-and-add)
              ;; Then try again
              (run-with-timer 0.2 nil 
                              (lambda ()
                                (with-current-buffer source-buffer
                                  (goto-char source-point)
                                  (org-collect-code-todos-toggle-state-at-point)))))))))))

(defun org-collect-code-todos--get-heading-text-without-todo (heading)
  "Extract the heading text without the TODO keyword."
  (if (and heading (string-match "\\(?:TODO\\|DONE\\)\\s-+\\(.*\\)" heading))
      (match-string 1 heading)
    heading))

(defun org-collect-code-todos--get-todo-state-from-heading (heading)
  "Extract the TODO state from a heading."
  (if (and heading (string-match "\\(TODO\\|DONE\\)" heading))
      (match-string 1 heading)
    "TODO"))  ; Default to TODO if no state found

(defun org-collect-code-todos--get-id-from-properties ()
  "Get the TODO_ID property from the current org entry."
  (if (derived-mode-p 'org-mode)
      (org-entry-get nil "TODO_ID")
    nil))

(defun org-collect-code-todos--org-to-source-format (org-heading org-scheduled org-deadline comment-prefix indent todo-id)
  "Convert org format to source code comment format.
ORG-HEADING is the org heading with TODO state.
ORG-SCHEDULED and ORG-DEADLINE are the scheduling timestamps.
COMMENT-PREFIX is the comment character.
INDENT is the indentation string.
TODO-ID is the ID of the TODO item."
  (let* ((todo-state (or (org-collect-code-todos--get-todo-state-from-heading org-heading) "TODO"))
         (todo-text (org-collect-code-todos--get-heading-text-without-todo org-heading))
         (source-line (format "%s%s %s[%s] %s" indent comment-prefix todo-state todo-id todo-text))
         (scheduling-lines ""))

    (org-collect-code-todos--debug-log 
     "Converting to source format: state=%s, text=%s" todo-state todo-text)

    ;; Add scheduling information if present
    (when org-scheduled
      (setq scheduling-lines
            (concat scheduling-lines
                    (format "\n%s%s SCHEDULED: %s" indent comment-prefix org-scheduled))))

    (when org-deadline
      (setq scheduling-lines
            (concat scheduling-lines
                    (format "\n%s%s DEADLINE: %s" indent comment-prefix org-deadline))))

    (concat source-line scheduling-lines)))


;; Removed function org-collect-code-todos--update-scheduling-comments
;; as it's now handled by org-collect-code-todos-mark-source-todo-state

;; Removed function org-collect-code-todos-update-source-file-by-id
;; as it's now handled by org-collect-code-todos-mark-source-todo-state

(defun org-collect-code-todos-mark-source-todo-state ()
  "Update TODO/DONE state and scheduling in source file when changed in code-todos.org."
  (when (and (derived-mode-p 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p))
    (org-collect-code-todos--debug-log
     "Checking for changes to sync to source file at point %d" (point))
    
    (condition-case error-obj
        (let* ((props (org-collect-code-todos--extract-todo-properties))
               (todo-id (plist-get props :id))
               (path (plist-get props :path))
               (org-heading nil)
               (current-state nil)
               (scheduled nil)
               (deadline nil))
          
          ;; Only get org-specific properties if we're in org-mode
          (when (derived-mode-p 'org-mode)
            (setq org-heading (org-get-heading t t t t))
            (setq current-state (org-get-todo-state))
            (setq scheduled (org-entry-get nil "SCHEDULED"))
            (setq deadline (org-entry-get nil "DEADLINE")))

          (org-collect-code-todos--debug-log
           "Extracted properties: id=%s, path=%s, state=%s, scheduled=%s, deadline=%s"
           todo-id path current-state scheduled deadline)

          (when (and todo-id path (file-exists-p path))
            (with-current-buffer (find-file-noselect path)
              ;; Find the TODO line
              (goto-char (point-min))
              (when (re-search-forward (format "\\[%s\\]" (regexp-quote todo-id)) nil t)
                (let* ((line-start (line-beginning-position))
                       (indent (make-string (current-indentation) ? ))
                       (comment-prefix (concat (string-trim comment-start) " "))
                       ;; Generate the new source format from org data
                       (new-source-format
                        (org-collect-code-todos--org-to-source-format
                         org-heading scheduled deadline comment-prefix indent todo-id)))

                  (org-collect-code-todos--debug-log
                   "Updating source TODO[%s] with new format: %s"
                   todo-id new-source-format)

                  ;; Replace the entire TODO block (including scheduling)
                  (beginning-of-line)
                  (let ((start-pos (point)))
                    ;; Find the end of the TODO block (including scheduling lines)
                    (forward-line 1)
                    (while (and (< (point) (point-max))
                                (looking-at (format "^\\s-*%s\\s-*\\(SCHEDULED:\\|DEADLINE:\\)"
                                                    (regexp-quote (string-trim comment-start)))))
                      (forward-line 1))

                    ;; Replace with new format
                    (delete-region start-pos (point))
                    (goto-char start-pos)
                    (insert new-source-format))

                  (save-buffer))))))
      (error
       (org-collect-code-todos--debug-log
        "Error updating source TODO: %s" (error-message-string error-obj))
       (message "Error updating source TODO: %s" (error-message-string error-obj))))))

;;; Safe wrappers for org functions

(defun org-collect-code-todos-safe-archive-subtree (orig-fun &rest args)
  "Safely execute org-archive-subtree with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--debug-log "Safe archive subtree called")
    (org-collect-code-todos--with-writable-buffer
     (lambda ()
       (let ((org-archive-location (or org-archive-location
                                       (concat (org-collect-code-todos--get-archive-file)
                                               "::* Archived Tasks"))))
         (when (derived-mode-p 'org-mode)
           (org-back-to-heading t)
           (apply orig-fun args)
           (save-buffer)))))))

(defun org-collect-code-todos-safe-toggle-tag (orig-fun &rest args)
  "Safely execute org-toggle-tag with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--debug-log "Safe toggle tag called")
    (org-collect-code-todos--with-writable-buffer
     (lambda () 
       (when (derived-mode-p 'org-mode)
         (apply orig-fun args))))))

(defun org-collect-code-todos-safe-schedule (orig-fun &rest args)
  "Safely execute org-schedule with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--debug-log "Safe schedule called")
    (org-collect-code-todos--with-writable-buffer
     (lambda () 
       (when (derived-mode-p 'org-mode)
         (apply orig-fun args))))))

(defun org-collect-code-todos-safe-deadline (orig-fun &rest args)
  "Safely execute org-deadline with proper read-only handling."
  (if (not (org-collect-code-todos--is-todos-buffer-p))
      (apply orig-fun args)
    (org-collect-code-todos--debug-log "Safe deadline called")
    (org-collect-code-todos--with-writable-buffer
     (lambda () 
       (when (derived-mode-p 'org-mode)
         (apply orig-fun args))))))


;;; Interactive functions for scheduling

(defun org-collect-code-todos-set-planning-at-point (planning-type)
  "Add or modify a planning timestamp for the TODO at point via org file.
PLANNING-TYPE should be either 'scheduled or 'deadline."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let ((todo (org-collect-code-todos--find-todo-at-point)))
      (when todo
        (let* ((todo-id (or (plist-get todo :id) (org-collect-code-todos--generate-id)))
               (todo-state (plist-get todo :state))
               (todo-text (plist-get todo :text))
               (source-buffer (current-buffer))
               (source-point (point)))

          (org-collect-code-todos--debug-log
           "Setting %s for TODO[%s]" planning-type (or todo-id "new"))

          ;; If no ID exists, update the source line with ID
          (unless (plist-get todo :id)
            (save-excursion
              (beginning-of-line)
              (when (re-search-forward (concat (regexp-quote todo-state) "\\s-+")
                                       (line-end-position) t)
                (replace-match (concat todo-state "[" todo-id "] ")))
              (save-buffer)))

          ;; Find or create the org entry
          (let ((entry (org-collect-code-todos--find-todo-by-id todo-id)))
            (if entry
                ;; Jump to the org entry (invisibly) and set planning
                (with-current-buffer (car entry)
                  (save-window-excursion
                    (goto-char (cdr entry))

                    ;; Set the planning info in the org file
                    (cond
                     ((eq planning-type 'scheduled)
                      (org-schedule nil))
                     ((eq planning-type 'deadline)
                      (org-deadline nil)))

                    ;; Save the org buffer
                    (save-buffer)))

              ;; If entry not found, create it first
              (message "Creating org entry for this TODO first...")
              (org-collect-code-todos-collect-and-add)
              ;; Then try again
              (run-with-timer 0.2 nil
                              (lambda ()
                                (with-current-buffer source-buffer
                                  (goto-char source-point)
                                  (org-collect-code-todos-set-planning-at-point planning-type))))))

          (message "%s timestamp added" (capitalize (symbol-name planning-type))))))))

(defun org-collect-code-todos-schedule-at-point ()
  "Add or modify a SCHEDULED timestamp for the TODO at point."
  (interactive)
  (org-collect-code-todos-set-planning-at-point 'scheduled))

(defun org-collect-code-todos-set-deadline-at-point ()
  "Add or modify a DEADLINE timestamp for the TODO at point."
  (interactive)
  (org-collect-code-todos-set-planning-at-point 'deadline))


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
    (org-collect-code-todos--debug-log "Setting buffer to read-only")
    (read-only-mode 1)
    (message "Code TODOs buffer is read-only. Use package commands to modify.")))

(defun org-collect-code-todos-set-archive-location ()
  "Set up the archive location for the code-todos file."
  (when (org-collect-code-todos--is-todos-buffer-p)
    (setq-local org-archive-location
                (concat (org-collect-code-todos--get-archive-file)
                        "::* Archived Tasks"))))


;; Define a function to set up all hooks
(defun org-collect-code-todos-setup-hooks ()
  "Set up all hooks needed for org-collect-code-todos."
  ;; For TODO state changes
  (add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos-mark-source-todo-state)
  
  ;; For scheduling changes
  (add-hook 'org-after-schedule-hook #'org-collect-code-todos-mark-source-todo-state)
  (add-hook 'org-after-deadline-hook #'org-collect-code-todos-mark-source-todo-state)
  
  ;; For property changes (in case TODO_ID is modified)
  (add-hook 'org-property-changed-functions
            (lambda (property value)
              (when (and (derived-mode-p 'org-mode)
                         (string= property "TODO_ID"))
                (org-collect-code-todos-mark-source-todo-state))))
  
  ;; For general changes to the org file
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (org-collect-code-todos--is-todos-buffer-p))
                (org-collect-code-todos--debug-log "Org file saved, checking for changes")
                (condition-case err
                    (org-map-entries
                     (lambda ()
                       (when (derived-mode-p 'org-mode)
                         (let ((props (org-collect-code-todos--extract-todo-properties)))
                           (when (and (plist-get props :id) (plist-get props :path))
                             (org-collect-code-todos-mark-source-todo-state)))))
                     nil nil)
                  (error
                   (org-collect-code-todos--debug-log 
                    "Error in after-save-hook: %s" (error-message-string err))))))))

;; Add a safer version of org-element-at-point
(defun org-collect-code-todos--safe-org-element-at-point (&rest args)
  "Safely call org-element-at-point only in org-mode buffers."
  (if (derived-mode-p 'org-mode)
      (apply #'org-element-at-point args)
    (org-collect-code-todos--debug-log 
     "Prevented org-element-at-point in non-org buffer %s" (buffer-name))
    nil))

;; Add hooks
(add-hook 'after-save-hook #'org-collect-code-todos-collect-and-add)
(add-hook 'find-file-hook #'org-collect-code-todos-set-read-only)
(add-hook 'find-file-hook #'org-collect-code-todos-set-archive-location)

;; Add debug advice to track org-element-at-point calls
(defun org-collect-code-todos--debug-org-element-call (&rest args)
  "Debug when org-element-at-point is called."
  (unless (derived-mode-p 'org-mode)
    (org-collect-code-todos--debug-log 
     "org-element-at-point called in non-org buffer %s at %d\nBacktrace: %s" 
     (buffer-name) (point) (backtrace-to-string))))

(advice-add 'org-element-at-point :before #'org-collect-code-todos--debug-org-element-call)
(advice-add 'org-element-at-point :around #'org-collect-code-todos--safe-org-element-at-point)

;; Call the setup function to set up all hooks
(org-collect-code-todos-setup-hooks)

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
