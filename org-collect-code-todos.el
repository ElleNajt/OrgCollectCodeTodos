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

(require 'uuid)
(require 'cl-lib)

;;; Customization

(defgroup org-collect-code-todos nil
  "Collect TODO comments from code files into an org file."
  :group 'org
  :prefix "org-collect-code-todos-")

(defcustom org-collect-code-todos-file (expand-file-name "~/code-todos.org")
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
      (insert (format "[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (apply #'format message args))
      (insert "\n")
      (append-to-file (point-min) (point-max) log-file))))

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
Returns a plist with :id, :path, and :last-text properties."
  (save-excursion
    (org-back-to-heading t)
    (let* ((next-heading-pos (save-excursion
                               (condition-case nil
                                   (outline-next-heading)
                                 (error (goto-char (point-max))))
                               (point)))
           (heading-content (buffer-substring-no-properties (point) next-heading-pos))
           (todo-id nil)
           (path nil)
           (last-text nil))
      
      ;; Extract file path from link
      (when (string-match "\\[\\[\\(.+?\\)\\]" heading-content)
        (setq path (match-string 1 heading-content)))
      
      ;; Extract TODO_ID property
      (when (string-match ":TODO_ID:\\s-*\\([0-9a-f]+\\)" heading-content)
        (setq todo-id (match-string 1 heading-content)))
      
      ;; Extract LAST property
      (when (string-match ":LAST:\\s-*\\(.*\\)" heading-content)
        (setq last-text (match-string 1 heading-content)))
      
      (list :id todo-id :path path :last-text last-text))))

;;; Core functionality

(defun org-collect-code-todos-collect-and-add ()
  "Collect TODOs from current buffer, add them to the org file, and remove deleted ones."
  (when (derived-mode-p 'prog-mode)
    (let ((file-path (buffer-file-name))
          (comment-start (string-trim comment-start))
          todos)
      
      ;; Find TODOs in the current buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward 
                (format "^[%s]+\\(\s*?\\)\\(TODO\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\)[ \t]+\\(.*\\)"
                        (regexp-quote comment-start)) 
                nil t)
          (let* ((existing-id (match-string-no-properties 3))
                 (todo-text (string-trim (match-string-no-properties 4)))
                 (file-name (replace-regexp-in-string "[.-]" "_"
                                                      (file-name-nondirectory file-path)))
                 (id (or existing-id (substring (uuid-string) 0 8)))
                 (entry (format "* TODO %s :%s:\n:PROPERTIES:\n:TODO_ID: %s\n:LAST: %s\n:END:\n[[%s][%s]]\n"
                                todo-text
                                file-name
                                id
                                todo-text
                                file-path
                                todo-text)))
            
            ;; If no ID exists, add one to the source file
            (unless existing-id
              (let ((original-prefix (buffer-substring-no-properties 
                                      (line-beginning-position)
                                      (match-beginning 2)))
                    (todo-with-id (format "TODO[%s] %s" id todo-text)))
                (replace-match (concat original-prefix todo-with-id))))
            
            (push entry todos))))
      
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
             
             ;; Then, find and remove TODOs that reference this file but aren't in the source anymore
             (org-collect-code-todos--remove-deleted-todos file-path source-todo-ids)
             
             ;; Now add/update TODOs from the source file
             (dolist (todo todos)
               (let* ((todo-lines (split-string todo "\n"))
                      (heading-line (car todo-lines))
                      (id-line (nth 2 todo-lines))
                      (todo-id (when (string-match ":TODO_ID:\\s-*\\(.*\\)" id-line)
                                 (match-string 1 id-line)))
                      (todo-text (when (string-match "\\* TODO \\(.*\\) :" heading-line)
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
                     (goto-char (point-max))
                     (insert "\n" todo)))))
             
             (save-buffer)))))))))

(defun org-collect-code-todos--remove-deleted-todos (file-path active-todo-ids)
  "Remove TODOs from the org file that reference FILE-PATH but aren't in ACTIVE-TODO-IDS."
  (save-excursion
    (goto-char (point-min))
    (let ((file-path-regexp (regexp-quote file-path))
          (removed-count 0))
      (while (re-search-forward file-path-regexp nil t)
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (let* ((props (org-collect-code-todos--extract-todo-properties))
                     (todo-id (plist-get props :id))
                     (path (plist-get props :path)))
                
                ;; If this entry references our file but its ID isn't in active-todo-ids, delete it
                (when (and path 
                           (string= path file-path)
                           todo-id
                           (not (member todo-id active-todo-ids)))
                  (org-collect-code-todos--debug-log 
                   "Removing TODO with ID %s (not found in source)" todo-id)
                  (let ((start (point)))
                    (org-end-of-subtree t t)
                    (delete-region start (point))
                    (setq removed-count (1+ removed-count))))))
          (error nil)))
      
      (when (> removed-count 0)
        (message "Removed %d TODOs that no longer exist in %s" 
                 removed-count (file-name-nondirectory file-path))))))

(defun org-collect-code-todos-update-source-file-by-id (path todo-id org-state org-todo-text last-text)
  "Update TODO state in source file by searching for its ID.
PATH is the source file path.
TODO-ID is the unique identifier for the TODO.
ORG-STATE is the new state (TODO or DONE).
ORG-TODO-TEXT is the text of the TODO item.
LAST-TEXT is the previous text of the TODO item."
  (with-current-buffer (find-file-noselect path)
    (let ((text-changed (not (string= org-todo-text last-text)))
          (found nil)
          (result nil))
      
      ;; Search for the TODO[c40ac004] with the specific ID
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward (format "\\(^\\|[ \t]\\)\\(TODO\\|DONE\\)\\[%s\\][ \t]+\\(.*\\)"
                                             (regexp-quote todo-id))
                                     nil t))
        (setq found t)
        (let ((leading-space (match-string 1))
              (current-text (match-string 3)))
          (if text-changed
              ;; Text changed - generate new UUID and update everything
              (let ((new-uuid (substring (uuid-string) 0 8)))
                (replace-match (concat leading-space org-state "[" new-uuid "] " org-todo-text))
                (setq result (cons new-uuid org-todo-text)))
            ;; Just update the state
            (replace-match (concat leading-space org-state "[" todo-id "] " current-text)))))
      
      ;; Save the buffer if we made changes
      (when found
        (save-buffer))
      
      result)))

(defun org-collect-code-todos-mark-source-todo-state ()
  "Update TODO/DONE state in source file when changed in code-todos.org."
  (when (and (eq major-mode 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p)
             (member org-state '("TODO" "DONE")))
    (condition-case nil
        (let* ((props (org-collect-code-todos--extract-todo-properties))
               (todo-id (plist-get props :id))
               (path (plist-get props :path))
               (last-text (plist-get props :last-text))
               (org-todo-text (org-get-heading t t t t)))
          
          (when (and todo-id path)
            ;; Update the source file
            (let ((update-result (org-collect-code-todos-update-source-file-by-id
                                  path todo-id org-state org-todo-text last-text)))
              
              ;; If update-result is non-nil, we need to update the TODO_ID property
              (when update-result
                (org-collect-code-todos--with-writable-buffer
                 (lambda ()
                   (let ((new-uuid (car update-result))
                         (new-text (cdr update-result)))
                     (org-entry-put (point) "TODO_ID" new-uuid)
                     (org-entry-put (point) "LAST" new-text)
                     (save-buffer))))))))
      (error (message "Error updating source TODO state: %s" (error-message-string err))))))

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


;;; Setup hooks and advice

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
(add-hook 'find-file-hook #'org-collect-code-todos-set-read-only)
(add-hook 'find-file-hook #'org-collect-code-todos-set-archive-location)

;; Add advice
(advice-add 'org-todo  :around #'org-collect-code-todos-safe-toggle-tag)
(advice-add 'org-archive-subtree :around #'org-collect-code-todos-safe-archive-subtree)
(advice-add 'org-archive-subtree-default :around #'org-collect-code-todos-safe-archive-subtree)
(advice-add 'org-toggle-tag :around #'org-collect-code-todos-safe-toggle-tag)

(provide 'org-collect-code-todos)

;;; org-collect-code-todos.el ends here
