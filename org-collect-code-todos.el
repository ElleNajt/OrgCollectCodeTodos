;;; org-collect-code-todos.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: April 29, 2025
;; Modified: April 29, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/elle/org-collect-code-todos
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defgroup org-collect-code-todos nil
  "Collect TODO comments from code files into an org file."
  :group 'org
  :prefix "org-collect-code-todos-")

(defcustom org-collect-code-todos-file (expand-file-name "~/code-todos.org")
  "File path where code TODOs will be collected."
  :type 'file
  :group 'org-collect-code-todos)

(defcustom org-collect-code-todos-read-only t
  "Whether the code-todos.org file should be read-only by default.
When enabled, the file is read-only except when marking TODOs as done or archiving."
  :type 'boolean
  :group 'org-collect-code-todos)

(defvar-local org-collect-code-todos-keep-writable nil
  "When non-nil, prevents the buffer from being made read-only.
This is used during operations like changing TODO states or archiving.")

(require 'uuid)
(require 'cl-lib)

(defun org-collect-code-todos--is-todos-buffer-p ()
  "Check if current buffer is the code-todos file."
  (and (buffer-file-name)
       (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file))))

(defun org-collect-code-todos-make-writable ()
  "Make the code-todos buffer writable temporarily."
  (when (and org-collect-code-todos-read-only
             (org-collect-code-todos--is-todos-buffer-p))
    (read-only-mode -1)))

(defun org-collect-code-todos-make-read-only ()
  "Make the code-todos buffer read-only again."
  (when (and org-collect-code-todos-read-only
             (org-collect-code-todos--is-todos-buffer-p))
    (read-only-mode 1)))

(defun org-collect-code-todos-set-read-only ()
  "Set the code-todos buffer to read-only when opened."
  (when (and org-collect-code-todos-read-only
             (org-collect-code-todos--is-todos-buffer-p))
    (read-only-mode 1)))



(defun org-collect-code-todos-collect-and-add ()
  (when (derived-mode-p 'prog-mode)
    (message "Starting TODO collection for %s" (buffer-file-name))
    (save-excursion
      (let ((file-path (buffer-file-name))
            (comment-start (string-trim comment-start))
            todos)
        ;; Handle regular comments - only match TODOs with spaces before and after
        (goto-char (point-min))
        (while (re-search-forward (format "%s.*[ \t]\\(TODO\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\)[ \t]\\(.*\\)" (regexp-quote comment-start)) nil t)
          (let* ((line-number (line-number-at-pos))
                 (existing-id (match-string-no-properties 2))
                 (todo-text (string-trim (match-string-no-properties 3)))
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
            (message "Found TODO: '%s' with ID: %s at line %d" todo-text id line-number)
            ;; If no ID exists, add one to the source file
            (unless existing-id
              (let ((original-prefix (buffer-substring-no-properties 
                                      (line-beginning-position)
                                      (match-beginning 1)))
                    (todo-with-id (format "TODO[%s] %s" 
                                          id
                                          todo-text)))
                (replace-match (concat original-prefix todo-with-id))))
            (push entry todos)))

        ;; Handle string literals
        (goto-char (point-min))
        (while (re-search-forward "\\(\"\"\"\\|'''\\)\\(\\(?:.\\|\n\\)*?\\)\\1" nil t)
          (let ((string-text (match-string-no-properties 2))
                (string-start-pos (match-beginning 0))
                (string-lines 0))
            ;; Count lines to adjust line numbers correctly
            (save-excursion
              (goto-char string-start-pos)
              (setq string-lines (count-lines string-start-pos (match-beginning 2))))
            
            (with-temp-buffer
              (insert string-text)
              (goto-char (point-min))
              (while (re-search-forward "[ \t]TODO\\(?:\\[\\([0-9a-f]+\\)\\]\\)?[ \t]\\(.*\\)" nil t)
                (let* ((existing-id (match-string-no-properties 1))
                       (todo-text (string-trim (match-string-no-properties 2)))
                       (original-line (+ string-lines (line-number-at-pos (match-beginning 0))))
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
                  
                  ;; If this is a new TODO[eefae85e] without an ID, we need to update the original buffer
                  (unless existing-id
                    (let ((pos-in-original (with-current-buffer (current-buffer)
                                             (save-excursion
                                               (goto-char string-start-pos)
                                               (forward-line (1- (- (line-number-at-pos) 1)))
                                               (move-to-column (current-column))
                                               (point)))))
                      ;; We can't directly modify the string literal here, but we've generated a UUID already
                      ))
                  
                  (push entry todos))))))

        (message "Found %d TODOs in %s" (length todos) file-path)
        (when todos
          (with-current-buffer (find-file-noselect org-collect-code-todos-file)
            (message "Processing TODOs in %s" org-collect-code-todos-file)
            (org-mode)
            ;; Temporarily make the buffer writable
            (when org-collect-code-todos-read-only
              (read-only-mode -1))
            (dolist (todo todos)
              (let* ((todo-lines (split-string todo "\n"))
                     (heading-line (car todo-lines))
                     (id-line (nth 2 todo-lines))
                     (last-line (nth 3 todo-lines))
                     (todo-id (when (string-match ":TODO_ID:\\s-*\\(.*\\)" id-line)
                                (match-string 1 id-line)))
                     (todo-text (when (string-match "\\* TODO \\(.*\\) :" heading-line)
                                  (match-string 1 heading-line)))
                     (last-text (when (string-match ":LAST:\\s-*\\(.*\\)" last-line)
                                  (match-string 1 last-line)))
                     (existing-entry-found nil))
                
                ;; Check if we have an entry with the same ID
                (save-excursion
                  (goto-char (point-min))
                  (when (and todo-id 
                             (re-search-forward (format ":TODO_ID:\\s-*%s" (regexp-quote todo-id)) nil t))
                    (setq existing-entry-found t)
                    ;; Go to the heading of this entry
                    (condition-case nil
                        (org-back-to-heading t)
                      (error
                       (message "Error: Could not find heading for TODO ID %s" todo-id)
                       (setq existing-entry-found nil)))
                    
                    (when existing-entry-found
                    ;; Check the :LAST: property
                    (let ((current-last nil))
                      (save-excursion
                        (when (re-search-forward ":LAST:\\s-*\\(.*\\)" (save-excursion (outline-next-heading) (point)) t)
                          (setq current-last (match-string 1))))
                      
                      ;; Get the current heading text (without TODO[1c76e5ad] keyword and tags)
                      (let ((current-heading-text (org-get-heading t t t t)))
                        ;; If :LAST: matches the current heading text but differs from the code todo-text,
                        ;; update the heading and :LAST: property
                        (message "EXISTING ENTRY - Current heading: '%s', LAST: '%s', New code todo: '%s'" 
                                 current-heading-text current-last todo-text)
                        (when (and current-last 
                                   (string= current-last current-heading-text)
                                   (not (string= current-heading-text todo-text)))
                          (message "UPDATING heading: '%s' -> '%s'" current-heading-text todo-text)
                          (org-edit-headline todo-text)
                          (org-entry-put (point) "LAST" todo-text))))))
                
                ;; If no existing entry was found or updated, add the new entry
                (unless existing-entry-found
                  (message "Adding new entry for TODO: '%s' with ID: %s" todo-text todo-id)
                  (goto-char (point-max))
                  (insert "\n" todo))
                (message "Entry processed: existing=%s, id=%s, text='%s'" 
                         existing-entry-found todo-id todo-text)))
            
            (save-buffer)
            ;; Restore read-only state if needed
            (when org-collect-code-todos-read-only
              (read-only-mode 1))))))))


(add-hook 'after-save-hook #'org-collect-code-todos-collect-and-add)

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
      
      ;; Search for the TODO[a6e3db8b] with the specific ID
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward (format "\\([ \t]*\\)\\(TODO\\|DONE\\)\\[%s\\][ \t]+\\(.*\\)" 
                                             (regexp-quote todo-id))
                                     nil t))
        (setq found t)
        (let ((leading-space (match-string 1))
              (current-text (match-string 3)))
          (if text-changed
              ;; Text changed - generate new UUID and update everything
              (let ((new-uuid (substring (uuid-string) 0 8)))
                (replace-match (concat leading-space org-state "[" new-uuid "] " org-todo-text))
                ;; Return the new UUID to update the org entry
                (setq result (cons new-uuid org-todo-text)))
            ;; Just update the state
            (replace-match (concat leading-space org-state "[" todo-id "] " current-text))
            ;; Return nil to indicate no ID change needed
            (setq result nil))))
      
      ;; Save the buffer if we made changes
      (when found
        (save-buffer))
      
      result)))

(defun org-collect-code-todos-update-source-file (path line todo-id org-state org-todo-text last-text)
  "Update TODO state in source file.
PATH is the source file path.
LINE is the line number.
TODO-ID is the unique identifier for the TODO.
ORG-STATE is the new state (TODO or DONE).
ORG-TODO-TEXT is the text of the TODO item.
LAST-TEXT is the previous text of the TODO item."
  (with-current-buffer (find-file-noselect path)
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))
    
    (let ((text-changed (not (string= org-todo-text last-text)))
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          (found nil))
      
      ;; First try to find a TODO/DONE with ID
      (when todo-id
        (goto-char line-start)
        (when (re-search-forward (format "\\([ \t]*\\)\\(TODO\\|DONE\\)\\[%s\\][ \t]+\\(.*\\)" 
                                         (regexp-quote todo-id))
                                 line-end t)
          (setq found t)
          (let ((leading-space (match-string 1))
                (current-text (match-string 3)))
            (if text-changed
                ;; Text changed - generate new UUID and update everything
                (let ((new-uuid (substring (uuid-string) 0 8)))
                  (replace-match (concat leading-space org-state "[" new-uuid "] " org-todo-text))
                  ;; Return the new UUID to update the org entry
                  (cons new-uuid org-todo-text))
              ;; Just update the state
              (replace-match (concat leading-space org-state "[" todo-id "] " current-text))
              ;; Return nil to indicate no ID change needed
              nil))))
      
      ;; If not found with ID, try to find a plain TODO
      (unless found
        (goto-char line-start)
        (when (re-search-forward "\\([ \t]*\\)\\(TODO\\|DONE\\)[ \t]+\\(.*\\)" line-end t)
          (setq found t)
          (let ((leading-space (match-string 1))
                (current-text (match-string 3)))
            ;; For plain TODOs, always add an ID
            (let ((new-uuid (substring (uuid-string) 0 8)))
              (replace-match (concat leading-space org-state "[" new-uuid "] " 
                                     (if text-changed org-todo-text current-text)))
              ;; Return the new UUID
              (cons new-uuid (if text-changed org-todo-text current-text))))))
      
      ;; Save the buffer if we made changes
      (when found
        (save-buffer)))))

(defun org-collect-code-todos-mark-source-todo-state ()
  "Update TODO/DONE state in source file when changed in code-todos.org.
If the TODO text has been updated, assign a new UUID."
  (cl-block org-collect-code-todos-mark-source-todo-state
  ;; Make sure the buffer is writable for this operation
  (when (and (eq major-mode 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p))
    (org-collect-code-todos-make-writable)
    ;; Ensure we don't immediately revert to read-only
    (setq-local org-collect-code-todos-keep-writable t))
  
  (when (and (eq major-mode 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p)
             (member org-state '("TODO" "DONE")))
    (save-excursion
      (condition-case nil
          (org-back-to-heading t)
        (error
         (message "Error: Not on a heading when changing TODO state")
         (setq org-state nil)
         (cl-return-from org-collect-code-todos-mark-source-todo-state nil)))
      
      (let ((heading-content (buffer-substring-no-properties
                              (point)
                              (save-excursion
                                (condition-case nil
                                    (outline-next-heading)
                                  (error (goto-char (point-max))))
                                (point)))))
        (message "Heading content: %s" heading-content)
        (when (string-match "\\[\\[\\(.+?\\)\\]" heading-content)
          (let ((path (match-string 1 heading-content))
                (todo-id nil)
                (last-text nil))
            
            ;; Extract TODO_ID and LAST property from properties
            (save-excursion
              (condition-case nil
                  (org-back-to-heading t)
                (error
                 (message "Error: Could not go back to heading")
                 (cl-return-from org-collect-code-todos-mark-source-todo-state nil)))
              
              (let ((next-heading-pos (save-excursion
                                        (condition-case nil
                                            (outline-next-heading)
                                          (error (goto-char (point-max))))
                                        (point))))
                (when (re-search-forward ":TODO_ID:\\s-*\\([0-9a-f]+\\)" next-heading-pos t)
                (setq todo-id (match-string 1)))
              (when (re-search-forward ":LAST:\\s-*\\(.*\\)" next-heading-pos t)
                (setq last-text (match-string 1))))

            ;; Only proceed if we have a valid TODO_ID
            (when todo-id
              (let ((org-todo-text (org-get-heading t t t t)))  ; get current org heading
                (message "org:'%s', last:'%s', id:'%s'" org-todo-text last-text todo-id)
                
                ;; Use the centralized function to update the source file
                (let ((update-result (org-collect-code-todos-update-source-file-by-id
                                      path todo-id org-state org-todo-text last-text)))
                  
                  ;; If update-result is non-nil, we need to update the TODO_ID property
                  (when update-result
                    (let ((new-uuid (car update-result))
                          (new-text (cdr update-result)))
                      (save-excursion
                        (with-current-buffer (find-buffer-visiting org-collect-code-todos-file)
                          ;; Temporarily make the buffer writable if needed
                          (let ((was-read-only (and org-collect-code-todos-read-only
                                                    buffer-read-only)))
                            (when was-read-only
                              (read-only-mode -1))
                            (org-back-to-heading t)
                            (org-entry-put (point) "TODO_ID" new-uuid)
                            (org-entry-put (point) "LAST" new-text)
                            (save-buffer)
                            ;; Restore read-only state
                            (when was-read-only
                              (read-only-mode 1))))))))))))))))

(add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos-mark-source-todo-state)

;; Define wrapper functions that accept arguments
(defun org-collect-code-todos-make-writable-with-args (&rest _args)
  "Make the code-todos buffer writable, ignoring any arguments."
  (org-collect-code-todos-make-writable)
  ;; Set a flag to indicate we're in the middle of an operation
  (when (and (eq major-mode 'org-mode)
             (org-collect-code-todos--is-todos-buffer-p))
    (setq-local org-collect-code-todos-keep-writable t)))

(defun org-collect-code-todos-make-read-only-with-args (&rest _args)
  "Make the code-todos buffer read-only, ignoring any arguments."
  ;; Only make read-only if we're not in the middle of a TODO[c907d2d9] state change
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file))
             (not (bound-and-true-p org-collect-code-todos-keep-writable)))
    (org-collect-code-todos-make-read-only)))

;; Fix for org-before-todo-state-change-hook
(defun org-collect-code-todos-before-todo-state-change (&rest _args)
  "Make the code-todos buffer writable before changing TODO state."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    (org-collect-code-todos-make-writable)
    ;; Set a flag to indicate we're in the middle of a TODO[ccaacf04] state change
    (setq-local org-collect-code-todos-keep-writable t)))

;; Add a function to clear the writable flag after TODO[5f9e80e4] state change is complete
(defun org-collect-code-todos-after-todo-state-change (&rest _args)
  "Clear the writable flag after todo state change is complete."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    (setq-local org-collect-code-todos-keep-writable nil)))

;; Add hooks for TODOs state change
(add-hook 'org-before-todo-state-change-hook #'org-collect-code-todos-before-todo-state-change)
(add-hook 'org-after-todo-state-change-hook #'org-collect-code-todos-after-todo-state-change)

;; Make the buffer read-only when opened
(add-hook 'find-file-hook #'org-collect-code-todos-set-read-only)

;; Make the buffer temporarily writable for specific operations
(advice-add 'org-todo :before #'org-collect-code-todos-make-writable-with-args)
;; Archive operations are now handled by our safe wrappers with try-finally

;; Make the buffer read-only again after operations
(defun org-collect-code-todos-delayed-read-only (&rest _args)
  "Make the buffer read-only immediately after operations complete."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    ;; Clear the writable flag
    (setq-local org-collect-code-todos-keep-writable nil)
    ;; Always make read-only
    (org-collect-code-todos-make-read-only)))

;; Create safe wrappers with try-finally logic
(defun org-collect-code-todos-safe-archive-subtree (&rest args)
  "Safely execute org-archive-subtree with proper read-only handling."
  (interactive "P")
  (let ((was-todos-buffer (org-collect-code-todos--is-todos-buffer-p)))
    (when was-todos-buffer
      (org-collect-code-todos-make-writable)
      (setq-local org-collect-code-todos-keep-writable t))
    (unwind-protect
        (condition-case err
            (apply #'org-archive-subtree-default args)
          (error
           (message "Archive error: %s" (error-message-string err))
           (signal (car err) (cdr err))))
      ;; Always run this cleanup code, even if an error occurred
      (when was-todos-buffer
        (setq-local org-collect-code-todos-keep-writable nil)
        (org-collect-code-todos-make-read-only)))))

(defun org-collect-code-todos-safe-archive-subtree-default (&rest args)
  "Safely execute org-archive-subtree-default with proper read-only handling."
  (interactive "P")
  (let ((was-todos-buffer (org-collect-code-todos--is-todos-buffer-p)))
    (when was-todos-buffer
      (org-collect-code-todos-make-writable)
      (setq-local org-collect-code-todos-keep-writable t))
    (unwind-protect
        (condition-case err
            (apply #'org-archive-subtree-default args)
          (error
           (message "Archive error: %s" (error-message-string err))
           (signal (car err) (cdr err))))
      ;; Always run this cleanup code, even if an error occurred
      (when was-todos-buffer
        (setq-local org-collect-code-todos-keep-writable nil)
        (org-collect-code-todos-make-read-only)))))

;; Make the buffer read-only again after operations
(advice-add 'org-todo :after #'org-collect-code-todos-delayed-read-only)

;; Replace the archive functions with our safe versions
(advice-add 'org-archive-subtree :around #'org-collect-code-todos-safe-archive-subtree)
(advice-add 'org-archive-subtree-default :around #'org-collect-code-todos-safe-archive-subtree-default)

(provide 'org-collect-code-todos)

;;; org-collect-code-todos.el ends here
