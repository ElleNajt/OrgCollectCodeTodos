;;; org-collect-code-TODO[fe4a3329] s.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: April 29, 2025
;; Modified: April 29, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/elle/org-collect-code-TODO[82129f23] s
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

(require 'uuid)

(defun org-collect-code-todos-make-writable ()
  "Make the code-todos buffer writable temporarily."
  (when (and org-collect-code-todos-read-only
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    (read-only-mode -1)))

(defun org-collect-code-todos-make-read-only ()
  "Make the code-todos buffer read-only again."
  (when (and org-collect-code-todos-read-only
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    (read-only-mode 1)))

(defun org-collect-code-todos-set-read-only ()
  "Set the code-todos buffer to read-only when opened."
  (when (and org-collect-code-todos-read-only
             (buffer-file-name)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file)))
    (read-only-mode 1)))



(defun collect-todos-and-add-to-code-todos ()
  (when (derived-mode-p 'prog-mode)
    (message "Starting TODO collection for %s" (buffer-file-name))
    (save-excursion
      (let ((file-path (buffer-file-name))
            (comment-start (string-trim comment-start))
            todos)
        ;; Handle regular comments
        (goto-char (point-min))
        (while (re-search-forward (format "%s.*\\(TODO\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\)\\s-*\\(.*\\)" (regexp-quote comment-start)) nil t)
          (let* ((line-number (line-number-at-pos))
                 (existing-id (match-string-no-properties 2))
                 (todo-text (string-trim (match-string-no-properties 3)))
                 (file-name (replace-regexp-in-string "[.-]" "_"
                                                      (file-name-nondirectory file-path)))
                 (id (or existing-id (substring (uuid-string) 0 8)))
                 (entry (format "* TODO %s :%s:\n:PROPERTIES:\n:TODO_ID: %s\n:LAST: %s\n:END:\n[[%s::%d][%s]]\n"
                                todo-text
                                file-name
                                id
                                todo-text
                                file-path
                                line-number
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
              (while (re-search-forward "TODO\\(?:\\[\\([0-9a-f]+\\)\\]\\)?\\s-*\\(.*\\)" nil t)
                (let* ((existing-id (match-string-no-properties 1))
                       (todo-text (string-trim (match-string-no-properties 2)))
                       (original-line (+ string-lines (line-number-at-pos (match-beginning 0))))
                       (file-name (replace-regexp-in-string "[.-]" "_"
                                                            (file-name-nondirectory file-path)))
                       (id (or existing-id (substring (uuid-string) 0 8)))
                       (entry (format "* TODO %s :%s:\n:PROPERTIES:\n:TODO_ID: %s\n:LAST: %s\n:END:\n[[%s::%d][%s]]\n"
                                      todo-text
                                      file-name
                                      id
                                      todo-text
                                      file-path
                                      original-line
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
                    (org-back-to-heading t)
                    ;; Check the :LAST: property
                    (let ((current-last nil))
                      (save-excursion
                        (when (re-search-forward ":LAST:\\s-*\\(.*\\)" (save-excursion (outline-next-heading) (point)) t)
                          (setq current-last (match-string 1))))
                      
                      ;; Get the current heading text (without TODO keyword and tags)
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
            
            (save-buffer)))))))


(add-hook 'after-save-hook #'collect-todos-and-add-to-code-todos)

(defun mark-source-todo-state ()
  "Update TODO/DONE state in source file when changed in code-todos.org.
If the TODO text has been updated, assign a new UUID."
  (when (and (eq major-mode 'org-mode)
             (string= (buffer-file-name) (expand-file-name org-collect-code-todos-file))
             (member org-state '("TODO" "DONE")))
    (save-excursion
      (org-back-to-heading t)
      (let ((heading-content (buffer-substring-no-properties
                              (point)
                              (save-excursion
                                (outline-next-heading)
                                (point)))))
        (message "Heading content: %s" heading-content)
        (when (string-match "\\[\\[\\(.+?\\)::\\([0-9]+\\)\\]" heading-content)
          (let ((path (match-string 1 heading-content))
                (line (match-string 2 heading-content))
                (todo-id nil)
                (last-text nil))
            
            ;; Extract TODO_ID and LAST property from properties
            (save-excursion
              (org-back-to-heading t)
              (when (re-search-forward ":TODO_ID:\\s-*\\([0-9a-f]+\\)" (save-excursion (outline-next-heading) (point)) t)
                (setq todo-id (match-string 1)))
              (when (re-search-forward ":LAST:\\s-*\\(.*\\)" (save-excursion (outline-next-heading) (point)) t)
                (setq last-text (match-string 1))))

            (let ((org-todo-text (org-get-heading t t t t)))  ; get current org heading
              (message "org:'%s', last:'%s'" org-todo-text last-text)
              
              ;; Check if the heading text has changed from the last saved value
              (let ((text-changed (not (string= org-todo-text last-text)))
                    (new-id (if todo-id todo-id (substring (uuid-string) 0 8))))
                
                (message "Found link - Path: %s, Line: %s, ID: %s, Text changed: %s" 
                         path line todo-id text-changed)
                
                (with-current-buffer (find-file-noselect path)
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))
                  
                  (if todo-id
                      (when (re-search-forward (format "\\(TODO\\|DONE\\)\\[%s\\]\\s-*\\(.*\\)" todo-id) (line-end-position) t)
                        (let ((current-text (match-string 2)))
                          (if text-changed
                              (progn
                                ;; Generate a new UUID for the updated text
                                (let ((new-uuid (substring (uuid-string) 0 8)))
                                  (replace-match (concat org-state "[" new-uuid "] " org-todo-text))
                                  ;; Update the TODO_ID property in the org file
                                  (save-excursion
                                    (with-current-buffer (find-buffer-visiting org-collect-code-todos-file)
                                      (org-back-to-heading t)
                                      (org-entry-put (point) "TODO_ID" new-uuid)
                                      (org-entry-put (point) "LAST" org-todo-text)
                                      (save-buffer)
                                      ;; Make the buffer read-only again
                                      (when org-collect-code-todos-read-only
                                        (read-only-mode 1))))))
                            ;; Just update the TODO/DONE state
                            (replace-match (concat org-state "[" todo-id "] " current-text)))
                          (save-buffer)))
                    
                    ;; No ID found, handle plain TODO
                    (when (re-search-forward "\\(TODO\\|DONE\\)\\s-*\\(.*\\)" (line-end-position) t)
                      (replace-match (concat org-state " " org-todo-text))
                      (save-buffer))))))))))))

(add-hook 'org-after-todo-state-change-hook #'mark-source-todo-state)

;; Make the buffer read-only when opened
(add-hook 'find-file-hook #'org-collect-code-todos-set-read-only)

;; Make the buffer temporarily writable for specific operations
(advice-add 'org-todo :before #'org-collect-code-todos-make-writable)
(advice-add 'org-archive-subtree :before #'org-collect-code-todos-make-writable)
(advice-add 'org-archive-subtree-default :before #'org-collect-code-todos-make-writable)

;; Make the buffer read-only again after operations
(advice-add 'org-todo :after #'org-collect-code-todos-make-read-only)
(advice-add 'org-archive-subtree :after #'org-collect-code-todos-make-read-only)
(advice-add 'org-archive-subtree-default :after #'org-collect-code-todos-make-read-only)

(provide 'org-collect-code-todos)
;;; org-collect-code-TODO[d4ed979c] s.el ends here
