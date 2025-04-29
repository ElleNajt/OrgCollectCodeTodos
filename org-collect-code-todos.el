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

(require 'uuid)



(defun collect-todos-and-add-to-code-todos ()
  (when (derived-mode-p 'prog-mode)
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
                  
                  ;; If this is a new TODO without an ID, we need to update the original buffer
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

        (when todos
          (with-current-buffer (find-file-noselect org-collect-code-todos-file)
            (goto-char (point-max))
            (dolist (todo todos)
              (unless (save-excursion
                        (goto-char (point-min))
                        (search-forward todo nil t))
                (insert "\n" todo)))
            (save-buffer)))))))


(add-hook 'after-save-hook #'collect-todos-and-add-to-code-todos)

(defun mark-source-todo-state ()
  "Update TODO/DONE state in source file when changed in code-todos.org."
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
                (todo-id nil))
            
            ;; Extract TODO_ID from properties
            (save-excursion
              (org-back-to-heading t)
              (when (re-search-forward ":TODO_ID:\\s-*\\([0-9]+\\)" (save-excursion (outline-next-heading) (point)) t)
                (setq todo-id (match-string 1))))
            
            (message "Found link - Path: %s, Line: %s, ID: %s" path line todo-id)
            (with-current-buffer (find-file-noselect path)
              (goto-char (point-min))
              (forward-line (1- (string-to-number line)))

              (if todo-id
                  (when (re-search-forward (format "\\(TODO\\|DONE\\)\\[%s\\]" todo-id) (line-end-position) t)
                    (replace-match (concat org-state "[" todo-id "]"))
                    (save-buffer))
                (when (re-search-forward "\\(TODO\\|DONE\\)" (line-end-position) t)
                  (replace-match org-state)
                  (save-buffer))))))))))

(add-hook 'org-after-todo-state-change-hook #'mark-source-todo-state)


(provide 'org-collect-code-todos)
;;; org-collect-code-todos.el ends here
