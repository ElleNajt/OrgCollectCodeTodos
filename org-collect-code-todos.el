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



(defun collect-todos-and-add-to-code-todos ()
  (when (derived-mode-p 'prog-mode)
    (save-excursion
      (let ((file-path (buffer-file-name))
            (comment-start (string-trim comment-start))
            todos)
        ;; Handle regular comments
        (goto-char (point-min))
        (while (re-search-forward (format "%s.*\\(TODO\\)\\s-*\\(.*\\)" (regexp-quote comment-start)) nil t)
          (let* ((line-number (line-number-at-pos))
                 (todo-text (string-trim (match-string-no-properties 2)))
                 (file-name (replace-regexp-in-string "[.-]" "_"
                                                      (file-name-nondirectory file-path)))
                 (entry (format "* TODO %s :%s:\n[[%s::%d][%s]]\n"
                                todo-text
                                file-name
                                file-path
                                line-number
                                todo-text)))
            (push entry todos)))

        ;; Handle string literals
        (goto-char (point-min))
        (while (re-search-forward "\\(\"\"\"\\|'''\\)\\(\\(?:.\\|\n\\)*?\\)\\1" nil t)
          (let ((string-text (match-string-no-properties 2)))
            (with-temp-buffer
              (insert string-text)
              (goto-char (point-min))
              (while (re-search-forward "TODO\\s-*\\(.*\\)" nil t)
                (let* ((todo-text (string-trim (match-string 1)))
                       (original-line (line-number-at-pos (match-beginning 0)))
                       (file-name (replace-regexp-in-string "[.-]" "_"
                                                            (file-name-nondirectory file-path)))
                       (entry (format "* TODO %s :%s:\n[[%s::%d][%s]]\n"
                                      todo-text
                                      file-name
                                      file-path
                                      original-line
                                      todo-text)))
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
                (line (match-string 2 heading-content)))
            (message "Found link - Path: %s, Line: %s" path line)
            (with-current-buffer (find-file-noselect path)
              (goto-char (point-min))
              (forward-line (1- (string-to-number line)))

              (when (re-search-forward "\\(TODO\\|DONE\\)" (line-end-position) t)
                (replace-match org-state)
                (save-buffer)
                ))))))))

(add-hook 'org-after-todo-state-change-hook #'mark-source-todo-state)

(provide 'org-collect-code-todos)
;;; org-collect-code-todos.el ends here
