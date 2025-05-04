;;; org-collect-code-todos-test.el --- Tests for org-collect-code-todos -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helper functions in org-collect-code-todos.el

;;; Code:

(require 'ert)
(require 'org-collect-code-todos)

(ert-deftest org-collect-code-todos-test-uuid-generation ()
  "Test UUID generation using org-id."
  (let ((uuid1 (org-collect-code-todos--generate-uuid))
        (uuid2 (org-collect-code-todos--generate-uuid)))
    (should (stringp uuid1))
    (should (stringp uuid2))
    (should (not (string= uuid1 uuid2)))
    (should (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$" uuid1))))

(ert-deftest org-collect-code-todos-test-org-to-source ()
  "Test conversion from org format to source code format."
  (let* ((org-heading "TODO Refactor this function")
         (org-properties '(("TODO_ID" . "5c1ef67f-72c7-487c-88bc-b05c77535b01")
                           ("SCHEDULED" . "<2025-05-04 Sun>")
                           ("DEADLINE" . "<2025-05-04 Sun>")))
         (python-mode t)
         (result (org-collect-code-todos--org-to-source org-heading org-properties)))
    (should (equal (car result) 
                   "# TODO[5c1ef67f-72c7-487c-88bc-b05c77535b01] Refactor this function"))
    (should (member "# SCHEDULED: <2025-05-04 Sun>" result))
    (should (member "# DEADLINE: <2025-05-04 Sun>" result))))

(ert-deftest org-collect-code-todos-test-source-to-org ()
  "Test conversion from source code format to org format."
  (let* ((todo-line "# TODO[5c1ef67f-72c7-487c-88bc-b05c77535b01] Refactor this function")
         (following-lines '("# SCHEDULED: <2025-05-04 Sun>" "# DEADLINE: <2025-05-04 Sun>"))
         (result (org-collect-code-todos--source-to-org todo-line following-lines)))
    (should (equal (car result) "TODO Refactor this function"))
    (should (equal (cdr (assoc "TODO_ID" (cdr result))) 
                   "5c1ef67f-72c7-487c-88bc-b05c77535b01"))
    (should (equal (cdr (assoc "SCHEDULED" (cdr result))) "<2025-05-04 Sun>"))
    (should (equal (cdr (assoc "DEADLINE" (cdr result))) "<2025-05-04 Sun>"))))

(ert-deftest org-collect-code-todos-test-collect-todos-in-buffer ()
  "Test collecting TODOs from a buffer."
  (with-temp-buffer
    ;; Set buffer to python-mode to simulate a programming buffer
    (python-mode)
    
    ;; Insert some TODOs in the buffer
    (insert "# This is a comment\n")
    (insert "def some_function():\n")
    (insert "    # TODO[5c1ef67f-72c7-487c-88bc-b05c77535b01] Refactor this function\n")
    (insert "    # SCHEDULED: <2025-05-04 Sun>\n")
    (insert "    # DEADLINE: <2025-05-04 Sun>\n")
    (insert "    pass\n\n")
    (insert "# TODO[6d2fg78g-83d8-598d-99cd-c16d88646b02] Fix this bug\n")
    (insert "# SCHEDULED: <2025-05-05 Mon>\n")
    
    ;; Collect TODOs
    (let ((todos (org-collect-code-todos--collect-todos-in-buffer)))
      ;; Should find 2 TODOs
      (should (= (length todos) 2))
      
      ;; Check first TODO
      (let ((first-todo (nth 0 todos)))
        (should (string-match "TODO\\[5c1ef67f-72c7-487c-88bc-b05c77535b01\\]" (car first-todo)))
        (should (= (length (cadr first-todo)) 2))
        (should (string-match "SCHEDULED:" (nth 0 (cadr first-todo))))
        (should (string-match "DEADLINE:" (nth 1 (cadr first-todo)))))
      
      ;; Check second TODO
      (let ((second-todo (nth 1 todos)))
        (should (string-match "TODO\\[6d2fg78g-83d8-598d-99cd-c16d88646b02\\]" (car second-todo)))
        (should (= (length (cadr second-todo)) 1))
        (should (string-match "SCHEDULED:" (nth 0 (cadr second-todo))))))))

(ert-deftest org-collect-code-todos-test-ensure-org-file-exists ()
  "Test ensuring the org file exists."
  (let* ((temp-dir (make-temp-file "org-collect-code-todos-test" t))
         (default-directory temp-dir)
         (file-path (expand-file-name "code-todos.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Delete the file if it exists
          (when (file-exists-p file-path)
            (delete-file file-path))
          
          ;; Call the function
          (let ((result (org-collect-code-todos--ensure-org-file-exists)))
            ;; Check that the file exists
            (should (file-exists-p file-path))
            ;; Check that the function returns the file path
            (should (equal result file-path))
            ;; Check the file contents
            (with-temp-buffer
              (insert-file-contents file-path)
              (should (string-match-p "^\\#\\+TITLE: Code TODOs" (buffer-string)))
              (should (string-match-p "^\\* Code TODOs" (buffer-string))))))
      ;; Clean up
      (delete-directory temp-dir t))))

(ert-deftest org-collect-code-todos-test-find-or-create-heading ()
  "Test finding or creating a heading."
  (let* ((temp-dir (make-temp-file "org-collect-code-todos-test" t))
         (default-directory temp-dir)
         (file-path (expand-file-name "test-headings.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Create a test file
          (with-temp-file file-path
            (insert "#+TITLE: Test Headings\n\n")
            (insert "* Existing Heading\n")
            (insert "** Existing Subheading\n"))
          
          ;; Test finding existing heading
          (let ((point (org-collect-code-todos--find-or-create-heading 
                        file-path '("Existing Heading"))))
            (with-current-buffer (find-file-noselect file-path)
              (goto-char point)
              (beginning-of-line)
              (should (looking-at "\\* Existing Heading$"))))
          
          ;; Test finding existing subheading
          (let ((point (org-collect-code-todos--find-or-create-heading 
                        file-path '("Existing Heading" "Existing Subheading"))))
            (with-current-buffer (find-file-noselect file-path)
              (goto-char point)
              (beginning-of-line)
              (should (looking-at "\\*\\* Existing Subheading$"))))
          
          ;; Test creating new heading
          (let ((point (org-collect-code-todos--find-or-create-heading 
                        file-path '("New Heading"))))
            (with-current-buffer (find-file-noselect file-path)
              (goto-char point)
              (beginning-of-line)
              (should (looking-at "\\* New Heading$"))))
          
          ;; Test creating new subheading
          (let ((point (org-collect-code-todos--find-or-create-heading 
                        file-path '("Existing Heading" "New Subheading"))))
            (with-current-buffer (find-file-noselect file-path)
              (goto-char point)
              (beginning-of-line)
              (should (looking-at "\\*\\* New Subheading$")))))
      ;; Clean up
      (delete-directory temp-dir t))))

(provide 'org-collect-code-todos-test)
;;; org-collect-code-todos-test.el ends here
