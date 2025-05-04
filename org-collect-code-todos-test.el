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

(provide 'org-collect-code-todos-test)
;;; org-collect-code-todos-test.el ends here
