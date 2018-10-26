;;; cerbere-go-test.el --- Launch GO unit tests

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cerbere-common)
(require 'go-mode)

(defgroup cerbere-go nil
  "Golang back-end for Cerbere."
  :group 'cerbere)

;; (defcustom go-test-args ""
;;   "Argument to pass to go."
;;   :type 'string
;;   :group 'cerbere-go)

(defcustom cerbere-go-test-verbose nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'cerbere-go)


;; Commands
;; -----------

(defun cerbere--go-test-get-program (args)
  "Return the command to launch unit test.
ARGS corresponds to go command line arguments."
  (concat go-command " test "
            ;;(go-test-get-root-directory)
            args))

(defun cerbere--go-test-test-project ()
  "Return the directory for the current file."
  (file-name-directory (buffer-file-name)))

(defun cerbere--go-test-test-root (test)
  "Return the root directory for TEST."
  (plist-get test :project))

(defun cerbere--go-test-test-file (test)
  "Return the file name for TEST."
  (plist-get test :file))

(defun cerbere--go-test-test-name (test)
  "Return the test name for TEST."
  (plist-get test :name))


(defun cerbere--go-test-get-current-file (&optional file)
  "Return the filename of the go test for `FILE'."
  (file-name-nondirectory (buffer-file-name)))

(defun cerbere--go-test-get-current-test ()
  "Return the test name at current point."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "\\s-*func\\s-+\\(Test\\w+\\)" nil 't)
      (let ((test-name (match-string 1)))
        (set-text-properties 0 (length test-name) nil test-name)
        test-name))))

(defun cerbere--go-test-arguments (test)
  "Return arguments for running TEST."
  (let ((verbose (if cerbere-go-test-verbose " -v" ""))
        (test-file (and test (plist-get test :file)))
        (test-name (and test (plist-get test :test))))
    (if test-name (format "%s -run %s" verbose test-name)
      (if test-file (format "%s -file=%s" verbose test-file)
        verbose))))

(defun cerbere--go-test-run (test)
  "Run go TEST."
  (let ((default-directory (cerbere--go-test-test-root test)))
    (cerbere--build (cerbere--go-test-get-program
		     (cerbere--go-test-arguments test)))))

; API
;; ----

(defun cerbere--go-test-test-at-point ()
  "Return the test at point.

Return a proprety list containing the test name and the test class for
the current buffer point, nil if there are no test."
  (let ((name (cerbere--go-test-get-current-test)))
    (when name
      (list :backend 'go-test
            :project (cerbere--go-test-test-project)
            :file (cerbere--go-test-get-current-file)
            :name (cerbere--go-test-get-current-test)))))

(defun cerbere--go-test-test-for-file ()
  "Return the test for the current buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((name (cerbere--go-test-get-current-test)))
      (when name
        (list :backend 'go-test
              :project (cerbere--go-test-test-project)
              :file (cerbere--go-test-get-current-file))))))

(defun cerbere--go-test-test-for-project ()
  "Return the test for the current project."
  (let ((root (cerbere--go-test-test-project)))
    (when root
      (list :backend 'go-test
            :project root))))

(defun cerbere--go-test-run-test (test)
  "Launch Go-Test on `TEST'."
  (cerbere--go-test-run test))

(cerbere-define-backend go-test "go"
  "Cerbere backend that runs go-test tests."
  :run-test #'cerbere--go-test-run-test
  :test-at-point #'cerbere--go-test-test-at-point
  :test-for-file #'cerbere--go-test-test-for-file
  :test-for-project #'cerbere--go-test-test-for-project)

(provide 'cerbere-go-test)
;;; cerbere-go-test.el ends here
