;;; cerbere-go-test-test.el --- Tests for Cerbere Go lang backend

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Copyright (C) 2014  Nicolas Lamirault

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


(require 'cerbere)
(require 'cerbere-gotest)

(defun go-test-command (&rest arg)
  (apply 'concat "go test " arg))



;; cerbere-go-test

(ert-deftest test-cerbere-go-test ()
  (with-temp-buffer
    (go-mode)
    (should (featurep 'cerbere-gotest))))


;; Arguments

(ert-deftest test-go-test-get-program-without-args ()
  (should (string= (go-test-command)
		   (cerbere--go-test-get-program
		    (cerbere--go-test-arguments "")))))

(ert-deftest test-go-test-add-verbose-argument ()
  (let ((cerbere-go-test-verbose t))
    (should (string= (go-test-command " -v")
		     (cerbere--go-test-get-program
		      (cerbere--go-test-arguments ""))))))

(ert-deftest test-go-test-test-at-point ()
  (cerbere-with-test-content "go-test/hello_test.go"
    (should-not (cerbere--go-test-test-at-point))
    (forward-line 4)
    (let ((test (cerbere--go-test-test-at-point)))
      (should (equal "TestReverse" (cerbere--go-test-test-name test)))
      (should (equal "hello_test.go" (cerbere--go-test-test-file test)))
      (should (equal (concat cerbere-test-path "data/go-test/") (cerbere--go-test-test-root test)))))
  (cerbere-with-test-content "go-test/hello.go"
        (goto-char (point-max))
    (should-not (cerbere--go-test-test-at-point))))

(ert-deftest test-go-test-test-for-file ()
  (cerbere-with-test-content "go-test/hello_test.go"
    (let ((test (cerbere--go-test-test-for-file)))
      (should-not (cerbere--go-test-test-name test))
      (should (equal "hello_test.go" (cerbere--go-test-test-file test)))
      (should (equal (concat cerbere-test-path "data/go-test/") (cerbere--go-test-test-root test)))))
  (cerbere-with-test-content "go-test/hello.go"
    (goto-char (point-max))
    (should-not (cerbere--go-test-test-for-file))))

(ert-deftest test-go-test-test-for-project ()
  (cerbere-with-test-content "go-test/hello_test.go"
    (let ((test (cerbere--go-test-test-for-project)))
      (should-not (cerbere--go-test-test-name test))
      (should-not (cerbere--go-test-test-file test))
      (should (equal (concat cerbere-test-path "data/go-test/") (cerbere--go-test-test-root test))))
  (cerbere-with-test-content "go-test/hello.go"
    (goto-char (point-max))
    (let ((test (cerbere--go-test-test-for-project)))
      (should-not (cerbere--go-test-test-name test))
      (should-not (cerbere--go-test-test-file test))
      (should (equal (concat cerbere-test-path "data/go-test/") (cerbere--go-test-test-root test)))))))

(provide 'gotest-test)
;;; cerbere-go-test-test.el ends here
