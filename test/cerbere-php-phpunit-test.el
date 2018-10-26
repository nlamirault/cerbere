;;; cerbere-php-phpunit-test.el --- Unit tests for phpunit backend of Cerbere

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


(require 'cerbere)
(require 'cerbere-php-phpunit)
(require 'test-helper)

(defun phpunit-command (&rest arg)
  (apply 'concat "phpunit -c " "phpunit.xml" arg))

;; cerbere-php-phpunit Mode

(ert-deftest test-cerbere-php-phpunit ()
  (with-temp-buffer
    (should (featurep 'cerbere-php-phpunit))))


;;

(ert-deftest test-cerbere-php-phpunit-get-current-class ()
  (should (string= "PhpUnitTest"
		   (cerbere--php-phpunit-get-current-class "/tmp/foo/PhpUnitTest.php"))))

;; Arguments

(ert-deftest test-cerbere-php-phpunit-get-program-without-args ()
  (should (string= (phpunit-command)
		   (cerbere--php-phpunit-get-program ""
		    (cerbere--php-phpunit-arguments "")))))

(ert-deftest test-php-phpunit-add-stop-on-error-argument ()
  (let ((cerbere-php-phpunit-stop-on-error t))
    (should (string= (phpunit-command " --stop-on-error")
		     (cerbere--php-phpunit-get-program ""
		      (cerbere--php-phpunit-arguments ""))))))

(ert-deftest test-php-phpunit-add-stop-on-failure-argument ()
  (let ((cerbere-php-phpunit-stop-on-failure t))
    (should (string= (phpunit-command " --stop-on-failure")
		     (cerbere--php-phpunit-get-program ""
		      (cerbere--php-phpunit-arguments ""))))))

(ert-deftest test-php-phpunit-add-stop-on-skipped-argument ()
  (let ((cerbere-php-phpunit-stop-on-skipped t))
    (should (string= (phpunit-command " --stop-on-skipped")
		     (cerbere--php-phpunit-get-program ""
		      (cerbere--php-phpunit-arguments ""))))))

(ert-deftest test-php-phpunit-add-verbose-argument ()
  (let ((cerbere-php-phpunit-verbose-mode t))
    (should (string= (phpunit-command " --verbose")
		     (cerbere--php-phpunit-get-program ""
		      (cerbere--php-phpunit-arguments ""))))))

(ert-deftest test-php-phpunit-test-at-point ()
  (cerbere-with-test-content "php-phpunit/tests/SimpleTest.php"
    (should-not (cerbere--php-phpunit-test-at-point))
    (forward-line 8)
    (let ((test (cerbere--php-phpunit-test-at-point)))
      (should (equal "testDivide" (cerbere--php-phpunit-test-name test)))
      (should (equal "SimpleTest" (cerbere--php-phpunit-test-class test)))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test)))))
  (cerbere-with-test-content "php-phpunit/NoTest.php"
        (forward-line 8)
    (should-not (cerbere--php-phpunit-test-at-point))))

(ert-deftest test-php-phpunit-test-for-file ()
  (cerbere-with-test-content "php-phpunit/tests/SimpleTest.php"
    (let ((test (cerbere--php-phpunit-test-for-file)))
      (should-not (cerbere--php-phpunit-test-name test))
      (should (equal "SimpleTest" (cerbere--php-phpunit-test-class test)))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test))))
    (forward-line 8)
    (let ((test (cerbere--php-phpunit-test-for-file)))
      (should-not (cerbere--php-phpunit-test-name test))
      (should (equal "SimpleTest" (cerbere--php-phpunit-test-class test)))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test)))))
  (cerbere-with-test-content "php-phpunit/NoTest.php"
    (forward-line 8)
    (should-not (cerbere--php-phpunit-test-for-file))))

(ert-deftest test-php-phpunit-test-for-project ()
  (cerbere-with-test-content "php-phpunit/tests/SimpleTest.php"
    (let ((test (cerbere--php-phpunit-test-for-project)))
      (should-not (cerbere--php-phpunit-test-name test))
      (should-not (cerbere--php-phpunit-test-class test))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test))))
    (forward-line 8)
    (let ((test (cerbere--php-phpunit-test-for-project)))
      (should-not (cerbere--php-phpunit-test-name test))
      (should-not (cerbere--php-phpunit-test-class test))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test)))))
  (cerbere-with-test-content "php-phpunit/NoTest.php"
    (forward-line 8)
    (let ((test (cerbere--php-phpunit-test-for-project)))
      (should-not (cerbere--php-phpunit-test-name test))
      (should-not (cerbere--php-phpunit-test-class test))
      (should (equal (concat cerbere-test-data-path "php-phpunit/") (cerbere--php-phpunit-test-root test))))))

(provide 'cerbere-php-phpunit-test)
;;; cerbere-php-phpunit-test.el ends here
