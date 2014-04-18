;;; cerbere-phpunit-test.el --- Unit tests for phpunit backend of Cerbere

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


(require 'test-helper)

(require 'cerbere)
(require 'cerbere-phpunit)


(defun phpunit-command (&rest arg)
  (apply 's-concat "phpunit -c " "phpunit.xml" arg))


;; cerbere-phpunit Mode

(ert-deftest test-cerbere-phpunit ()
  (with-temp-buffer
    (should (featurep 'cerbere-phpunit))))


;;

(ert-deftest test-cerbere-phpunit-get-current-class ()
  (should (string= "PhpUnitTest"
		   (cerbere--phpunit-get-current-class "/tmp/foo/PhpUnitTest.php"))))

;; Arguments

(ert-deftest test-cerbere-phpunit-get-program-without-args ()
  (should (string= (phpunit-command)
		   (cerbere--phpunit-get-program
		    (cerbere--phpunit-arguments "")))))

(ert-deftest test-phpunit-add-stop-on-error-argument ()
  (let ((cerbere-phpunit-stop-on-error t))
    (should (string= (phpunit-command " --stop-on-error")
		     (cerbere--phpunit-get-program
		      (cerbere--phpunit-arguments ""))))))

(ert-deftest test-phpunit-add-stop-on-failure-argument ()
  (let ((cerbere-phpunit-stop-on-failure t))
    (should (string= (phpunit-command " --stop-on-failure")
		     (cerbere--phpunit-get-program
		      (cerbere--phpunit-arguments ""))))))

(ert-deftest test-phpunit-add-stop-on-skipped-argument ()
  (let ((cerbere-phpunit-stop-on-skipped t))
    (should (string= (phpunit-command " --stop-on-skipped")
		     (cerbere--phpunit-get-program
		      (cerbere--phpunit-arguments ""))))))

(ert-deftest test-phpunit-add-verbose-argument ()
  (let ((cerbere-phpunit-verbose-mode t))
    (should (string= (phpunit-command " --verbose")
		     (cerbere--phpunit-get-program
		      (cerbere--phpunit-arguments ""))))))


(provide 'phpunit-test)
;;; cerbere-phpunit-test.el ends here
