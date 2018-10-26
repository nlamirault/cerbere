;;; cerbere-test.el --- Tests for Cerbere

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

(ert-deftest test-cerbere-go-keybindings ()
  (with-temp-buffer
    (cerbere-mode)
    (should (eql 'cerbere-version
                 (key-binding (kbd "C-c c v"))))
    (should (eql 'cerbere-current-test
                 (key-binding (kbd "C-c c t"))))
    (should (eql 'cerbere-current-file
                 (key-binding (kbd "C-c c f"))))
    (should (eql 'cerbere-current-project
                 (key-binding (kbd "C-c c p"))))))

(defvar cerbere-fake-backend-ran-test '() "The last test run by the fake backend.")

(defmacro cerbere-with-fake-test-buffer (&rest body)
  "Setup a fake buffer and return `TEST' when asked when executing BODY."
  (declare (indent 0))
  `(save-excursion
     (with-temp-buffer
       (let ((buffer-file-name "test.fake")
             (old-backends cerbere-backends))
         (cerbere-define-backend fake "fake"
           "Test backend."
           :run-test (lambda (test) (setq cerbere-fake-backend-ran-test test))
           :test-at-point (lambda () '(:backend fake :context test-at-point))
           :test-for-file (lambda () '(:backend fake :context test-for-file))
           :test-for-project (lambda () '(:backend fake :context test-for-project)))
         ,@body
         (setq cerbere-backends old-backends)))))

(ert-deftest test-cerbere-should-define-backend ()
  (cerbere-with-fake-test-buffer
    (should (equal 'fake (plist-get cedere--backend-fake :name)))
    (should (equal "fake" (plist-get cedere--backend-fake :f-ext)))))

(ert-deftest test-cerbere-find-backend-by-ext ()
  (cerbere-with-fake-test-buffer
    (should (equal 'fake (cerbere-backend-name (cerbere-find-backend-by-ext "fake"))))))

(ert-deftest test-cerbere-backend-call-should-call-backend-fun ()
  (cerbere-with-fake-test-buffer
    (cerbere-backend-call :run-test 'foobar)
    (should (equal 'foobar cerbere-fake-backend-ran-test))))

(ert-deftest test-cerbere-run-test-should-call-run-and-save-test ()
  (cerbere-with-fake-test-buffer
    (cerbere-run-test '(:backend fake :context foobar))
    (should (equal 'foobar (plist-get cerbere-fake-backend-ran-test :context)))
    (should (equal cerbere-last-test cerbere-fake-backend-ran-test))))

(ert-deftest test-cerbere-fetch-test-should-fetch-test ()
  (cerbere-with-fake-test-buffer
    (should (equal 'test-at-point (plist-get (cerbere-fetch-test :test-at-point) :context)))
    (should (equal 'test-for-file (plist-get (cerbere-fetch-test :test-for-file) :context)))
    (should (equal 'test-for-project (plist-get (cerbere-fetch-test :test-for-project) :context)))))

(ert-deftest test-cerbere-fetch-test-no-file ()
  (with-temp-buffer
    (should-not (cerbere-fetch-test :test-at-point))
    (should-not (cerbere-fetch-test :test-for-file))
    (should-not (cerbere-fetch-test :test-for-project))))

(ert-deftest test-cerbere-current-test ()
  (cerbere-with-fake-test-buffer
    (cerbere-current-test)
    (should (equal 'test-at-point (plist-get cerbere-fake-backend-ran-test :context)))))

(ert-deftest test-cerbere-current-file ()
  (cerbere-with-fake-test-buffer
   (cerbere-current-file)
    (should (equal 'test-for-file (plist-get cerbere-fake-backend-ran-test :context)))))

(ert-deftest test-cerbere-current-project ()
  (cerbere-with-fake-test-buffer
    (cerbere-current-project)
    (should (equal 'test-for-project (plist-get cerbere-fake-backend-ran-test :context)))))

;;; cerbere-test.el ends here
