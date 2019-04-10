;;; cerbere-elisp-ert-runner-test.el -- Ert runner Cerbere backend tests.

;; Copyright (C) 2018 Damien Merenne <dam@cosinux.org>

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

;; Tests for the elisp ert-runner Cerbere backend.

;;; Code:

(require 'cerbere-elisp-ert-runner)

(ert-deftest test-cerbere-elisp-ert-runner ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (should (featurep 'cerbere-elisp-ert-runner))))

(ert-deftest test-cerbere-elisp-ert-runner-find-project ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (should (equal (concat cerbere-test-data-path "elisp-ert-runner/")
                   (cerbere-elisp-ert-runner-find-project)))))

(ert-deftest test-cerbere-elisp-ert-runner-command ()
  (should (equal "cask exec ert-runner"
                 (cerbere-elisp-ert-runner-command '(:project "/"))))
  (should (equal "cask exec ert-runner --verbose"
                 (cerbere-elisp-ert-runner-command '(:project "/") t)))
  (should (equal "cask exec ert-runner --pattern \\^foobar\\$"
                 (cerbere-elisp-ert-runner-command '(:project "/" :pattern "^foobar$")))))

(ert-deftest test-cerbere-elisp-ert-runner-sexp-at-point ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (forward-line 9)
    (should (equal '(ert-deftest test-example ()
                      (should t))
                   (cerbere-elisp-ert-runner-sexp-at-point)))))

(ert-deftest test-cerbere-elisp-ert-runner-test-at-point ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (forward-line 9)
    (should (equal (list :backend 'elisp-ert-runner
                         :project (concat cerbere-test-data-path "elisp-ert-runner/")
                         :pattern "^test-example$")
                   (cerbere-elisp-ert-runner-test-at-point)))
    (forward-line 3)
    (should-not (cerbere-elisp-ert-runner-test-at-point))
    (goto-char (point-min))
    (should-not (cerbere-elisp-ert-runner-test-at-point))))

(ert-deftest test-cerbere-elisp-ert-runner-test-for-file ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (should (equal (list :backend 'elisp-ert-runner
                         :project (concat cerbere-test-data-path "elisp-ert-runner/")
                         :pattern "^test-example")
                   (cerbere-elisp-ert-runner-test-for-file))))
  (cerbere-with-test-content "elisp-ert-runner/example.el"
    (should-not (cerbere-elisp-ert-runner-test-for-file))))

(ert-deftest test-cerbere-elisp-ert-runner-test-for-project ()
  (cerbere-with-test-content "elisp-ert-runner/test/example-test.el"
    (should (equal (list :backend 'elisp-ert-runner
                         :project (concat cerbere-test-data-path "elisp-ert-runner/"))
                   (cerbere-elisp-ert-runner-test-for-project))))
  (cerbere-with-test-content "elisp-ert-runner/example.el"
    (should (equal (list :backend 'elisp-ert-runner
                         :project (concat cerbere-test-data-path "elisp-ert-runner/"))
                   (cerbere-elisp-ert-runner-test-for-project))))
  (cerbere-with-test-content "php-phpunit/tests/SimpleTest.php"
    (should-not (cerbere-elisp-ert-runner-test-for-file))))

;;; cerbere-elisp-ert-runner-test.el ends here
