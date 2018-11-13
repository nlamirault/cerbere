;;; cerbere-ruby-minitest-test.el --- Unit tests for Cerbere ruby minitest backend

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

;;; Code:

(require 'cerbere-ruby-minitest)

;; cerbere-ruby-minitest mode

(defmacro cerbere-ruby-minitest-with-test-buffer (&rest body)
  "Setup a buffer with our test data and run BODY in it."
    (declare (indent 0))
    `(cerbere-with-test-content "ruby-minitest/test/example_test.rb"
       ,@body))

(ert-deftest test-cerbere-ruby-minitest ()
  (cerbere-ruby-minitest-with-test-buffer
    (ruby-mode)
    (should (featurep 'cerbere-ruby-minitest))))

(ert-deftest test-cerbere-ruby-minitest-options ()
  (should (equal '("--name=test" --verbose)
                 (cerbere-ruby-minitest-options "test" t)))
  (should-not (cerbere-ruby-minitest-options)))


(ert-deftest test-cerbere-ruby-minitest-command ()
  (should (equal "rake test TEST\\=file TESTOPTS\\=--name\\\\\\=name\\ --verbose"
                 (cerbere-ruby-minitest-command '(:file "file" :name "name" :project "/") t)))
  (should (equal "rake test TEST\\=file TESTOPTS\\=--name\\\\\\=name"
                 (cerbere-ruby-minitest-command '(:file "file" :name "name" :project "/"))))
  (should (equal "rake test TEST\\=file TESTOPTS\\="
                 (cerbere-ruby-minitest-command '(:file "file" :project "/")))))

(ert-deftest test-cerbere-ruby-minitest-find-file ()
  (cerbere-ruby-minitest-with-test-buffer
    (should (equal "test/example_test.rb" (cerbere-ruby-minitest-find-file)))))

(ert-deftest test-cerbere-ruby-minitest-test-at-point ()
  (cerbere-ruby-minitest-with-test-buffer
    (search-forward "def test_first_test")
    (should (equal (list :backend 'ruby-minitest
                         :project (concat cerbere-test-data-path "ruby-minitest/")
                         :file "test/example_test.rb" :name "test_first_test")
                   (cerbere-ruby-minitest-test-at-point)))
    (search-forward "test \"first spec\"")
    (should (equal (list :backend 'ruby-minitest
                         :project (concat cerbere-test-data-path "ruby-minitest/")
                         :file "test/example_test.rb" :name "test_first_spec")
                   (cerbere-ruby-minitest-test-at-point))))
  (cerbere-with-test-content "ruby-minitest/example.rb"
    (forward-line)
    (should-not (cerbere-ruby-minitest-test-at-point))))

(ert-deftest test-cerbere-ruby-minitest-test-for-file ()
  (cerbere-ruby-minitest-with-test-buffer
    (should (equal (list :backend 'ruby-minitest
                         :project (concat cerbere-test-data-path "ruby-minitest/")
                         :file "test/example_test.rb")
                   (cerbere-ruby-minitest-test-for-file))))
  (cerbere-with-test-content "ruby-minitest/example.rb"
    (forward-line 1)
    (should-not (cerbere-ruby-minitest-test-for-file))))

(ert-deftest test-cerbere-ruby-minitest-test-for-project ()
  (cerbere-ruby-minitest-with-test-buffer
    (should (equal (list :backend 'ruby-minitest
                         :project (concat cerbere-test-data-path "ruby-minitest/"))
                   (cerbere-ruby-minitest-test-for-project))))
  (cerbere-with-test-content "elisp-ert-runner/Cask"
    (forward-line 1)
    (should-not (cerbere-ruby-minitest-test-for-project))))

(provide 'cerbere-ruby-minitest-test)
;;; cerbere-ruby-minitest-test.el ends here
