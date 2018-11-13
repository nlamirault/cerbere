;;; cerbere-rust-libtest-test.el --- Unit tests for Cerbere ruby minitest backend

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

(require 'cerbere-rust-libtest)
(require 'rust-mode)

;; cerbere-rust-libtest mode

(defmacro cerbere-rust-libtest-with-test-buffer (&rest body)
  "Setup a buffer with our test data and run BODY in it."
    (declare (indent 0))
    `(cerbere-with-test-content "rust-libtest/src/foo/bar.rs"
       ,@body))

(ert-deftest test-cerbere-rust-libtest ()
  (cerbere-rust-libtest-with-test-buffer
    (rust-mode)
    (should (featurep 'cerbere-rust-libtest))))

(ert-deftest test-cerbere-rust-libtest-command ()
  (should (equal "cargo test -- module\\:\\:tests\\:\\:name"
                 (cerbere-rust-libtest-command '(:module "module::tests" :name "name" :project "/") t)))
  (should (equal "cargo test -- -q module\\:\\:tests\\:\\:name"
                 (cerbere-rust-libtest-command '(:module "module::tests" :name "name" :project "/"))))
  (should (equal "cargo test -- -q module\\:\\:tests\\:\\:"
                 (cerbere-rust-libtest-command '(:module "module::tests" :project "/")))))

(ert-deftest test-cerbere-rust-libtest-find-module ()
  (cerbere-rust-libtest-with-test-buffer
    (should (equal "foo::bar::tests" (cerbere-rust-libtest-find-module)))))

(ert-deftest test-cerbere-rust-libtest-test-at-point ()
  (cerbere-rust-libtest-with-test-buffer
    (search-forward "foo_bar_mod")
    (should (equal (list :backend 'rust-libtest
                         :project (concat cerbere-test-data-path "rust-libtest/")
                         :module "foo::bar::tests" :name "foo_bar_mod")
                   (cerbere-rust-libtest-test-at-point)))
    (search-forward "not_a_test")
    (should-not (cerbere-rust-libtest-test-at-point))))

(ert-deftest test-cerbere-rust-libtest-test-for-file ()
  (cerbere-rust-libtest-with-test-buffer
    (should (equal (list :backend 'rust-libtest
                         :project (concat cerbere-test-data-path "rust-libtest/")
                         :module "foo::bar::tests")
                   (cerbere-rust-libtest-test-for-file)))))

(ert-deftest test-cerbere-rust-libtest-test-for-project ()
  (cerbere-rust-libtest-with-test-buffer
    (should (equal (list :backend 'rust-libtest
                         :project (concat cerbere-test-data-path "rust-libtest/"))
                   (cerbere-rust-libtest-test-for-project))))
  (cerbere-with-test-content "elisp-ert-runner/Cask"
    (forward-line 1)
    (should-not (cerbere-rust-libtest-test-for-project))))

(provide 'cerbere-rust-libtest-test)
;;; cerbere-rust-libtest-test.el ends here
