;;; cerbere-rust-libtest.el --- Rust libtest backend for Cerbere

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

;; Rust libtest backend for Cebere that ease running rust tests with cargo
;; form inside Emacs.

;;; Code:

(require 'cerbere-common)

(defgroup cerbere-rust-libtest nil
  "Ruby Minitest utility"
  :group 'cerbere)

(defcustom cerbere-rust-libtest-cargo-command "cargo"
  "Cargo command to run tests."
  :type 'string
  :group 'cerbere-rust-libtest)

(defcustom cerbere-rust-libtest-cargo-module "tests"
  "Test module name, this will be appended to the current file's module."
  :type 'string
  :group 'cerbere-rust-libtest)

(defconst cerbere-rust-libtest-test-regexp
  "#\\[\\(.*test.*\\)\\][\n ]+fn\\s-+\\([a-zA-Z0-9_]+\\)"
  "Regular expression for a rust test definition.")

(defun cerbere-rust-libtest-test-root (test)
  "Return the root directory for TEST."
  (plist-get test :project))

(defun cerbere-rust-libtest-test-name (test)
  "Return the name for TEST."
  (plist-get test :name))

(defun cerbere-rust-libtest-test-module (test)
  "Return the module for TEST."
  (plist-get test :module))

(defun cerbere-rust-libtest-command (test &optional verbose)
  "Return command for running TEST using cargo, possibly being more VERBOSE."
  (let ((root (cerbere-rust-libtest-test-root test))
        (module (cerbere-rust-libtest-test-module test))
        (name (cerbere-rust-libtest-test-name test)))
    (cerbere-command
     cerbere-rust-libtest-cargo-command
     'test
     '--
     (and (not verbose) "-q")
     (and module (format "%s::%s" module (or name ""))))))

(defun cerbere-rust-libtest-run-test (test &optional verbose)
  "Run TEST, possibly being more VERBOSE."
  (cerbere-build (cerbere-rust-libtest-test-root test)
                 (cerbere-rust-libtest-command test verbose)))

(defun cerbere-rust-libtest-find-project ()
  "Return the project directory containing a Rakefile."
  (cerbere-project-root "Cargo.toml"))

(defun cerbere-rust-libtest-find-module ()
  "Return the module for the current project."
  (let* ((path (file-relative-name buffer-file-name
                                   (concat (cerbere-rust-libtest-find-project) "src")))
         (basename (file-name-nondirectory path))
         (path (file-name-sans-extension (if (equal basename "mod.rs")
                                             (directory-file-name (file-name-directory path))
                                           path)))
         (module (string-join (split-string path "/") "::")))
    (concat module "::" cerbere-rust-libtest-cargo-module)))

(defun cerbere-rust-libtest-test-at-point ()
  "Return the test at point.

Return a proprety list containing the test name and the test file for
the current buffer point, nil if there are no test."
  (save-excursion
    (end-of-line)
    (when (re-search-backward cerbere-rust-libtest-test-regexp nil 't)
      (let ((annotations (split-string (match-string 1) "[[:blank:]]*,[[:blank:]]*"))
            (test-name (match-string 2)))
        (when (seq-contains annotations "test")
          (set-text-properties 0 (length test-name) nil test-name)
          (list :backend 'rust-libtest
                :project (cerbere-rust-libtest-find-project)
                :module (cerbere-rust-libtest-find-module)
                :name test-name))))))

(defun cerbere-rust-libtest-test-for-file ()
  "Return the test for the current file."
  (list :backend 'rust-libtest
        :project (cerbere-rust-libtest-find-project)
        :module (cerbere-rust-libtest-find-module)))

(defun cerbere-rust-libtest-test-for-project ()
  "Return test definition for current project."
  ;; strip the -test.el from the file name and use that as a pattern
  (let ((project (cerbere-rust-libtest-find-project)))
    (when project
      (list :backend 'rust-libtest
            :project project))))

(cerbere-define-backend rust-libtest "rs"
  "Cerbere backend that runs Rust libtestt tests."
  :run-test #'cerbere-rust-libtest-run-test
  :test-at-point #'cerbere-rust-libtest-test-at-point
  :test-for-file #'cerbere-rust-libtest-test-for-file
  :test-for-project #'cerbere-rust-libtest-test-for-project)

(provide 'cerbere-rust-libtest)
;;; cerbere-rust-libtest.el ends here
