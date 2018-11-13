;;; cerbere-ruby-minitest.el --- Ruby minitest backend for Cerbere

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

;; Ruby minitest backend for Cebere that ease running minitest tests with rake
;; form inside Emacs.

;;; Code:

(require 'cerbere-common)

(defgroup cerbere-ruby-minitest nil
  "Ruby Minitest utility"
  :group 'cerbere)

(defcustom cerbere-ruby-minitest-bundle-command "bundle exec"
  "Bundle command to use as prefix for rake and ruby."
  :type 'string
  :group 'cerbere-ruby-minitest)

(defconst cerbere-ruby-minitest-beginning-of-test-regexp
  "\\(test\\s-+\"\\([^\"]+\\)\"\\|def\\s-+test_\\([0-9a-zA-Z_!]+\\)\\)"
  "Regular expression for a ruby minitest definition.")

(defun cerbere-ruby-minitest-executable (exec path)
  "Return bundle EXEC PATH if a Gemfile is found, EXEC PATH otherwise."
  (if (locate-dominating-file path "Gemfile")
      (list (split-string cerbere-ruby-minitest-bundle-command) exec)
    exec))

(defun cerbere-ruby-minitest-test-root (test)
  "Return the Rakefile directory for TEST."
  (plist-get test :project))

(defun cerbere-ruby-minitest-test-name (test)
  "Return the name for TEST."
  (plist-get test :name))

(defun cerbere-ruby-minitest-test-file (test)
  "Return the file for TEST."
  (plist-get test :file))

(defun cerbere-ruby-minitest-options (&optional name verbose)
  "Return minitest options for running test NAME, possibly being more VERBOSE."
  (remove nil (list (and name (format "--name=%s" name))
                    (and verbose '--verbose))))

(defun cerbere-ruby-minitest-command (test &optional verbose)
  "Return command for running TEST using rake, possibly being more VERBOSE."
  (let ((root (cerbere-ruby-minitest-test-root test))
        (file (cerbere-ruby-minitest-test-file test))
        (name (cerbere-ruby-minitest-test-name test)))
    (cerbere-command
      (cerbere-ruby-minitest-executable "rake" root)
      'test
      (and file (format "TEST=%s" file))
      (format "TESTOPTS=%s" (apply #'cerbere-command (cerbere-ruby-minitest-options name verbose))))))

(defun cerbere-ruby-minitest-run-test (test &optional verbose)
  "Run TEST, possibly being more VERBOSE."
  (cerbere-build (cerbere-ruby-minitest-test-root test)
                 (cerbere-ruby-minitest-command test verbose)))

(defun cerbere-ruby-minitest-find-project ()
  "Return the project directory containing a Rakefile."
  (cerbere-project-root "Rakefile"))

(defun cerbere-ruby-minitest-find-file ()
  "Return the file path relative to the project root."
  (file-relative-name buffer-file-name (cerbere-ruby-minitest-find-project)))

(defun cerbere-ruby-minitest-test-at-point ()
  "Return the test at point.

Return a proprety list containing the test name and the test file for
the current buffer point, nil if there are no test."
  (save-excursion
    (end-of-line)
    (when (re-search-backward cerbere-ruby-minitest-beginning-of-test-regexp nil 't)
      (let ((test-name (or (match-string 2) (match-string 3))))
        (set-text-properties 0 (length test-name) nil test-name)
        (list :backend 'ruby-minitest
              :project (cerbere-ruby-minitest-find-project)
              :file (cerbere-ruby-minitest-find-file)
              :name (concat "test_" (replace-regexp-in-string " " "_" test-name)))))))

(defun cerbere-ruby-minitest-test-for-file ()
  "Return the test for the current file."
  (save-excursion
    (goto-char (point-max))
    (let ((name (cerbere-ruby-minitest-test-at-point)))
      (when name
        (list :backend 'ruby-minitest
              :project (cerbere-ruby-minitest-find-project)
              :file (cerbere-ruby-minitest-find-file))))))

(defun cerbere-ruby-minitest-test-for-project ()
  "Return test definition for current project."
  ;; strip the -test.el from the file name and use that as a pattern
  (let ((project (cerbere-ruby-minitest-find-project)))
    (when project
      (list :backend 'ruby-minitest
            :project project))))

(cerbere-define-backend ruby-minitest "rb"
  "Cerbere backend that runs Ruby minitest tests."
  :run-test #'cerbere-ruby-minitest-run-test
  :test-at-point #'cerbere-ruby-minitest-test-at-point
  :test-for-file #'cerbere-ruby-minitest-test-for-file
  :test-for-project #'cerbere-ruby-minitest-test-for-project)

(provide 'cerbere-ruby-minitest)
;;; cerbere-ruby-minitest.el ends here
