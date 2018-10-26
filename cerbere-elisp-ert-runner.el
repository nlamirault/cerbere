;;; cerbere-elisp-ert-runner.el -- Ert runner backend for Cerbere

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

;; Ert runner backend for Cerbere that ease running ert tests with cask from
;; inside Emacs.

;;; Code:

(require 'cerbere-common)

(defgroup cerbere-elisp-ert-runner nil
  "Cerbere elisp Ert runner customizations"
  :group 'cerbere)

(defcustom cerbere-elisp-ert-runner-cask "cask"
  "Cask binary path."
  :type 'file
  :group 'cerbere-elisp-ert-runner)

(defun cerbere-elisp-ert-runner-find-project ()
  "Return the root of this Cask project."
  (cerbere-project-root "Cask"))

(defun cerbere-elisp-ert-runner-test-root (test)
  "Return the Cask directory for TEST."
  (plist-get test :project))

(defun cerbere-elisp-ert-runner-test-pattern (test)
  "Return the Cask directory for TEST."
  (plist-get test :pattern))

(defun cerbere-elisp-ert-runner-command (test &optional verbose)
  "Return commond to run TEST, possibly being more VERBOSE."
  (let ((pattern (cerbere-elisp-ert-runner-test-pattern test)))
    (cerbere-command
      cerbere-elisp-ert-runner-cask 'exec 'ert-runner
      (and verbose '--verbose)
      (and pattern `(--pattern ,pattern)))))

(defun cerbere-elisp-ert-runner-run-test (test &optional verbose)
  "Run TEST, possibly being more VERBOSE."
  (cerbere-build (cerbere-elisp-ert-runner-test-root test)
                 (cerbere-elisp-ert-runner-command test verbose)))

(defun cerbere-elisp-ert-runner-sexp-at-point ()
  "Return current test at point."
  (save-excursion
    (beginning-of-defun)
    (when (looking-at-p "(")
      (let ((start (point)))
        (forward-sexp)
        (car (read-from-string (buffer-substring start (point))))))))

(defun cerbere-elisp-ert-runner-test-at-point ()
  "Return test definition for test at point."
  (let ((sexp (cerbere-elisp-ert-runner-sexp-at-point)))
    (when (and sexp (eql 'ert-deftest (car sexp)))
      (list :backend 'elisp-ert-runner
            :project (cerbere-elisp-ert-runner-find-project)
            :pattern (format "^%s$" (cadr sexp))))))

(defun cerbere-elisp-ert-runner-test-for-file ()
  "Return test definition for current file."
  ;; strip the -test.el from the file name and use that as a pattern
  (when (string-match "\\([^/]+\\)-test.el" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name)))
      (list :backend 'elisp-ert-runner
            :project (cerbere-elisp-ert-runner-find-project)
            :pattern (format "^test-%s" name)))))

(defun cerbere-elisp-ert-runner-test-for-project ()
  "Return test definition for current project."
  ;; strip the -test.el from the file name and use that as a pattern
  (let ((project (cerbere-elisp-ert-runner-find-project)))
    (when project
      (list :backend 'elisp-ert-runner
            :project project))))

(cerbere-define-backend elisp-ert-runner "el"
  "Cerbere backend that runs Emacs lips ert runner tests."
  :run-test #'cerbere-elisp-ert-runner-run-test
  :test-at-point #'cerbere-elisp-ert-runner-test-at-point
  :test-for-file #'cerbere-elisp-ert-runner-test-for-file
  :test-for-project #'cerbere-elisp-ert-runner-test-for-project)

(provide 'cerbere-elisp-ert-runner)

;;; cerbere-elisp-ert-runner.el ends here
