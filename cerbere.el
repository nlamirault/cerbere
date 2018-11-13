;;; cerbere.el --- Unit testing in Emacs for several programming languages

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/cerbere
;; Version: 0.1.0
;; Keywords: python, go, php, phpunit, elisp, ert, tests, tdd

;; Package-Requires: ((pkg-info "0.5"))

;; Copyright (C) 2014  Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;; Global dependencies
(require 'pkg-info)

;; Project dependencies
(require 'cerbere-common)

(with-eval-after-load 'php-mode (require 'cerbere-php-phpunit))
(with-eval-after-load 'go-mode (require 'cerbere-go-test))
(with-eval-after-load 'python (require 'cerbere-python-tox))
(with-eval-after-load 'elisp-mode (require 'cerbere-elisp-ert-runner))
(with-eval-after-load 'ruby-mode (require 'cerbere-ruby-minitest))
(with-eval-after-load 'rust-mode (require 'cerbere-rust-libtest))

;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup cerbere nil
  "Unit testing in Emacs for several programming languages."
  :group 'tools)

(defcustom cerbere-keymap-prefix (kbd "C-c c")
  "Cerbere keymap prefix."
  :group 'cerbere
  :type 'cerbere)

(defvar cerbere-package-version "0.1.0"
  "Release version of Cerbere.")


;;; backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cerbere-last-test '() "The last executed test.")

(defun cerbere-backend-f-ext (backend)
  "Return BACKEND supported file extension."
  (plist-get backend :f-ext))

(defun cerbere-find-backend-by-ext (f-ext)
  "Search a candidate into all available backends using F-EXT."
  (seq-some (lambda (backend)
              (when (equal f-ext (cerbere-backend-f-ext backend))
                backend))
            cerbere-backends))

(defun cerbere-find-backend-by-name (name)
  "Search backends for NAME."
  (seq-some (lambda (backend)
              (when (equal name (cerbere-backend-name backend))
                backend))
            cerbere-backends))

(defun cerbere-backend-fun (backend context)
  "Return the BACKEND function associated to CONTEXT."
  (plist-get backend context))

(defun cerbere-extract-file-ext ()
  "Extract file extension from current buffer."
  (when buffer-file-name (file-name-extension buffer-file-name)))

(defun cerbere-backend-call (context &rest arguments)
  "Fetch backend for the current file and call CONTEXT with ARGUMENTS on it."
  (let* ((backend (cerbere-find-backend-by-ext (cerbere-extract-file-ext)))
         (fun (cerbere-backend-fun backend context)))
    (when backend
      (apply fun arguments))))

;; Here we cannot use backend-call. We my want to run test even though we are
;; not in a test buffer so we cannot use the current extension to find the
;; backend. We have a test and they have the backend definition in them, use
;; that.
(defun cerbere-run-test (test &optional verbose)
  "Run TEST, possibly being more VERBOSE, and remember it."
  (setq cerbere-last-test test)
  (let ((backend (cerbere-find-backend-by-name (plist-get test :backend))))
    (unless backend
      (error "Unable to find backend %s" (plist-get test :backend)))
    (funcall (cerbere-backend-fun backend :run-test) test verbose)))

(defun cerbere-fetch-test (context)
  "Fetch test for current CONTEXT."
  (cerbere-backend-call context))

(defun cerbere-fetch-and-run-test (context &optional verbose)
  "Run test for current CONTEXT, maybe being VERBOSE."
  (cerbere-run-test (cerbere-fetch-test context) verbose))

(defun cerbere-fetch-or-last-run-test (context &optional verbose)
  "Run test for current CONTEXT or the last test if they are none.

The test will be run verbosely if VERBOSE is not nil."
  (let ((test (or (cerbere-fetch-test context) cerbere-last-test)))
    (if test (cerbere-run-test test verbose)
      (message "Cerbere did not find any test to run"))))

;;;###autoload
(defun cerbere-current-test (&optional verbose)
  "Launch backend on current test, maybe being VERBOSE."
  (interactive "P")
  (cerbere-fetch-and-run-test :test-at-point verbose))

;;;###autoload
(defun cerbere-current-file (&optional verbose)
  "Launch backend on current file, maybe being VERBOSE.."
  (interactive "P")
  (cerbere-fetch-and-run-test :test-for-file verbose))

;;;###autoload
(defun cerbere-current-project (&optional verbose)
  "Launch backend on current project, maybe being VERBOSE.."
  (interactive "P")
  (cerbere-fetch-and-run-test :test-for-project verbose))

;;;###autoload
(defun cerbere-last-test (&optional verbose)
  "Launch backend on the last test, maybe being VERBOSE.."
  (interactive "P")
  (if cerbere-last-test (cerbere-run-test cerbere-last-test verbose)
    (message "Cerbere did not find any last test to run")))

;;;###autoload
(defun cerbere-test-dwim (&optional verbose)
  "Try to launch the test at point or the last executed test.

This will check if the cursor is currently in a test function
definition.  If so it will run that test.  If there are no test
definition, it will run the last executed test.

The test will be run verbosely if VERBOSE is not nil."
  (interactive "P")
  (cerbere-fetch-or-last-run-test :test-at-point verbose))

;;;###autoload
(defun cerbere-version ()
  "Dislay the Cerbere's version."
  (interactive)
  ;;(message "Cerbere version: %s" cerbere-package-version)
  (let ((version (pkg-info-version-info 'cerbere)))
    (message "Cerbere %s" version)))

;;; Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cerbere-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "v") 'cerbere-version)
      (define-key prefix-map (kbd "t") 'cerbere-current-test)
      (define-key prefix-map (kbd "f") 'cerbere-current-file)
      (define-key prefix-map (kbd "p") 'cerbere-current-project)
      (define-key prefix-map (kbd "l") 'cerbere-last-test)
      (define-key prefix-map (kbd "d") 'cerbere-dwim)
      (define-key map cerbere-keymap-prefix prefix-map))
    map)
  "Keymap used by `cerbere-mode'..")

;;;###autoload
(defconst cerbere-mode-line-lighter " Cerbere"
  "The default lighter for `cerbere-mode'.")

(define-minor-mode cerbere-mode
  "Minor mode for Cerbere..

\\{cerbere-mode-map}"
  :lighter cerbere-mode-line-lighter
  :keymap cerbere-mode-map
  :group 'cerbere
  :require 'cerbere)

;;;###autoload
(define-globalized-minor-mode cerbere-global-mode cerbere-mode cerbere-on)

(defun cerbere-on ()
  "Turn on `cerbere-mode'."
  (interactive)
  (cerbere-mode +1))

(defun cerbere-off ()
  "Turn off `cerbere-mode'."
  (interactive)
  (cerbere-mode -1))

(provide 'cerbere)
;;; cerbere.el ends here
