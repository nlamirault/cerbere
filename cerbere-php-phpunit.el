;;; cerbere-php-phpunit.el --- Launch PHP unit tests using phpunit

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

(require 'cerbere-common)

(defgroup cerbere-php-phpunit nil
  "PHPUnit utility"
  :group 'cerbere)

(defcustom cerbere--php-phpunit-program "phpunit"
  "PHPUnit binary path."
  :type 'file
  :group 'cerbere-php-phpunit)

(defcustom phpunit-arg ""
  "Argument to pass to phpunit."
  :type 'string
  :group 'cerbere-php-phpunit)

(defcustom cerbere-php-phpunit-stop-on-error nil
  "Stop execution upon first error."
  :type 'boolean
  :group 'cerbere-php-phpunit)

(defcustom cerbere-php-phpunit-stop-on-failure nil
  "Stop execution upon first error or failure."
  :type 'boolean
  :group 'cerbere-php-phpunit)

(defcustom cerbere-php-phpunit-stop-on-skipped nil
  "Stop execution upon first skipped test."
  :type 'boolean
  :group 'cerbere-php-phpunit)

(defcustom cerbere-php-phpunit-verbose-mode nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'cerbere-php-phpunit)


(defconst cerbere--php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(test\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")

(defun cerbere--php-phpunit-test-root (test)
  "Return the root directory for TEST."
  (plist-get test :project))

(defun cerbere--php-phpunit-test-class (test)
  "Return the class name for TEST."
  (plist-get test :class))

(defun cerbere--php-phpunit-test-name (test)
  "Return the test name for TEST."
  (plist-get test :name))

(defun cerbere--php-phpunit-get-root-directory ()
  "Return the project root directory."
  (let ((filename (buffer-file-name)))
    (when filename
      (file-truename (or (locate-dominating-file filename "phpunit.xml")
			 (file-name-directory filename))))))

(defun cerbere--php-phpunit-get-program (root args)
  "Return the command to launch unit test in ROOT using ARGS.
`ARGS' corresponds to phpunit command line arguments."
  (concat cerbere--php-phpunit-program " -c " root "phpunit.xml" args))

(defun cerbere--php-phpunit-get-current-class (&optional file)
  "Return the class name of the PHPUnit test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    (file-name-nondirectory (directory-file-name (replace-regexp-in-string "\\.php\\'" "" file)))))


(defun cerbere--php-phpunit-get-current-test ()
  (save-excursion
    (when (re-search-backward cerbere--php-beginning-of-defun-regexp nil t)
      (match-string-no-properties 1))))

(defun cerbere--php-phpunit-arguments (args &optional verbose)
  "Append options to ARGS given package configuration, possibly adding VERBOSE."
  (let ((opts args))
     (when cerbere-php-phpunit-stop-on-error
       (setq opts (concat opts " --stop-on-error")))
     (when cerbere-php-phpunit-stop-on-failure
       (setq opts (concat opts " --stop-on-failure")))
     (when cerbere-php-phpunit-stop-on-skipped
       (setq opts (concat opts " --stop-on-skipped")))
     (when (or cerbere-php-phpunit-verbose-mode verbose)
       (setq opts (concat opts " --verbose")))
     opts))

(defun cerbere--php-phpunit-test-args (test)
  "Return arguments for running `TEST'."
  (let ((test-class (and test (plist-get test :class)))
        (test-name (and test (plist-get test :test))))
    (if (or test-class test-name)
        (format " --filter '%s%s'" test-class
                (if test-name (format "::%s" test-name) ""))
      "")))

(defun cerbere--php-phpunit-run (test &optional verbose)
  "Run `TEST', possibly being more VERBOSE."
  (cerbere--build
   (cerbere--php-phpunit-get-program
    (cerbere--php-phpunit-test-root test)
    (cerbere--php-phpunit-arguments (cerbere--php-phpunit-test-args test) verbose))))

(defun cerbere--php-phpunit-test-at-point ()
  "Return the test at point.

Return a proprety list containing the test name and the test class for
the current buffer point, nil if there are no test."
  (let ((name (cerbere--php-phpunit-get-current-test)))
    (when name
      (list :backend 'php-phpunit
            :project (cerbere--php-phpunit-get-root-directory)
            :class (cerbere--php-phpunit-get-current-class)
            :name (cerbere--php-phpunit-get-current-test)))))

(defun cerbere--php-phpunit-test-for-file ()
  "Return the test for the current buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((name (cerbere--php-phpunit-get-current-test)))
      (when name
        (list :backend 'php-phpunit
              :project (cerbere--php-phpunit-get-root-directory)
              :class (cerbere--php-phpunit-get-current-class))))))

(defun cerbere--php-phpunit-test-for-project ()
  "Return the test for the current project."
  (let ((root (cerbere--php-phpunit-get-root-directory)))
    (when root
      (list :backend 'php-phpunit
            :project root))))

(defun cerbere--php-phpunit-run-test (test &optional verbose)
  "Launch PHPUnit on `TEST', possibly being more VERBOSE."
  (cerbere--php-phpunit-run test verbose))

(cerbere-define-backend php-phpunit "php"
  "Cerbere backend that runs phpunit tests."
  :run-test #'cerbere--php-phpunit-run-test
  :test-at-point #'cerbere--php-phpunit-test-at-point
  :test-for-file #'cerbere--php-phpunit-test-for-file
  :test-for-project #'cerbere--php-phpunit-test-for-project)

(provide 'cerbere-php-phpunit)
;;; cerbere-php-phpunit.el ends here
