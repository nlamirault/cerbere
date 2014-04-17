;;; cerbere-phpunit.el --- Launch PHP unit tests using phpunit

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


(require 's)
(require 'f)

(require 'cerbere-common)


(defgroup cerbere-phpunit nil
  "PHPUnit utility"
  :group 'cerbere)

(defcustom cerbere--phpunit-program "phpunit"
  "PHPUnit binary path."
  :type 'file
  :group 'cerbere-phpunit)

(defcustom phpunit-arg ""
  "Argument to pass to phpunit."
  :type 'string
  :group 'cerbere-phpunit)

(defcustom cerbere-phpunit-stop-on-error nil
  "Stop execution upon first error."
  :type 'boolean
  :group 'cerbere-phpunit)

(defcustom cerbere-phpunit-stop-on-failure nil
  "Stop execution upon first error or failure."
  :type 'boolean
  :group 'cerbere-phpunit)

(defcustom cerbere-phpunit-stop-on-skipped nil
  "Stop execution upon first skipped test."
  :type 'boolean
  :group 'cerbere-phpunit)

(defcustom cerbere-phpunit-verbose-mode nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'cerbere-phpunit)


(defconst cerbere--php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")


;; Commands
;; -----------

(defun cerbere--phpunit-get-root-directory()
  "Return the root directory to run tests."
  (let ((filename (buffer-file-name)))
    (when filename
      (file-truename (or (locate-dominating-file filename "phpunit.xml")
			 "./")))))

(defun cerbere--phpunit-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to phpunit command line arguments."
  (s-concat cerbere--phpunit-program " -c "
	    (cerbere--phpunit-get-root-directory)
	    "phpunit.xml"
	    args))

(defun cerbere--phpunit-get-current-class (&optional file)
  "Return the class name of the PHPUnit test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    (f-filename (replace-regexp-in-string "\\.php\\'" "" file))))


(defun cerbere--phpunit-get-current-test ()
  (save-excursion
    (when (re-search-backward cerbere--php-beginning-of-defun-regexp nil t)
      (match-string-no-properties 1))))

(defun cerbere--phpunit-arguments (args)
  (let ((opts args))
     (when cerbere-phpunit-stop-on-error
       (setq opts (s-concat opts " --stop-on-error")))
     (when cerbere-phpunit-stop-on-failure
       (setq opts (s-concat opts " --stop-on-failure")))
     (when cerbere-phpunit-stop-on-skipped
       (setq opts (s-concat opts " --stop-on-skipped")))
     (when cerbere-phpunit-verbose-mode
       (setq opts (s-concat opts " --verbose")))
     opts))


(defun cerbere--phpunit-run (args)
  (cerbere--build (cerbere--phpunit-get-program
		  (cerbere--phpunit-arguments args))))


;; API
;; ----


(defun cerbere--phpunit-current-test ()
  "Launch PHPUnit on curent test."
  (interactive)
  (let ((args (s-concat " --filter '"
			(cerbere--phpunit-get-current-class)
			"::"
			(cerbere--phpunit-get-current-test) "'")))
    (cerbere--phpunit-run args)))


(defun cerbere--phpunit-current-class ()
  "Launch PHPUnit on current class."
  (interactive)
  (let ((args (s-concat " --filter '" (cerbere--phpunit-get-current-class) "'")))
    (cerbere--phpunit-run args)))


(defun cerbere--phpunit-current-project ()
  "Launch PHPUnit on current project."
  (interactive)
  (cerbere--phpunit-run ""))


;;;###autoload
(defun cerbere-phpunit (command)
  "PHPUnit cerbere backend."
  (pcase command
    (`test (cerbere--phpunit-current-test))
    (`file (cerbere--phpunit-current-class))
    (`project (cerbere--phpunit-current-project))))



(provide 'cerbere-phpunit)
;;; cerbere-phpunit.el ends here
