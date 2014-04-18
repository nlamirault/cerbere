;;; cerbere-gotest.el --- Launch GO unit tests

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

(require 's)
(require 'f)
(require 'go-mode)


(require 'cerbere-common)


(defgroup cerbere-go nil
  "Golang back-end for Cerbere."
  :group 'cerbere)

;; (defcustom go-test-args ""
;;   "Argument to pass to go."
;;   :type 'string
;;   :group 'cerbere-go)

(defcustom cerbere-go-test-verbose nil
  "Display debugging information during test execution."
  :type 'boolean
  :group 'cerbere-go)


;; Commands
;; -----------

(defun cerbere--go-test-get-program (args)
  "Return the command to launch unit test.
`ARGS' corresponds to go command line arguments."
  (s-concat go-command " test "
            ;;(go-test-get-root-directory)
            args))

;; (defun cerbere--go-test-get-root-directory()
;;   "Return the root directory to run tests."
;;   (let ((filename (buffer-file-name)))
;;     (when filename
;;       (file-truename (or (locate-dominating-file filename "Makefile")
;;                          "./")))))

(defun cerbere--go-test-get-current-file (&optional file)
  "Return the filename of the go test for `FILE'."
  (let* ((file (or file (buffer-file-name))))
    (f-long (f-filename file))))


(defun cerbere--go-test-get-current-test ()
  (let ((start (point))
        test-name)
    (save-excursion
      (end-of-line)
      (unless (and
               (search-backward-regexp "^[[:space:]]*func[[:space:]]*Test" nil t)
               (save-excursion (go-end-of-defun) (< start (point))))
        (error "Unable to find a test"))
      (save-excursion
        (search-forward "Test")
        (setq test-name (thing-at-point 'word))))
    test-name))


(defun cerbere--go-test-arguments (args)
  (let ((opts args))
    (when cerbere-go-test-verbose
      (setq opts (s-concat opts " -v")))
    opts))


(defun cerbere--go-test-run (args)
  (cerbere--build (cerbere--go-test-get-program
		   (cerbere--go-test-arguments args))))


; API
;; ----


(defun cerbere-go-test-current-test ()
  "Launch go test on curent test."
  (interactive)
  (let ((test-name (cerbere--go-test-get-current-test)))
    (when test-name
      (let ((args (s-concat " -run " test-name)))
      (cerbere--go-test-run args)))))


(defun cerbere-go-test-current-file ()
  "Launch go test on file."
  (interactive)
  (let ((args (s-concat " -file=" (cerbere--go-test-get-current-file))))
    (cerbere--go-test-run args)))


(defun cerbere-go-test-current-project ()
  "Launch go test on project."
  (interactive)
  (cerbere--go-test-run ""))


;;; ###autoload
(defun cerbere-go-test (command)
  "Go lang backend."
  (pcase command
    (`test (cerbere-go-test-current-test))
    (`file (cerbere-go-test-current-file))
    (`project (cerbere-go-test-current-project))))

;;(cerbere-add-backend "go" 'cerbere-go-test)

(provide 'cerbere-gotest)
;;; cerbere-gotest.el ends here
