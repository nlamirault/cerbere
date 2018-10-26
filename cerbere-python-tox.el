;;; cerbere-python-tox.el --- Launch python tests with tox

;; Copyright (C) 2013 Chmouel Boudjnah <chmouel@chmouel.com>
;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
(require 'python)

(defgroup cerbere-python-tox nil
  "Tox back-end for Cerbere."
  :group 'cerbere)

(defcustom cerbere--python-tox-program "tox"
  "Tox binary path."
  :type 'string
  :group 'cerbere-python-tox)

(defcustom cerbere--python-tox-arg ""
  "Argument to pass to tox."
  :type 'string
  :group 'cerbere-python-tox)

(defcustom cerbere--python-tox-default-env nil
  "Default argument for Tox."
  :type 'string
  :group 'cerbere-python-tox)

(defun cerbere--python-tox-read-python-tox-ini-envlist()
  "Read the tox.ini file and grab the environement list."
  (let ((tox-ini-file
         (concat (locate-dominating-file
                  (buffer-file-name) "tox.ini") "tox.ini"))
        (envlist))
    (with-temp-buffer
      (buffer-disable-undo)
      (cond ((get-file-buffer tox-ini-file)
             (insert (with-current-buffer (get-file-buffer tox-ini-file)
                       (buffer-substring (point-min) (point-max)))))
            ((not (file-exists-p tox-ini-file)))
            (t (insert-file-contents tox-ini-file)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n) (newline))
      (goto-char (point-min))
      (while (re-search-forward "^envlist\s*=\s*\\([^\t\n ]+\\)" nil t)
        (setq envlist
          (split-string (buffer-substring-no-properties
                         (match-beginning 1)(match-end 1)) ","))))
    envlist))

(defun cerbere--python-tox-get-root-directory()
  "Return the root directory to run test cases."
  (file-truename (or (locate-dominating-file
                      (buffer-file-name) "tox.ini")
                     "./")))

(defun cerbere--python-tox-extract-path ()
  "Extract python module from pathname."
  (subst-char-in-string
      ?/ ?.
      (file-name-sans-extension
       (substring (file-truename
                   (buffer-file-name))
                  (length (cerbere--python-tox-get-root-directory))))))

(defun cerbere--python-tox-get-command (tox-test &optional envlist)
  "Return the command to launch TOX-TEST with ENVLIST."
    (concat
     cerbere--python-tox-program " "
     cerbere--python-tox-arg " "
     (if envlist (concat "-e" envlist " "))
     tox-test))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro with-python-tox (current &optional askenvs &rest body)
  "Macro which initialize environments variables to launch unit tests."
    `(let ((toxenvs (if ,askenvs
			(completing-read
			 "Tox Environement: " (cerbere--python-tox-read-python-tox-ini-envlist))
		      cerbere--python-tox-default-env))
	   (default-directory (cerbere--python-tox-get-root-directory))
	   (compilation-auto-jump-to-first-error nil)
	   (compilation-scroll-output nil)
	   (,current (python-info-current-defun)))
       ,@body))


(defun cerbere--python-tox-current-test (&optional askenvs)
  "Launch tox on current test.
A prefix arg will ask for a env to use which is by default what
specified in `cerbere--python-tox-default-env'."
  (interactive "P")
  (with-python-tox current askenvs
     (unless current
       (error "No function at point"))
     (cerbere--build (cerbere--python-tox-get-command
		      (concat (cerbere--python-tox-extract-path) ":" current)
		      toxenvs))))

(defun cerbere--python-tox-current-class (&optional askenvs)
  "Launch tox on current class.
A prefix arg will ask for a env to use which is by default what
specified in `cerbere--python-tox-default-env'."
  (interactive "P")
  (with-python-tox current askenvs
     (if current
	 (let ((current-class (car (split-string current "\\."))))
	   (cerbere--build (cerbere--python-tox-get-command
			    (concat (cerbere--python-tox-extract-path) ":" current-class)
			    toxenvs)))
       (error "No class at point"))))


;; Not use in CERBERE.
;; FIXME: try to use it
;; (defun cerbere--python-tox-current-module (&optional askenvs)
;;   "Launch tox on current module.
;; A prefix arg will ask for a env to use which is by default what
;; specified in `cerbere--python-tox-default-env'."
;;   (interactive "P")
;;   (with-python-tox current askenvs
;;      (if current
;; 	 (cerbere--build (cerbere--python-tox-get-command
;;                          (cerbere--python-tox-extract-path) toxenvs)))))


(defun cerbere--python-tox-current-project (&optional askenvs)
  "Launch tox on current project.
A prefix arg will ask for a env to use which is by default what
specified in `cerbere--python-tox-default-env'."
  (interactive "P")
  (with-python-tox current askenvs
     (if current
	 (cerbere--build (cerbere--python-tox-get-command "" toxenvs)))))

;;;###autoload
(defun cerbere-python-tox (command &optional test)
  "Tox cerbere backend."
  (pcase command
    (`test (cerbere--python-tox-current-test))
    (`file (cerbere--python-tox-current-class))
    (`project (cerbere--python-tox-current-project))))


;;; End tox.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cerbere-python-tox)
;;; cerbere-python-tox.el ends here
