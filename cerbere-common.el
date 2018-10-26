;;; cerbere-common.el --- Tools for Cerbere backends

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

(defvar cerbere-backends '()
  "The list of Cerbere backends.
Each backend provide several method for unit testing.")

(defun cerbere-backend-name (backend)
  "Return BACKEND supported file extension."
  (plist-get backend :name))

(defun cerbere-add-backend (backend)
  "Add BACKEND to the list of backends."
  (add-to-list 'cerbere-backends backend))

(defmacro cerbere-define-backend (name f-ext doc-string &rest props)
  "Add a new backend for Cerbere.
NAME is the backend name.

F-EXT is the file extensions.

DOC-STRING is the backend documentation.
PROPS is a list of properties defining backend apis:

`:run-test' should be a function that takes a test objects as
argument and run it.

`:test-at-point' should be a function that returns an object
identifing the test at cursor location or nil if there isn't.

`:test-for-file' should be a function that returns an object
identifing the test for the file associated to the current buffer
or nil if there isn't.

`:test-for-project' should be a function that returns an object
identifing the test for the project associated to the current buffer
or nil if there isn't.

A test object is a property list.  It should contain at list one
property: the name of your backend.  The other properties should
tell you enough information to run the test.  The run test
function should extract the needed information from the test
object and execute the identifed test.

\(cerbere-define-backend my-backend \"my-file-extension\"
  \"My backend documentation.\"
  :test-at-point (lamdba () '(:backend my-backend
                              :file \"MyJavaTest.java\"
                              :test \"testShouldFooBar\"))
  :test-for-file (lambda () '(:backend my-backend
                              :file \"MyJavaTest.java\"))
  :test-for-project (lambda () '(:backend my-backend
                                 :project \"/path/to/project\"))
  :run-test (lamdba (test) #'my-backend-run-test))"
  (declare (indent 2)
           (doc-string 3))
  (let* ((var-name (intern (format "cedere--backend-%s" name)))
         (run-test (plist-get props :run-test))
         (test-at-point (plist-get props :test-at-point))
         (test-for-file (plist-get props :test-for-file))
         (test-for-project (plist-get props :test-for-project)))
    (unless run-test (error "Backend should define :run-test function"))
    (unless test-at-point (error "Backend should define :test-at-point function"))
    (unless test-for-file (error "Backend should define :test-for-file function"))
    (unless test-for-project (error "Backend should define :test-for-project& function"))
    `(progn
       (if (boundp ',var-name) (makunbound ',var-name))
       (defvar ,var-name (list :name (quote ,name)
                               :f-ext ,f-ext
                               :run-test ,run-test
                               :test-at-point ,test-at-point
                               :test-for-file ,test-for-file
                               :test-for-project ,test-for-project))
       (cerbere-add-backend ,var-name))))

;; shamelessly stolen from dash.
(defun cerbere-flatten (l)
  "Take a nested list L and return its contents as a single, flat list.

Note that because nil represents a list of zero elements (an
empty list), any mention of nil in L will disappear after
flattening.  If you need to preserve nils, consider `-flatten-n'
or map them to some unique symbol and then map them back.

Conses of two atoms are considered \"terminals\", that is, they
aren't flattened further."
  (declare (pure t) (side-effect-free t))
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'cerbere-flatten l)
    (list l)))

(defun cerbere--build (command)
  "Launch a COMMAND."
  (compile command))

(defun cerbere-build (directory command)
  "Change to DIRECTORY and run COMMAND."
  (let ((default-directory directory))
    (cerbere--build command)))

(defun cerbere-shell-arg (arg)
  "Return a string representation of ARG suitable for use in a shell command."
  (shell-quote-argument (with-output-to-string (princ arg))))

(defun cerbere-command (&rest args)
  "Remove nils from ARGS and join all its remaining elemts with a space."
  (declare (indent 0))
  (string-join (seq-map #'cerbere-shell-arg (cerbere-flatten args)) " "))

(defun cerbere-buffer-file-name ()
  "Return the basename of the file associated to the current buffer."
  (file-name-nondirectory buffer-file-name))

(defun cerbere-project-root (file)
  "Locate current project root by looking up for FILE."
  (let ((root (locate-dominating-file (buffer-file-name) file)))
    (when root
        (file-name-as-directory (file-name-directory (expand-file-name root))))))

(provide 'cerbere-common)
;;; cerbere-common.el ends here
