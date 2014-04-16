;;; cerbere.el --- Unit testing in Emacs for several programming languages

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/cerbere
;; Version: 0.1.0
;; Keywords: python, go, php, tests, tdd

;; Package-Requires: ((s "1.9.0") (f "0.11.0") (go-mode "20131222") (pkg-info "0.5"))

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

(require 'pkg-info)
(require 's)
(require 'f)

;;(add-to-list 'load-path "/home/nlamirault/Perso/cerbere/")

(require 'cerbere-gotest)
(require 'cerbere-phpunit)
(require 'cerbere-tox)


(defgroup cerbere nil
  "Unit testing in Emacs for several programming languages."
  :group 'tools)

(defcustom cerbere-keymap-prefix (kbd "C-c c")
  "Cerbere keymap prefix."
  :group 'cerbere
  :type 'cerbere)

(defvar cerbere-package-version "0.1.0"
  "Release version of Cerbere.")


;;; user

;;;###autoload
(defun cerbere-version ()
  "Dislay the Cerbere's version."
  (interactive)
  ;;(message "Cerbere version: %s" cerbere-package-version)
  (let ((version (pkg-info-version-info 'cerbere)))
    (message "Cerbere %s" version)))


;;; backends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cerberes-backends
  '(("py" . cerbere-tox)
    ("go" . cerbere-gotest)
    ("php" . cerbere-phpunit))
  "The list of Cerbere backends.
Each backend provide several method for unit testing.")

(defun cerbere-add-backend (f-ext name)
  "Add a new backend for Cerbere.
`F-EXT' is the file extensions.
`NAME' is the backend name"
  (push (cons f-ext name) cerberes-backends))

(defun cerbere-find-backend (f-ext)
  "Search a candidate into all available backends using `F-EXT'"
  (message "ext: %s %s" f-ext cerberes-backends)
  (assoc f-ext cerberes-backends))

(defmacro with-backend (backend f-ext &rest body)
  "Macro which setup current `BACKEND' and execute `BODY'.
`F-EXT' is used to search the backend."
  `(let ((,backend (cerbere-find-backend ,f-ext)))
     (if ,backend
	 ,@body
       (error "No backend available"))))

(defun cerbere-call-backend (backend command)
  "Call `BACKEND' function using `COMMAND'."
  (funcall (cdr backend) command))

;;;###autoload
(defun cerbere-current-test ()
  "Launch backend on current test."
  (interactive)
  (with-backend backend (f-ext (buffer-file-name))
  ;; (let ((backend (cerbere-find-backend (f-ext (buffer-file-name)))))
  ;;   (if backend
	(cerbere-call-backend backend 'test)))
;;      (error "No backend available"))))

;;;###autoload
(defun cerbere-current-file ()
  "Launch backend on current file."
  (interactive)
  (with-backend backend (f-ext (buffer-file-name))
     (cerbere-call-backend backend 'file)))

;;;###autoload
(defun cerbere-current-project ()
  "Launch backend on current project."
  (interactive)
  (with-backend backend (f-ext (buffer-file-name))
     (cerbere-call-backend backend 'project)))



;;; Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar cerbere-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "v") 'cerbere-version)
      (define-key prefix-map (kbd "t") 'cerbere-current-test)
      (define-key prefix-map (kbd "f") 'cerbere-current-file)
      (define-key prefix-map (kbd "p") 'cerbere-current-project)
      (define-key map cerbere-keymap-prefix prefix-map))
    map)
  "Keymap used by `cerbere-mode'..")

(define-minor-mode cerbere-mode
  "Minor mode for Cerbere..

\\{cerbere-mode-map}"
  :lighter " Cerbere"
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
