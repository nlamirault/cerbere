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



(defun cerbere--build (command)
  "Launch a `COMMAND'."
  (compile command))


;; (defun notify-compilation-result (buffer msg)
;;   "Notify that the compilation is finished,
;; close the *compilation* buffer if the compilation is successful,
;; and set the focus back to Emacs frame"
;;   (if (string-match "^finished" msg)
;;       (progn
;; 	(delete-windows-on buffer)
;; 	(tooltip-show "\n Testing Successful :-) \n "))
;;     (tooltip-show "\n Tests Failed :-( \n "))
;;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;   (select-frame-set-input-focus current-frame))

;; (add-to-list 'compilation-finish-functions
;; 	     'notify-compilation-result)

(provide 'cerbere-common)
;;; cerbere-common.el ends here
