;; test-helper.el --- Test helpers for cerbere

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Copyright (C) 2014  Nicolas Lamirault

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

(defmacro cerbere-with-test-content (file-name &rest body)
  "Setup a buffer backing FILE-NAME with CONTENT and run BODY in it."
    (declare (indent 1))
    `(save-excursion
       (with-current-buffer (find-file-noselect
                             (concat cerbere-test-path "data/" ,file-name))
         (goto-char (point-min))
         ,@body
         (kill-buffer))))

(provide 'test-helper)
;;; test-helper.el ends here
