;;; ert-loader.el --- Load Ert if not included in Emacs

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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


(require 'f)
(eval-when-compile
  (require 'cl))


(defvar cerbere-test-root-path
  (f-parent (f-parent load-file-name))
  "Path to root.")

(defvar cerbere-vendor-path
  (f-expand "vendor" cerbere-root-path)
  "Path to vendor.")

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" cerbere-vendor-path)))


(provide 'ert-loader.el)
;;; ert-loader.el ends here
