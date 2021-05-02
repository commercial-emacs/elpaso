;;; elpaso-defs.el --- elpaso basic definitions  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021  The Authors

;; Author: The Authors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup elpaso nil "elisp package archive self officiator" :group 'applications)

(defface elpaso-face-name
  '((t :inherit link))
  "Face used on package names in the package menu."
  :version "25.1")

(defface elpaso-face-description
  '((t :inherit default))
  "Face used on the status and version of available packages."
  :version "25.1")

(defconst elpaso-defs-call-process-buffer-name "*elpaso-call-process*")
(defsubst elpaso-defs-call-process (command &optional destination)
  (cl-destructuring-bind (program &rest args)
      (split-string command)
    (apply #'call-process
  	   program
  	   nil
           (or destination
  	       (list (get-buffer-create elpaso-defs-call-process-buffer-name) t))
  	   nil
  	   args)))

(defcustom elpaso-defs-install-dir (locate-user-emacs-file "elpaso")
  "Base of elpaso operations."
  :type 'directory
  :set (lambda (symbol value)
         (set-default symbol value)
         (make-directory value t))
  :group 'elpaso)

(defconst elpaso-defs-toplevel-dir elpaso-defs-install-dir
  "Const in normal operation, but we pull a fast one in `elpaso-dev'.")

(provide 'elpaso-defs)
;;; elpaso-defs.el ends here
