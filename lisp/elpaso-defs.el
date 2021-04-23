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

(require 'subr-x)

(defgroup elpaso nil "elisp package archive self officiator" :group 'applications)

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

(defconst elpaso-defs-toplevel-dir
  (with-temp-buffer
    (if (zerop (elpaso-defs-call-process "git rev-parse --show-toplevel" t))
        (string-trim (buffer-string))
      elpaso-defs-install-dir)))

(provide 'elpaso-defs)
;;; elpaso-defs.el ends here
