;;; elpaso.el --- Elisp package archive self officiator  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 The Authors of elpaso.el

;; Authors: dickmao <github id: dickmao>
;; Based on code written by Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.1.0
;; Keywords: maint tools
;; URL: https://github.com/dickmao/elpaso
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with elpaso.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Self-officiating package manager for the emacs text editor.
;;
;; ::
;;
;;     [C-u] M-x elpaso-refresh
;;     M-x elpaso-install
;;     M-x elpaso-delete
;;

;;; Code:

(require 'package)
(require 'subr-x)
(require 'elpaso-defs)
(require 'elpaso-admin)

;;;###autoload
(defun elpaso-delete ()
  "Merely calls `package-delete' but spares guff about dependencies."
  (interactive)
  (let ((cease-and-desist
	 (lambda (args)
	   (setf (nthcdr 1 args) (cons t (nthcdr 2 args)))
	   args)))
    (unwind-protect
	(progn
	  (add-function :filter-args (symbol-function 'package-delete) cease-and-desist)
	  (call-interactively #'package-delete))
      (remove-function (symbol-function 'package-delete) cease-and-desist))))

;;;###autoload
(defun elpaso-install (package)
  "Fetch and install PACKAGE directly from git forges."
  (interactive (list (completing-read
                      "Package: "
                      (mapcar #'car (let ((inhibit-message t))
				      (elpaso-admin--get-specs)))
                      nil t nil)))
  (elpaso-admin-for-pkg package (elpaso-admin-batch-install)))

;;;###autoload
(defalias 'elpaso #'elpaso-install)

;;;###autoload
(defun elpaso-refresh (&optional cookbook)
  "Refresh COOKBOOK, generally one of melpa, elpa, nongnu."
  (interactive (when current-prefix-arg
		 (list (completing-read
			"Archive: "
			(mapcar #'symbol-name elpaso-admin-cookbooks)
			nil t nil))))
  (dolist (c (if cookbook (list cookbook) elpaso-admin-cookbooks))
    (elpaso-admin-for-pkg c (elpaso-admin-batch-refresh)))
  (message nil))

(provide 'elpaso)

;;; elpaso.el ends here
