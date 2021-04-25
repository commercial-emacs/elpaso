;;; elpaso.el --- Emacs lisp package archive self officiator  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 The Authors of elpaso.el

;; Authors: dickmao <github id: dickmao>
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

;; Self-officiating package manager for the emacs text editor.  Like quelpa_.
;;
;; Elpaso abjures the middle-man role played by package archive operators like
;; ELPA and MELPA, building packages directly from upstream sources.
;;
;; ::
;;
;;     M-x elpaso-refresh
;;     M-x elpaso-install
;;     M-x elpaso-delete
;;

;;; Code:

(require 'package)
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
  (if (equal package "elpaso")
      ;; elpaso must be bootstrapped from source
      (message "elpaso-install: nice try")
    (elpaso-admin-for-pkg package (elpaso-admin-batch-install))
    (package-menu--post-refresh)))

;;;###autoload
(defalias 'elpaso #'elpaso-install)

;;;###autoload
(defun elpaso-refresh (&optional cookbook)
  "Refresh COOKBOOK, generally one of user, melpa, elpa, nongnu."
  (interactive (when current-prefix-arg
		 (list (completing-read
			"Archive: "
			(mapcar #'symbol-name elpaso-admin-cookbooks)
			nil t nil))))
  (dolist (c (if cookbook (list cookbook) elpaso-admin-cookbooks))
    (elpaso-admin-for-pkg c (elpaso-admin-batch-refresh)))
  (message nil))

(defalias 'elapso #'elpaso-install)
(defalias 'elapso-install #'elpaso-install)
(defalias 'elapso-delete #'elpaso-delete)
(defalias 'elapso-refresh #'elpaso-refresh)

(when (equal elpaso-defs-toplevel-dir elpaso-defs-install-dir)
  (let ((default-directory elpaso-defs-toplevel-dir))
    (if (not (executable-find "git"))
        (display-warning 'elpaso "git program not found" :error)
      (unless (zerop (elpaso-admin--call nil "git" "rev-parse" "--show-toplevel"))
        (with-temp-buffer
          (unless (zerop (elpaso-admin--call t "git" "init" "--bare"))
            (error "elpaso abort: %s" (buffer-string))))))))

(provide 'elpaso)
;;; elpaso.el ends here
