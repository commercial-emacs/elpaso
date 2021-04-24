;;; elpaso-dev.el --- shortcut  -*- lexical-binding:t -*-

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

;; Of particular value is melpa's sense for what file patterns belong in a package.

;;; Code:

(require 'subr-x)

(defconst elpaso-dev-toplevel-dir
  (with-temp-buffer
    (if (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))
      (error "elpaso-dev: not in a git directory"))))

(defun elpaso-dev ()
  "Set `elpaso-defs-toplevel-dir' to source directory."
  (interactive)
  (setq elpaso-defs-toplevel-dir elpaso-dev-toplevel-dir)
  (let ((elpaso-defs-toplevel-dir elpaso-defs-toplevel-dir))
    (elpaso-dev-load (split-string "test/test-elpaso.el"))))

(defun elpaso-dev-load (&optional add)
  (let ((default-directory elpaso-defs-toplevel-dir))
    (dolist (file
             (append add
                     (with-temp-buffer
                       (save-excursion (apply #'call-process "bash" nil t nil (split-string "cask files")))
                       (cl-loop until (eobp)
                                collect (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position))
                                do (forward-line)))))
      (let ((load-path load-path))
        (add-to-list 'load-path (file-name-directory file))
        (load-file file)))))

(provide 'elpaso-dev)
;;; elpaso-dev.el ends here
