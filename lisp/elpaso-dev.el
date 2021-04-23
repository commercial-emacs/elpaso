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

(defun elpaso-dev-ert ()
  (interactive)
  (elpaso-dev-load (split-string "test/test-elpaso.el"))
  (ert t))

(defun elpaso-dev-load (&optional add)
  (interactive)
  (let ((default-directory (with-temp-buffer
                             (save-excursion (apply #'call-process "git" nil t nil
                                                    (split-string "rev-parse --show-toplevel")))
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))
    (dolist (file
             (append add
                     (with-temp-buffer
                       (save-excursion (apply #'call-process "bash" nil t nil (split-string "cask files")))
                       (cl-loop until (eobp)
                                collect (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position))
                                do (forward-line)))))
      (load-file file))))
