;;; test-elpaso.el --- Tests for elpaso -*- lexical-binding: t; -*-

;; Copyright (C) 2021 The Authors of elpaso.el

;; Authors: dickmao <github id: dickmao>
;; URL: https://github.com/dickmao/elpaso

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

;; Test stuff.

;;; Code:

(require 'elpaso)
(require 'elpaso-dev)
(require 'ert)
(require 'tar-mode)
(require 'use-package)

(defmacro test-elpaso-for-mock (testdir &rest body)
  (declare (indent defun))
  `(dolist (mock '("" "mockhub.com/package.git" "mockhub.com/recipes.git"))
     (let ((default-directory (elpaso-admin--sling ,testdir mock)))
       ,@body)))

(cl-defmacro test-elpaso--doit (&rest body &key specs &allow-other-keys)
  (declare (indent defun))
  `(unwind-protect
       (let* ((elpaso-defs-toplevel-dir
	       (expand-file-name "test" elpaso-dev-toplevel-dir))
	      (default-directory elpaso-defs-toplevel-dir)
	      (user-emacs-directory default-directory)
	      (package-user-dir (locate-user-emacs-file "elpa"))
	      use-package-ensure-function
	      elpaso-admin--cookbooks-alist
	      elpaso-defs-install-dir
	      elpaso-admin-cookbooks
	      package-alist
	      package-activated-list
	      package-archives
	      package-archive-contents
	      (package-directory-list
	       (eval (car (get 'package-directory-list 'standard-value))))
	      (package-load-list
	       (eval (car (get 'package-load-list 'standard-value))))
	      (package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
	      elpaso-admin--specs)
         (test-elpaso-for-mock default-directory
           (delete-directory ".git" t)
	   (with-temp-buffer
	     (unless (zerop (elpaso-admin--call t "git" "init"))
	       (error "%s (init): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (elpaso-admin--call t "git" "config" "user.name" "kilroy"))
	       (error "%s (name): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (elpaso-admin--call t "git" "config" "user.email" "kilroy@wuz.here"))
	       (error "%s (email): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (elpaso-admin--call t "git" "add" "."))
	       (error "%s (add): %s" default-directory (buffer-string))))
	   (with-temp-buffer
	     (unless (zerop (elpaso-admin--call t "git" "commit" "-am" "initial commit"))
	       (error "%s (commit): %s" default-directory (buffer-string)))))
	 (customize-set-variable 'elpaso-defs-install-dir (locate-user-emacs-file "elpaso"))
	 (customize-set-variable 'use-package-ensure-function
				 'elpaso-use-package-ensure-function)
	 (delete-directory package-user-dir t)
	 (make-directory package-user-dir t)
	 (delete-directory elpaso-admin--build-dir t)
	 (delete-directory elpaso-admin--recipes-dir t)
	 (customize-set-variable 'elpaso-admin--cookbooks-alist
				 '((user :file "recipes")
				   (rtest :url "mockhub.com/recipes.git" :file "recipes")))
	 (customize-set-variable 'elpaso-admin-cookbooks '(user rtest))
	 (elpaso-refresh)
	 (should (file-readable-p
		  (elpaso-admin--sling elpaso-admin--recipes-dir "rtest/recipes")))
	 (let ((recipes (expand-file-name "user/recipes" elpaso-admin--recipes-dir)))
	   (should (file-exists-p recipes))
	   (with-temp-file recipes
	     (insert ";; -*- lisp-data -*-" "\n\n"
		     (cl-prin1-to-string ,specs)
		     "\n")))
	 (when ,specs
	   (should (member (car ,specs) (elpaso-admin--get-specs))))
	 (progn ,@body))
     (test-elpaso-for-mock (expand-file-name "test" elpaso-dev-toplevel-dir)
       (delete-directory ".git" t))))

(ert-deftest test-elpaso-basic ()
  (test-elpaso--doit t))

(ert-deftest test-elpaso-fetch ()
  (test-elpaso--doit
    (should-error (elpaso-admin-for-pkg 'test (elpaso-admin-batch-fetch)))
    (elpaso-admin-for-pkg 'ptest (elpaso-admin-batch-fetch))))

(ert-deftest test-elpaso-build ()
  (test-elpaso--doit
    :specs `(("utest" :url ,(elpaso-admin--sling "mockhub.com/package.git") :files ("lisp/*" (:exclude "lisp/ptest.el"))))
    (elpaso-admin-for-pkg 'utest
      (elpaso-admin-batch-fetch)
      (elpaso-admin-batch-build)
      (should (file-directory-p (elpaso-admin--sling elpaso-admin--build-dir "utest/lisp")))
      (with-temp-buffer
	(insert-file-contents-literally
	 (elpaso-admin--sling elpaso-admin--archive-dir "utest-0.5.0.tar"))
	(tar-mode)
	(should (cl-some (lambda (descriptor)
			   (string= "utest-pkg.el"
				    (file-name-nondirectory (tar-header-name descriptor))))
			 tar-parse-info))
	(should-not (cl-some (lambda (descriptor)
			       (string= "ptest.el"
					(file-name-nondirectory (tar-header-name descriptor))))
			     tar-parse-info))))
    (elpaso-admin-for-pkg 'ptest
      (elpaso-admin-batch-fetch)
      (elpaso-admin-batch-build)
      (with-temp-buffer
	(insert-file-contents-literally
	 (elpaso-admin--sling elpaso-admin--archive-dir "ptest-0.5.0.tar"))
	(tar-mode)
	(should (cl-some (lambda (descriptor)
			   (string= "ptest-pkg.el"
				    (file-name-nondirectory (tar-header-name descriptor))))
			 tar-parse-info))
	(should-not (cl-some (lambda (descriptor)
			       (string= "utest.el"
					(file-name-nondirectory (tar-header-name descriptor))))
			     tar-parse-info))))))

(ert-deftest test-elpaso-install ()
  (test-elpaso--doit
    :specs `(("utest" :url ,(elpaso-admin--sling "mockhub.com/package.git") :files ("lisp/*" (:exclude "lisp/ptest.el"))))
    (elpaso-install "utest")))

(defalias 'dont-macroexpand-me ()
  (lambda ()
    ))

;; Says ert-deftest:
;; Macros in BODY are expanded when the test is defined, not when it
;; is run.  If a macro (possibly with side effects) is to be tested,
;; it has to be wrapped in `(eval (quote ...))'.
;; This is what Patrice O'Neal would call "tricky sh_t"
(ert-deftest test-elpaso-use-package-ensure ()
  (test-elpaso--doit
    :specs `(("utest" :url ,(elpaso-admin--sling "mockhub.com/package.git") :files ("lisp/*" (:exclude "lisp/ptest.el"))))
    (should-not (package-installed-p 'utest))
    (eval (quote (use-package utest :ensure t)))
    (should (package-installed-p 'utest))))

(provide 'test-elpaso)

;;; test-elpaso.el ends here
