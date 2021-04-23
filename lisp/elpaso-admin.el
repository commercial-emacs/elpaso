;;; elpaso-admin.el --- elpaso guts based on elpa-admin.el  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021 The Authors

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

;; In the interest of code coverage, I gutted most of smonnier's elpa-admin.el.
;; Ideally, we converge to something that can be used for self-officiating
;; and remote-officiating package archives (gallery: that's not going to happen).

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'package)
(require 'message)
(require 'elpaso-defs)
(require 'elpaso-milky)

(defvar elpaso-admin--specs nil "Regenerate with fetched cookbooks.")

(defconst elpaso-admin--ref-master-dir "refs/remotes/master")

(defconst elpaso-admin--cookbooks-alist
  '((user :file "recipes")
    (melpa
     :url "https://github.com/melpa/melpa.git"
     :dir "recipes")
    (elpa
     :url "https://git.savannah.gnu.org/git/emacs/elpa.git"
     :file "elpa-packages")
    (nongnu
     :url "https://git.savannah.gnu.org/git/emacs/nongnu.git"
     :file "elpa-packages"))
  "Recipe urls.")

(defcustom elpaso-admin-cookbooks
  (mapcar #'car elpaso-admin--cookbooks-alist)
  "Where to crib recipes, in order of priority!"
  :group 'elpaso
  :set (lambda (symbol value)
	 (when-let ((culprit
		     (cl-find-if
		      (lambda (x)
			(not (memq x (mapcar #'car
					     elpaso-admin--cookbooks-alist))))
		      value)))
	   (error "elpaso-admin-cookbooks: not setting %s for %s" symbol culprit))
	 (setq elpaso-admin--specs nil)
         (set-default symbol value))
  :type '(repeat symbol))

(defvar elpaso-admin--sandbox nil
  "If non-nil, run some of the less trusted commands in a sandbox.
This is recommended when building packages from untrusted sources,
but this requires Bubblewrap to be installed and has only been tested
on some Debian systems.")

(defvar elpaso-admin--debug nil)

(defconst elpaso-admin--build-dir "packages")
(defconst elpaso-admin--archive-dir "archive")
(defconst elpaso-admin--recipes-dir "recipes")

(cl-defun elpaso-admin--get-package-spec (name
					  &aux
					  (name (if (symbolp name) (symbol-name name) name)))
  (let ((spec (assoc name (elpaso-admin--get-specs))))
    (cond (spec spec)
	  ((package-built-in-p (intern name)) nil)
	  (t (error "Unknown package %s" name)))))

(defsubst elpaso-admin--spec-get (pkg-spec prop &optional default)
  (or (plist-get (cdr pkg-spec) prop) default))

(defmacro elpaso-admin-for-pkg (name &rest body)
  (declare (indent defun))
  `(let ((name* (if (symbolp ,name) (symbol-name ,name) ,name)))
     ,@(mapcar (lambda (expr)
                 `(let ((command-line-args-left (list name*)))
                    ,expr))
               body)))

(defmacro elpaso-admin--sling (&rest args)
  (declare (indent defun))
  `(directory-file-name
    (mapconcat #'file-name-as-directory
	       (list ,@args)
	       "")))

(defun elpaso-admin--form-from-file-contents (filename)
  (with-temp-buffer
    (save-excursion (insert-file-contents filename))
    (read (current-buffer))))

(defun elpaso-admin--message (&rest args)
  (when elpaso-admin--debug (apply #'message args)))

(defconst elpaso-admin--another-re-no-dot
  "[^.]\\|\\.\\.\\."
  "Regexp matching any file name except \".\" and \"..\".")

(defconst elpaso-admin--re-no-dot "\\`\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regular expression matching all files except \".\" and \"..\".")

(defun elpaso-admin--version-to-list (vers)
  (when vers
    (let ((l (version-to-list vers)))
      ;; Signal an error for things like "1.02" which is parsed as "1.2".
      (cl-assert (equal vers (package-version-join l)) nil
                 "Unsupported version syntax %s" vers)
      l)))

(defun elpaso-admin--convert-require (elt)
  (let ((vers (elpaso-admin--version-to-list (car (cdr elt)))))
    (if vers
        (list (car elt) vers)
      (list (car elt)))))

(defun elpaso-admin--dirname (dir &optional base)
  (file-name-as-directory (expand-file-name dir base)))

(defmacro elpaso-admin--specs-filter (lst)
  (declare (indent defun))
  `(cl-remove-if (lambda (spec)
                   (or (elpaso-admin--spec-get spec :core)
                       (and (not (elpaso-admin--spec-get spec :url))
                            (let ((fetcher (elpaso-admin--spec-get spec :fetcher)))
                              (or (not fetcher)
                                  (not (memq fetcher '(github gitlab mockhub))))))))
                 ,lst))

(defun elpaso-admin--get-specs ()
  (interactive)
  (unless elpaso-admin--specs
    (let ((default-directory elpaso-defs-toplevel-dir))
      (dolist (repo* elpaso-admin-cookbooks elpaso-admin--specs)
        (let* ((repo (symbol-name repo*))
               (spec (elpaso-admin--get-cookbook-spec repo))
               (file (elpaso-admin--spec-get spec :file))
               (dir (elpaso-admin--spec-get spec :dir))
	       (stringify-car
		(lambda (contents)
		  (when (symbolp (car contents))
		    ;; accommodate gnu format upon which
		    ;; elpa-admin.el was based
		    (setcar contents (symbol-name (car contents))))
		  contents)))
          (cond (file
                 (let ((path (elpaso-admin--sling
                               elpaso-admin--recipes-dir repo file)))
                   (unless (file-readable-p path)
                     (let (elpaso-admin--specs)
                       (elpaso-admin--refresh-one-cookbook spec)))
                   (if (file-readable-p path)
                       (setq elpaso-admin--specs
                             (append elpaso-admin--specs
                                     (elpaso-admin--specs-filter
                                       (mapcar stringify-car
					       (elpaso-admin--form-from-file-contents path)))))
                     (message "elpaso-admin--get-specs: unreadable %s" path))))
                (dir
                 (let ((path (elpaso-admin--sling
                               elpaso-admin--recipes-dir repo dir)))
                   (unless (file-directory-p path)
                     (let (elpaso-admin--specs)
                       (elpaso-admin--refresh-one-cookbook spec)))
                   (if (file-directory-p path)
                       (setq elpaso-admin--specs
                             (append
                              elpaso-admin--specs
                              (elpaso-admin--specs-filter
                                (mapcar
				 (lambda (file)
				   (let ((contents
					  (elpaso-admin--form-from-file-contents file)))
				     (funcall stringify-car contents)))
				 (directory-files path t elpaso-admin--re-no-dot t)))))
                     (message "elpaso-admin--get-specs: not a directory %s" path)))))))))
  elpaso-admin--specs)

(defun elpaso-admin--main-file (pkg-spec dir)
  (cl-flet ((get (what) (elpaso-admin--spec-get pkg-spec what)))
    (let* ((name (car pkg-spec))
           (el (format "%s.el" name))
           (main-file (get :main-file))
           (lisp-dir (get :lisp-dir))
           (files (get :files)))
      (cond (main-file main-file)
            (lisp-dir (concat (file-name-as-directory lisp-dir) el))
            (files (elpaso-milky-locate dir el files))
            (t el)))))

(defun elpaso-admin--pkg-file (pkg-spec dir)
  (let* ((name (car pkg-spec))
         (el (format "%s-pkg.el" name))
         (files (elpaso-admin--spec-get pkg-spec :files)))
    (elpaso-milky-locate dir el files)))

(defun elpaso-admin--refspec (pkg-spec)
  (let* ((ref-master (elpaso-admin--ref-master pkg-spec))
         (branch (file-name-nondirectory ref-master)))
    (if (string= "HEAD" branch)
        (format "+HEAD:%s" ref-master)
      (format "+refs/heads/%s:%s" branch ref-master))))

(defun elpaso-admin--ref-master (pkg-spec)
  (let ((branch (or (elpaso-admin--spec-get pkg-spec :release-branch)
                    (elpaso-admin--spec-get pkg-spec :branch)
                    "HEAD"))
        (what (car pkg-spec)))
    (elpaso-admin--sling elpaso-admin--ref-master-dir
      (if (symbolp what) (symbol-name what) what) branch)))

(defun elpaso-admin--build-tar-transform (name r)
  (let ((from (nth 0 r)) (to (nth 1 r)))
    (cl-assert (not (string-match "[][*\\|?]" from)))
    (cl-assert (not (string-match "[][*\\|?]" to)))
    (format "--transform=s|^packages/%s/%s|packages/%s/%s|"
            name
            (if (string-match "/\\'" from)
                (concat (substring from 0 -1) "\\($\\|/\\)")
              (concat from "$"))
            name to)))

(defun elpaso-admin--temp-file (f)
  (when (boundp 'elpaso-admin--temp-files)
    (push (if (stringp f) (expand-file-name f) f) elpaso-admin--temp-files)))

(defun elpaso-admin--build-one-tarball (tarball dir pkg-spec metadata)
  "Create file TARBALL for NAME if not done yet.
Return non-nil if a new tarball was created."
  (elpaso-admin--message "Building tarball %s..." tarball)
  (let* ((destdir (let ((result (file-name-directory tarball)))
                    (prog1 result
                      (make-directory result t))))
         (build-dir elpaso-admin--build-dir)
         (name (car pkg-spec))
         (vers (nth 0 metadata))
         (elpaignore (expand-file-name ".elpaignore" dir))
         (files (elpaso-admin--spec-get pkg-spec :files))
         (ignores (elpaso-admin--spec-get pkg-spec :ignored-files))
         (renames (elpaso-admin--spec-get pkg-spec :renames))
         (ldir (elpaso-admin--spec-get pkg-spec :lisp-dir))
         (tardir (concat (file-name-as-directory build-dir) name)))
    (when ldir
      (cl-pushnew (list (file-name-as-directory ldir) "") renames
                  :test #'equal))
    ;; Run `make' before building the Info file, so that the `make' rule
    ;; can be used to build the Info/Texinfo file.
    (elpaso-admin--make pkg-spec dir)
    (elpaso-admin--build-Info pkg-spec dir)
    (elpaso-admin--write-pkg-file dir name metadata)
    (when files
      (unless (elpaso-admin--pkg-file pkg-spec dir)
        (push (concat name "-pkg.el") files)))
    ;; FIXME: Allow renaming files or selecting a subset of the files!
    (cl-assert (not (string-match "[][*\\|?]" name)))
    (cl-assert (not (string-match "[][*\\|?]" vers)))
    (if (or ignores renames)
        (apply #'elpaso-admin--call
               nil "tar"
               `("--exclude-vcs"
                 ,@(cond
                    (ignores
                     (mapcar (lambda (i) (format "--exclude=%s/%s/%s" build-dir name i))
                             ignores))
                    ((file-readable-p elpaignore) `("-X" ,elpaignore)))
                 ,@(mapcar (lambda (r) (elpaso-admin--build-tar-transform name r))
                           renames)
                 "--transform"
                 ,(format "s|^%s/%s|%s-%s|" build-dir name name vers)
                 "-chf" ,tarball
                 ,tardir))
      (let* ((mapping (elpaso-milky-expand-file-specs
                       (expand-file-name tardir default-directory)
                       (elpaso-milky-config-file-list files)))
             (seds* (mapcar
                     (lambda (x)
                       (cl-destructuring-bind (s . d) x
                         (format "s|^%s/%s|%s-%s/%s|"
                                 (regexp-quote tardir)
                                 (regexp-quote (or (file-name-directory s) ""))
                                 name vers
                                 (or (file-name-directory d) ""))))
                     mapping))
             (seds (cl-mapcan (lambda (x) (list "--transform" x)) (delete-dups seds*))))
        (apply #'elpaso-admin--call-region
               nil "tar"
               (mapcar (lambda (pair) (elpaso-admin--sling tardir (car pair)))
                       mapping)
               `(,@seds
                 "-chf" ,tarball
                 "--files-from" "-"))))
    (let ((pkgdesc
           ;; FIXME: `elpaso-admin--write-pkg-file' wrote the metadata to
           ;; <pkg>-pkg.el and then `elpaso-admin--process-multi-file-package'
           ;; reads it back.  We could/should skip the middle man.
           (elpaso-admin--process-multi-file-package
            dir name 'dont-rename)))
      (elpaso-admin--message "%s: %s" name pkgdesc)
      (let ((link (expand-file-name (format "%s.tar" name) destdir)))
        (when (file-symlink-p link) (delete-file link))
        (make-symbolic-link (file-name-nondirectory tarball) link))
      (message "Built %s" tarball)
      'new)))

(cl-defun elpaso-admin--get-cookbook-spec (name
                                           &aux
                                           (name (if (stringp name) (intern name) name)))
  (if-let ((spec (assq name elpaso-admin--cookbooks-alist)))
      spec
    (error "Unknown cookbook %s" name)))

(defun elpaso-admin-batch-build (&rest _)
  "Build the new tarballs (if needed) for one particular package."
  (while command-line-args-left
    (elpaso-admin--build-one-package (elpaso-admin--get-package-spec
                                     (pop command-line-args-left)))))

(defun elpaso-admin--remove-one-package (pkg-spec)
  (let* ((default-directory elpaso-defs-toplevel-dir)
	 (name (car pkg-spec))
	 (ref-master (elpaso-admin--ref-master pkg-spec))
         (packages-dir elpaso-admin--build-dir)
         (pkg-dir (expand-file-name name packages-dir)))
    (with-temp-buffer
      (unless (cl-every
	       #'zerop
	       (list (elpaso-admin--call t "git" "update-ref" "-d" ref-master)
		     (elpaso-admin--call t "git" "worktree" "remove" "-f" pkg-dir)))
	(error "elpaso-admin--remove-one-package: %s" (buffer-string))))
    (dolist (link (directory-files elpaso-admin--archive-dir t (format "%s-[0-9].*\\.tar\\'" name) t))
      (when (or (file-symlink-p link) (file-exists-p link))
        (delete-file link)))))

(defun elpaso-admin-purge ()
  (interactive)
  (let ((default-directory elpaso-defs-toplevel-dir))
    (with-temp-buffer
      (save-excursion (elpaso-admin--call t "git" "worktree" "list" "--porcelain"))
      (cl-loop with worktrees
	       with cand
	       until (eobp)
	       for line = (split-string (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	       do (cond ((and (string= "worktree" (car line)) (= 2 (length line)))
			 (setq cand (cadr line)))
			((and (string= "detached" (car line)) (= 1 (length line)))
			 (push cand worktrees)))
	       do (forward-line)
	       finally do (mapc
			   (lambda (x)
			     (elpaso-admin--call nil "git" "worktree" "remove" "-f" x))
			   worktrees)))
    (with-temp-buffer
      (save-excursion (elpaso-admin--call t "git" "for-each-ref" "--format=%(refname)"
					  elpaso-admin--ref-master-dir))
      (cl-loop until (eobp)
	       for line = (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	       do (elpaso-admin--call nil "git" "update-ref" "-d" line)
	       do (forward-line)))
    (dolist (link (directory-files elpaso-admin--archive-dir t ".*\\.tar\\'" t))
      (when (or (file-symlink-p link) (file-exists-p link))
        (delete-file link)))))

(defun elpaso-admin-batch-remove (&rest _)
  (while command-line-args-left
    (elpaso-admin--remove-one-package (elpaso-admin--get-package-spec
                                       (pop command-line-args-left)))))

(defun elpaso-admin-batch-refresh (&rest _)
  "Build the new tarballs (if needed) for one particular package."
  (while command-line-args-left
    (elpaso-admin--refresh-one-cookbook (elpaso-admin--get-cookbook-spec
                                         (pop command-line-args-left)))))

(defun elpaso-admin-batch-install (&rest _)
  (while command-line-args-left
    (elpaso-admin--install-one-package (elpaso-admin--get-package-spec
                                        (pop command-line-args-left)))))

(cl-defun elpaso-admin--install-file (name
                                      file
                                      &aux
                                      (name (if (stringp name) (intern name) name)))
  (mapc (lambda (odesc)
	  (when-let* ((odir (package-desc-dir odesc))
	              (leaf (file-name-nondirectory odir))
                      (backup-dir (expand-file-name "backups" elpaso-defs-install-dir))
                      (backup-name (let ((backup-directory-alist `(("." . ,backup-dir))))
                                     (car (find-backup-file-name leaf)))))
            (copy-directory odir backup-name t t)
            (package-delete odesc t)))
	(cdr (assq name package-alist)))
  (let ((wicked-gnarly
         (lambda (args)
           (cl-destructuring-bind (_l1 l2) args
             (when (version-list-<= '(19001201 1) l2)
               (message "elpaso-admin--install-file: bad required version %s"
                        (package-version-join l2))
               (setf (nth 1 args) (version-to-list "0pre"))))
           args)))
    (unwind-protect
        (progn
          (add-function :filter-args (symbol-function 'version-list-<) wicked-gnarly)
          (package-install-file file))
      (remove-function (symbol-function 'version-list-<) wicked-gnarly))))

(defun elpaso-admin--install-one-package (pkg-spec)
  (unless package--initialized
    (package-initialize 'no-activate))
  (cl-loop
   with target = (intern (car pkg-spec))
   ;; that package-alist elements have :archive set to nil is reassuring in light
   ;; of our scoping the archive-related special variables.
   with package-archive-contents = package-archive-contents
   with package-archives = (cl-remove-if (lambda (pair) (string= (car pair) "elpaso"))
                                         package-archives)
   with default-directory = elpaso-defs-toplevel-dir
   with seen = (list (list target))
   with queue = (list (list target))
   while queue
   do (cl-destructuring-bind (name
			      &optional version
			      &aux (version (or version (version-to-list "0pre"))))
	  (pop queue)
	(when (or (eq name target)
                  (not (package-installed-p name version)))
          (elpaso-admin-for-pkg name
            (elpaso-admin-batch-fetch)
            (elpaso-admin-batch-build))
          (when-let ((name-spec (if (eq name target)
			            pkg-spec
			          (elpaso-admin--get-package-spec name))))
            (let* ((name-dir (expand-file-name
			      (format "%s/%s" elpaso-admin--build-dir name)
			      elpaso-defs-toplevel-dir))
                   (main-desc
                    (when-let ((main-file (elpaso-admin--main-file name-spec name-dir)))
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name main-file name-dir))
		        (ignore-errors (package-buffer-info)))))
	           (pkg-desc
                    (when-let ((pkg-file (elpaso-admin--pkg-file name-spec name-dir)))
	              (with-temp-buffer
	                (insert-file-contents (expand-file-name pkg-file name-dir))
                        (package--read-pkg-desc 'tar))))
                   (guess-desc (if (and main-desc pkg-desc)
                                   (let ((main-reqs (package-desc-reqs main-desc))
                                         (pkg-reqs (package-desc-reqs pkg-desc)))
                                     (if (> (length main-reqs) (length pkg-reqs))
                                         main-desc
                                       pkg-desc))
                                 (or main-desc pkg-desc))))
	      (setf (package-desc-kind guess-desc) 'tar)
	      (setf (package-desc-archive guess-desc) "elpaso")
              (setf (alist-get name package-archive-contents) (list guess-desc))
              (mapc
	       (lambda (req)
	         (unless (memq (car req) (mapcar #'car seen))
	           (setq queue (append queue (list req)))
	           (push req seen)))
	       (package-desc-reqs guess-desc))))))
   finally do
   (let* ((dir (expand-file-name (symbol-name target) elpaso-admin--build-dir))
          (metadata (elpaso-admin--metadata dir pkg-spec))
          (vers (nth 0 metadata))
          (tarball (expand-file-name
                    (format "dist/%s-%s.tar" target vers)
                    dir)))
     (add-to-list 'package-archives `("elpaso" . ,(file-name-as-directory elpaso-admin--archive-dir)))
     (if (file-readable-p tarball)
         (progn
           (elpaso-admin--install-file target tarball)
	   (dolist (dep (mapcar #'car seen))
	     (if (eq dep target)
	         (elpaso-admin--remove-one-package pkg-spec)
	       (when-let ((to-remove (elpaso-admin--get-package-spec dep)))
	         (ignore-errors (elpaso-admin--remove-one-package to-remove))))))
       (error "elpaso-admin--install-one-package: %s not found" tarball)))))

(defun elpaso-admin--refresh-one-cookbook (spec)
  (setq elpaso-admin--specs nil)
  (let* ((default-directory elpaso-defs-toplevel-dir)
	 (name (symbol-name (car spec)))
         (recipes-dir (expand-file-name name elpaso-admin--recipes-dir))
         (url (elpaso-admin--spec-get spec :url))
         (file (elpaso-admin--spec-get spec :file))
         (dir (elpaso-admin--spec-get spec :dir)))
    (make-directory recipes-dir t)
    (cond (url
           (elpaso-admin--fetch-one-package spec)
           (elpaso-admin--worktree-sync spec recipes-dir))
          (file
           (let ((path (expand-file-name file recipes-dir)))
             (unless (file-exists-p path)
               (with-temp-file path
                 (insert ";; -*- lisp-data -*-" "\n\n" "()\n")))))
          (dir
           (let ((path (expand-file-name file recipes-dir)))
             (unless (file-directory-p path)
               (make-directory path t)))))))

(defun elpaso-admin--build-one-package (pkg-spec)
  "Build the new tarballs (if needed) for PKG-SPEC."
  (if (eq (nth 1 pkg-spec) :core)
      (error "elpaso-admin--build-one-package: core unhandled")
    (let* ((name (car pkg-spec))
           (packages-dir elpaso-admin--build-dir)
           (pkg-dir (expand-file-name name packages-dir)))
      (make-directory packages-dir t)
      (elpaso-admin--worktree-sync pkg-spec pkg-dir)))
  (let* ((default-directory elpaso-defs-toplevel-dir)
         (name (car pkg-spec))
         (dir (expand-file-name name elpaso-admin--build-dir))
         (metadata (elpaso-admin--metadata dir pkg-spec))
         (vers (nth 0 metadata)))
    ;; First, try and build the devel tarball
    ;; Do it before building the release tarball, because building
    ;; the release tarball may revert to some older commit.
    (let* ((tarball (format "%s-%s.tar" name vers))
           (tarpath (expand-file-name tarball (elpaso-admin--sling dir "dist")))
           (archive-path (expand-file-name tarball elpaso-admin--archive-dir)))
      (elpaso-admin--build-one-tarball tarpath dir pkg-spec metadata)
      (make-directory (file-name-directory archive-path) t)
      (dolist (link (directory-files (file-name-directory archive-path) t (format "%s-[0-9].*\\.tar\\'" name) t))
        (when (or (file-symlink-p link) (file-exists-p link))
          (delete-file link)))
      (make-symbolic-link (elpaso-admin--sling "../packages" name "dist" tarball)
                          archive-path))))

(defun elpaso-admin--call (destination program &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (elpaso-admin--message "call-process %s %s" program args)
  (apply #'call-process program nil destination nil args))

(defun elpaso-admin--call-region (destination program files-from &rest args)
  "Like ‘call-process’ for PROGRAM, DESTINATION, ARGS.
The INFILE and DISPLAY arguments are fixed as nil."
  (with-temp-buffer
    (save-excursion (insert (mapconcat #'identity files-from "\n")))
    (apply #'call-process-region (point-min) (point-max) program nil destination nil args)))

(defconst elpaso-admin--bwrap-args
  '("--unshare-all"
    "--dev" "/dev"
    "--proc" "/proc"
    "--tmpfs" "/tmp"))

(defvar elpaso-admin--sandboxed-ro-binds
  '("/lib" "/lib64" "/bin" "/usr" "/etc/alternatives" "/etc/emacs"))

(defun elpaso-admin--call-sandboxed (destination &rest args)
  "Like ‘elpaso-admin--call’ but sandboxed.
More specifically, uses Bubblewrap such that the command is
confined to only have write access to the `default-directory'.
Signal an error if the command did not finish with exit code 0."
  (if (not elpaso-admin--sandbox)
      (apply #'elpaso-admin--call destination args)
    (elpaso-admin--message "call-sandboxed %s" args)
    (let ((dd (expand-file-name default-directory))) ;No `~' allowed!
      (setq args (nconc `("--bind" ,dd ,dd) args)))
    ;; Add read-only dirs in reverse order.
    (dolist (b elpaso-admin--sandboxed-ro-binds)
      (when (file-exists-p b)         ;`brwap' burps on binds that don't exist!
        (setq b (expand-file-name b))
        (setq args (nconc `("--ro-bind" ,b ,b) args))))
    (let ((exitcode
           (apply #'elpaso-admin--call destination "bwrap"
                  (append elpaso-admin--bwrap-args args))))
      (unless (eq exitcode 0)
        (if (eq destination t)
            (error "Error-indicating exit code in elpaso-admin--call-sandboxed:\n%s"
                   (buffer-string))
          (error "Error-indicating exit code in elpaso-admin--call-sandboxed"))))))

(defun elpaso-admin--override-version (pkg-spec orig-fun header)
  (let ((version-map (plist-get (cdr pkg-spec) :version-map))
	(dont-release (plist-get (cdr pkg-spec) :dont-release))
	(str (funcall orig-fun header)))
    (or (when (or (equal header "version")
		  (and str (equal header "package-version")))
	  (or (cadr (assoc str version-map))
              (and str dont-release
		   (string-match dont-release str)
		   (replace-match "snapshot" t t str))
	      str
	      "0pre"))
	str)))

;; Some packages use version numbers which `version-to-list' doesn't
;; recognize out of the box.  So here we help.

(add-to-list 'version-regexp-alist '("^[-.+ ]*beta-?$" . -2)) ;"1.0.0-beta-3"
(add-to-list 'version-regexp-alist '("^[-.+ ]*dev$" . -4))    ;2.5-dev

(defun elpaso-admin--metadata (dir pkg-spec)
  "Return a list (VERSION DESCRIPTION REQ EXTRAS),
VERSION is the version string of the simple package;
DESCRIPTION is the brief description of the package;
REQ is a list of requirements;
EXTRAS is an alist with additional metadata.

PKG is the name of the package and DIR is the directory where it is."
  (let* ((main-file* (elpaso-admin--main-file pkg-spec dir))
	 (main-file (when main-file* (expand-file-name main-file* dir))))
    (unless (and main-file (file-exists-p main-file))
      (error "Can't find main file %s file in %s" main-file dir))
    (let* ((pkg-file* (elpaso-admin--pkg-file pkg-spec dir))
           (pkg-file (when pkg-file* (expand-file-name pkg-file* dir)))
           (pkg-version (when pkg-file
                          (when-let ((desc
                                      (ignore-errors
					(package-process-define-package
					 (elpaso-admin--form-from-file-contents pkg-file)))))
                            (package-version-join (package-desc-version desc))))))
      (with-temp-buffer
	(insert-file-contents main-file)
	(goto-char (point-min))
	(let* ((advice (apply-partially
			#'elpaso-admin--override-version
			pkg-spec))
	       (pkg-desc
		(unwind-protect
                    (progn
		      (advice-add #'lm-header :around advice)
                      (package-buffer-info))
                  (advice-remove 'lm-header advice)))
               (extras (package-desc-extras pkg-desc))
               (version (if (and pkg-version
				 (version-list-< (package-desc-version pkg-desc)
						 (version-to-list pkg-version)))
                            (version-to-list pkg-version)
                          (package-desc-version pkg-desc)))
               (keywords (lm-keywords-list))
               ;; (_ (elpaso-admin--version-to-list version)) ; Sanity check!
               (found-keywords (alist-get :keywords extras)))
          (when (and keywords (not found-keywords))
            ;; Using an old package-buffer-info which doesn't include
            ;; keywords.  Fix it by hand.
            (push (cons :keywords keywords) extras))
          (list (package-version-join version)
		(package-desc-summary pkg-desc)
		(package-desc-reqs pkg-desc)
		extras))))))

(defun elpaso-admin--alist-to-plist-args (alist)
  (mapcar (lambda (x)
            (if (and (not (consp x))
                     (or (keywordp x)
                         (not (symbolp x))
                         (memq x '(nil t))))
                x `',x))
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(defun elpaso-admin--plist-args-to-alist (plist)
  (let (alist)
    (while plist
      (let ((value (cadr plist)))
        (when value
          (cl-assert (keywordp (car plist)))
          (push (cons (car plist)
                      (if (eq 'quote (car-safe value)) (cadr value) value))
                alist)))
      (setq plist (cddr plist)))
    alist))

(defun elpaso-admin--process-multi-file-package (dir pkg &optional dont-rename)
  "Deploy the contents of DIR into the archive as a multi-file package.
Rename DIR/ to PKG-VERS/, and return the descriptor."
  (let* ((exp (elpaso-admin--multi-file-package-def dir pkg))
	 (vers (nth 2 exp))
         (req-exp (nth 4 exp))
	 (req (mapcar #'elpaso-admin--convert-require
                      (if (eq 'quote (car-safe req-exp)) (nth 1 req-exp)
                        (when req-exp
                          (error "REQ should be a quoted constant: %s"
                                 req-exp)))))
         (extras (elpaso-admin--plist-args-to-alist (nthcdr 5 exp))))
    (unless (equal (nth 1 exp) pkg)
      (error "Package name %s doesn't match file name %s"
	     (nth 1 exp) pkg))
    (unless dont-rename (rename-file dir (concat pkg "-" vers)))
    (cons (intern pkg) (vector (elpaso-admin--version-to-list vers)
                               req (nth 3 exp) 'tar extras))))

(defun elpaso-admin--multi-file-package-def (dir pkg)
  "Return the `define-package' form in the file DIR/PKG-pkg.el."
  (let ((pkg-file (expand-file-name (concat pkg "-pkg.el") dir)))
    (unless (file-exists-p pkg-file)
      (error "File not found: %s" pkg-file))
    (elpaso-admin--form-from-file-contents pkg-file)))

(defun elpaso-admin--write-pkg-file (pkg-dir name metadata)
  ;; FIXME: Use package-generate-description-file!
  (let ((pkg-file (expand-file-name (concat name "-pkg.el") pkg-dir))
	(print-level nil)
        (print-quoted t)
	(print-length nil))
    (elpaso-admin--temp-file pkg-file)
    (write-region
     (concat (format ";; Generated package description from %s.el  -*- no-byte-compile: t -*-\n"
		     name)
	     (prin1-to-string
              (cl-destructuring-bind (version desc requires extras)
                  metadata
                (nconc
                 (list 'define-package
                       name
                       version
                       desc
                       (list 'quote
                             ;; Turn version lists into string form.
                             (mapcar
                              (lambda (elt)
                                (list (car elt)
                                      (package-version-join (cadr elt))))
                              requires)))
                 (elpaso-admin--alist-to-plist-args extras))))
	     "\n")
     nil
     pkg-file)))

(defun elpaso-admin--pull (dirname)
  (let* ((default-directory (elpaso-admin--dirname dirname))
	 (pkg (file-name-nondirectory dirname))
         (pkg-spec (elpaso-admin--get-package-spec pkg)))
    ;; Undo any local changes to `<pkg>-pkg.el', in case it's under
    ;; version control.
    (elpaso-admin--call nil "git" "checkout" "--" (concat pkg "-pkg.el"))
    (with-temp-buffer
      (cond
       ((file-directory-p ".git")
        (elpaso-admin--call t "git" "pull"))
       ((file-exists-p ".git") ;; A worktree, presumably.
        (let ((remote-ref (elpaso-admin--ref-master pkg-spec)))
	  (if (elpaso-admin--ref-p remote-ref)
              (unless (with-temp-buffer
                        (elpaso-admin--call t "git" "status" "--branch" "--porcelain=2")
                        (string-match "\n# branch.upstream" (buffer-string)))
                (unless (zerop (elpaso-admin--call nil "git" "branch"
                                                   "--set-upstream-to"
					           remote-ref))
                  (error "elpaso-admin--pull: %s" (buffer-string))))
	    (error "No remote ref %s" remote-ref)))
	(elpaso-admin--call t "git" "merge"))
       (t (error "No .git in %s" default-directory))))))

(defun elpaso-admin--worktree-sync (pkg-spec pkg-dir)
  "Sync worktree of PKG-SPEC in PKG-DIR."
  (elpaso-admin--call nil "git" "worktree" "remove" "-f" pkg-dir)
  (with-temp-buffer
    (unless (zerop (elpaso-admin--call t "git" "worktree" "add" "--detach"
				       pkg-dir (elpaso-admin--ref-master pkg-spec)))
      (error "elpaso-admin--worktree-sync: %s" (buffer-string)))))

(defun elpaso-admin--build-Info (pkg-spec dir)
  (let ((docfile (elpaso-admin--spec-get pkg-spec :doc)))
    (dolist (f (if (listp docfile) docfile (list docfile)))
      (elpaso-admin--build-Info-1 f dir))))

(defun elpaso-admin--build-Info-1 (docfile dir)
  (let* ((elpaso-admin--sandboxed-ro-binds
          (cons default-directory elpaso-admin--sandboxed-ro-binds))
         (default-directory (elpaso-admin--dirname dir))
         (tmpfiles '()))
    (when (and docfile (file-readable-p docfile)
               (string-match "\\.org\\'" docfile))
      (with-temp-buffer
        (elpaso-admin--call-sandboxed
         t "emacs" "--batch" "-l" "ox-texinfo"
         ;; When building :core packages, don't follow the symlink,
         ;; otherwise Org will want to export into the Emacs tree!
         "--eval" "(setq vc-follow-symlinks nil)"
         docfile
         "--eval" "(message \"ELPATEXI=%s\" (org-texinfo-export-to-texinfo))")
        (message "%s" (buffer-string))
        (goto-char (point-max))
        (when (re-search-backward "ELPATEXI=\\(.*\\)\n?" nil t)
          (setq docfile (concat (file-name-directory docfile)
                                (match-string 1)))
          (push docfile tmpfiles)
          (elpaso-admin--temp-file docfile))))

    (when (and docfile (file-readable-p docfile)
               (string-match "\\.texi\\(nfo\\)?\\'" docfile))
      (let ((info-file (concat
                        (file-name-sans-extension
                         (file-name-nondirectory docfile))
                        ".info")))
        (elpaso-admin--temp-file info-file)
        (with-temp-buffer
          (elpaso-admin--call-sandboxed
           t "makeinfo" "--no-split" docfile "-o" info-file)
          (message "%s" (buffer-string)))
        (setq docfile info-file)))

    (when (and docfile (not (string-match "\\.info\\'" docfile)))
      (error "Not a supported doc format: %s" docfile))

    (when (and docfile (file-readable-p docfile)
               (file-name-directory docfile))
      ;; The built-in support for Info files in package.el only
      ;; works for Info file that are in the top-level directory.
      ;; FIXME: We could just not use it, but then we'd need to do
      ;; something like add a dummy .el file at toplevel with
      ;; an ;;;###autoload cookie which adds the right directory to
      ;; Info-directory-list.  This would have the advantage that
      ;;   emacs -l .../<pkg>-autoloads.el
      ;; would properly setup the Info reader, tho!
      (let ((info-file (file-name-nondirectory docfile)))
        (elpaso-admin--temp-file info-file)
        (copy-file docfile info-file)
        (setq docfile info-file)))

    (mapc #'delete-file tmpfiles)     ;Avoid intermediate files in the tarball.

    (when (and docfile (file-readable-p docfile))
      (let ((dir-file (expand-file-name "dir")))
        (elpaso-admin--temp-file dir-file)
        (with-temp-buffer
          (elpaso-admin--call-sandboxed
           t "install-info" (concat "--dir=" dir-file) docfile)
          (message "%s" (buffer-string)))))))

(defun elpaso-admin--make (pkg-spec dir)
  (let ((target (elpaso-admin--spec-get pkg-spec :make))
        (cmd (elpaso-admin--spec-get pkg-spec :shell-command)))
    (when (or cmd target)
      (with-temp-buffer
        (let ((elpaso-admin--sandboxed-ro-binds
               (cons default-directory elpaso-admin--sandboxed-ro-binds))
              (default-directory (elpaso-admin--dirname dir)))
          (when cmd
            (elpaso-admin--call-sandboxed t shell-file-name
                                   shell-command-switch
                                   cmd))
          (when target
            (apply #'elpaso-admin--call-sandboxed t "make"
                   (if (consp target) target (list target))))
          (elpaso-admin--message "%s" (buffer-string)))))))

(defun elpaso-admin--branch-p (possible)
  (zerop (elpaso-admin--call nil "git" "rev-parse" "--verify" possible)))

(defun elpaso-admin--ref-p (possible)
  (zerop (elpaso-admin--call nil "git" "show-ref" "--verify" possible)))

(defun elpaso-admin--cobble-url (pkg-spec)
  (cl-flet ((get (what) (elpaso-admin--spec-get pkg-spec what)))
    (let ((external (get :external))
          (url (get :url))
          (fetcher (get :fetcher))
          (repo (get :repo)))
      (cond (external external)
            (url url)
            ((and fetcher repo)
	     (let ((base (format "%s.com/%s.git" fetcher repo)))
	       (if (eq fetcher 'mockhub)
		   base
		 (format "https://%s" base))))
            (t nil)))))

(defun elpaso-admin--fetch-one-package (pkg-spec)
  (when-let ((url (elpaso-admin--cobble-url pkg-spec))
             (refspec (elpaso-admin--refspec pkg-spec)))
    (unwind-protect
        (with-temp-buffer
          (if (zerop (apply #'elpaso-admin--call t
			    (split-string (format "git fetch --no-tags --depth 1 %s %s"
						  url refspec))))
              (cl-destructuring-bind (from to)
                  (split-string refspec ":")
                (message "%s[%s] -> %s" url from to))
            (error "elpaso-admin--fetch-one-package: %s" (buffer-string))))
      (apply #'elpaso-admin--call nil (split-string "git gc --prune=all")))))

(defun elpaso-admin-batch-fetch ()
  (let ((specs (elpaso-admin--get-specs))
        (pkgs command-line-args-left))
    (setq command-line-args-left nil)
    (dolist (pkg pkgs)
      (if-let ((pkg-spec (assoc pkg specs)))
          (elpaso-admin--fetch-one-package pkg-spec)
        (error "elpaso-admin-batch-fetch: no recipe for %s" pkg)))))

(defun elpaso-admin-ert-package-install (top-directory package)
  ;; blitz default value and set up from elpa.
  (setq package-archives
        `(("local-elpa"
	   . ,(expand-file-name "packages" top-directory)))
	package-user-dir (make-temp-file "elpa-test" t))
  (package-initialize 'no-activate)
  (package-refresh-contents)
  (package-install package))

(defun elpaso-admin-ert-test-find-tests (package-directory package)
  (append
   `(,(expand-file-name
       (concat (symbol-name package) "-autoloads.el") package-directory))
   (or
    (directory-files package-directory t ".*-test.el$")
    (directory-files package-directory t ".*-tests.el$")
    (let ((dir-test (expand-file-name "test" package-directory)))
      (when (file-directory-p dir-test)
	(directory-files dir-test t elpaso-admin--another-re-no-dot)))
    (let ((dir-tests (expand-file-name "tests" package-directory)))
      (when (file-directory-p dir-tests)
	(directory-files dir-tests t elpaso-admin--another-re-no-dot))))))

(defun elpaso-admin-ert-load-tests (package-directory package)
  (mapc
   (lambda (file)
     (let ((force-load-messages t))
       (load-file file)))
   (elpaso-admin-ert-test-find-tests package-directory package)))

(defun elpaso-admin-ert-test-package (top-directory package)
  (elpaso-admin-ert-package-install top-directory package)
  (elpaso-admin-ert-load-tests
   (expand-file-name (concat "packages/" (symbol-name package)) top-directory)
   package)

  (ert-run-tests-batch-and-exit t))

(provide 'elpaso-admin)
;;; elpaso-admin.el ends here