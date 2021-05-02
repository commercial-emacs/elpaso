;;; elpaso-disc.el --- Discover new packages  -*- lexical-binding:t -*-

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

;;

;;; Code:

(require 'tabulated-list)
(require 'request)
(require 'elpaso-admin)
(require 'elpaso-defs)
(require 'ghub)

(defconst elpaso-disc-github-client-id "1f006d815c4bb23dfe96")
(defconst elpaso-disc-search-buffer "*elpaso search*")
(defvar elpaso-disc--access-token nil)
(defvar elpaso-disc--results nil)
(defvar elpaso-disc--readmes nil)

(cl-defstruct (elpaso-disc-time (:type list))
  "Overwrought but convenient to have named fields instead of anonymous 9-tuple."
  (second)
  (minute)
  (hour)
  (day)
  (month)
  (year)
  (weekday)
  (dst)
  (zone))

(defvar elpaso-disc-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'elpaso-disc-readme-button)
    (define-key map "h" 'elpaso-disc-quick-help)
    map)
  "Local keymap for `elpaso-disc-mode' buffers.")

(define-derived-mode elpaso-disc-mode tabulated-list-mode "Elpaso Search"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<elpaso-disc-mode-map>
\\{elpaso-disc-mode-map}"
  :group 'elpaso
  (setq buffer-file-coding-system 'utf-8)
  (setq tabulated-list-format
        `[("Repo" 35 elpaso-disc--name-predicate)
          ("Upd" 6 elpaso-disc--pushed-predicate :right-align t)
          ("Stars" 6 elpaso-disc--stars-predicate :right-align t :pad-right 2)
          ("Description" 0 elpaso-disc--description-predicate)])
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "Stars" t))
  (add-hook 'tabulated-list-revert-hook #'elpaso-disc--refresh nil t)
  (tabulated-list-init-header))

(defun elpaso-disc-readme-button (&optional button)
  (elpaso-disc--readme
   (if button
       (button-label button)
     (alist-get 'nameWithOwner (tabulated-list-get-id)))))

(defun elpaso-disc--readme (name)
  (let* ((text (alist-get name elpaso-disc--readmes nil nil #'equal))
	 (decoded (with-temp-buffer
		    (insert text)
		    (decode-coding-region (point-min) (point-max) 'utf-8 t))))
    (help-setup-xref (list #'elpaso-disc--readme name) nil)
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
	(insert decoded)))))

(defun elpaso-disc--print-info-simple (node)
  "Return a package entry suitable for `tabulated-list-entries'.
Return (NODE [REPO PUSHED STARS DESCRIPTION])."
  (list node
        `[(,(alist-get 'nameWithOwner node)
           face elpaso-face-name
           font-lock-face elpaso-face-name
           follow-link t
           action elpaso-disc-readme-button)
          ,(propertize (elpaso-disc-format-time-elapsed
			(alist-get 'pushedAt node))
		       'font-lock-face 'elpaso-face-description)
          ,(propertize (number-to-string (alist-get 'stargazers node))
		       'font-lock-face 'elpaso-face-description)
          ,(propertize (or (alist-get 'description node) "")
		       'font-lock-face 'elpaso-face-description)]))

(defun elpaso-disc--refresh ()
  "Re-populate the `tabulated-list-entries'.  Construct list of (PKG-DESC . STATUS)."
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar #'elpaso-disc--print-info-simple elpaso-disc--results)))

(defun elpaso-disc-squirrel (access-token)
  "Persist ACCESS-TOKEN to a sensible location."
  (let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
         (token-dir (concat (file-name-as-directory data-home) "elpaso")))
    (unless (file-directory-p token-dir)
      (make-directory token-dir t))
    (unless (string= "700" (format "%o" (file-modes token-dir)))
      (set-file-modes token-dir #o700))
    (with-temp-file (concat (file-name-as-directory token-dir) "access_token")
      (insert access-token "\n"))))

(cl-defun elpaso-disc-set-access-token (&key (force nil) (acquire-p t))
  "FORCE if non-nil reqacuires auth (no matter what).
ACQUIRE-P if nil refuses reacquisition (no matter what)."
  (interactive)
  (let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
         (token-dir (concat (file-name-as-directory data-home) "elpaso")))
    (setq elpaso-disc--access-token
	  (ignore-errors
	    (elpaso-admin--form-from-file-contents
	     (concat (file-name-as-directory token-dir) "access_token"))))
    (if (or force (and acquire-p (not elpaso-disc--access-token)))
	(progn
	  (setq elpaso-disc--access-token nil)
	  (elpaso-disc--new-access-token)
	  (elpaso-disc-set-access-token :force nil :acquire-p nil))
      (unless elpaso-disc--access-token
	(error "elpaso-disc-set-access-token: authentication failure")))))

(defconst elpaso-disc-github-url-login-token
  "https://github.com/login/oauth/access_token")

(defconst elpaso-disc-github-url-login-code
  "https://github.com/login/device/code")

(defun elpaso-disc--new-access-token ()
  "Do the dance."
  (let* (result
	 (client-id elpaso-disc-github-client-id)
	 (headers (list (cons "Accept" "application/vnd.github.v3+json")))
	 (with-grant-yield-token
	  (cl-function
           (lambda (&key data &allow-other-keys)
	     (let-alist data
	       (setq result (bound-and-true-p .access_token))))))
	 (with-user-request-grant
	  (lambda (device-code)
	    (request elpaso-disc-github-url-login-token
	      :sync t
	      :type "POST"
	      :headers headers
	      :params `((client_id . ,client-id)
			(device_code . ,device-code)
			(grant_type . "urn:ietf:params:oauth:grant-type:device_code"))
	      :parser 'json-read
	      :success with-grant-yield-token)))
	 (with-device-prompt-user
	  (cl-function
           (lambda (&key data &allow-other-keys)
	     (let-alist data
	       (when (y-or-n-p (format "Open browser to %s? " .verification_uri))
		 (browse-url .verification_uri)
		 (if (and (y-or-n-p (format "Enter user code %s? " .user_code))
			  (y-or-n-p (format
				     "[%s] Did you see \"Congratulations, you're all set\"? "
				     .user_code)))
		     (funcall with-user-request-grant .device_code)
		   (message "bummer"))))))))
    (request elpaso-disc-github-url-login-code
      :sync t
      :type "POST"
      :headers headers
      :params `((client_id . ,client-id) (scope . ""))
      :parser 'json-read
      :success with-device-prompt-user)
    (when result
      (elpaso-disc-squirrel result))))

(defmacro elpaso-disc--query-query (query &optional variables callback errorback)
  (declare (indent defun))
  `(let* ((host (ghub--host 'github)))
     (ghub--graphql-retrieve
      (ghub--make-graphql-req
       :url       (url-generic-parse-url
		   (format "https://%s/graphql"
			   (if (string-suffix-p "/v3" host)
			       (substring host 0 -3)
			     host)))
       :method    "POST"
       :headers   (list (cons "Accept" "application/vnd.github.v3+json")
			(cons "Authorization" (format "bearer %s" elpaso-disc--access-token)))
       :handler   'ghub--graphql-handle-response
       :query     ,query
       :variables ,variables
       :buffer    (current-buffer)
       ,@(when callback
	   (list :callback
		 `(lambda (data)
		    (ghub--graphql-set-mode-line ,(current-buffer) nil)
		    (funcall ,callback data))))
       :errorback ,errorback))))

(cl-defun elpaso-disc-query-results (&rest words
                                     &key (first 10) (callback #'ignore)
                                     &allow-other-keys)
  (cl-loop for i = 0 then (1+ i)
           until (>= i (length words))
           for word = (nth i words)
           if (keywordp word)
           do (cl-incf i)
           else
           collect word into result
           end
           finally do (setq words result))
  (elpaso-disc--query-query
    `(query
      (search
       [(query ,(format "%s is:public language:\"Emacs Lisp\""
			(mapconcat #'identity words " ")))
	(type REPOSITORY)
	(first $first Int!)]
       (nodes
	(...\ on\ Repository
	 id
	 nameWithOwner
	 url
	 pushedAt
	 description
	 (stargazers totalCount)
	 (defaultBranchRef name)))))
    `((first . ,first))
    (lambda (data)
      (pcase-let ((`(data (search (nodes . ,nodes))) data))
	(mapc (lambda (node)
		(dolist (pair node)
		  (when (listp (cdr pair))
		    (setcdr pair (cdr (cl-second pair))))))
	      nodes)
	(setq elpaso-disc--results nodes))
      (elpaso-disc-query-readmes callback))))

;; (gsexp-encode (ghub--graphql-prepare-query (elpaso-disc--query-readme "MDEwOlJlcG9zaXRvcnkyMzIxNjY0MQ==" "master")))

(defun elpaso-disc-query-readmes (&optional callback)
  (unless callback (setq callback #'ignore))
  (cl-flet ((dodge (s) (intern s)))
    (dolist (node elpaso-disc--results)
      (let-alist node
        (unless (assoc .nameWithOwner elpaso-disc--readmes)
	  (elpaso-disc--query-query
	   `(query
	     (node [(id ,.id)]
		   (,(dodge  "... on Repository")
		    (md:\ object [(expression ,(concat .defaultBranchRef ":README.md"))]
				 (,(dodge "... on Blob") text))
		    (rst:\ object [(expression ,(concat .defaultBranchRef ":README.rst"))]
				  (,(dodge "... on Blob") text))
		    (org:\ object [(expression ,(concat .defaultBranchRef ":README.org"))]
				  (,(dodge "... on Blob") text))
		    (texi:\ object [(expression ,(concat .defaultBranchRef ":README.texi"))]
				   (,(dodge "... on Blob") text)))))
	   nil
	   (lambda (data)
	     (pcase-let ((`(data (node (md (text  . ,md))
				       (rst (text . ,rst))
				       (org (text . ,org))
				       (texi (text . ,texi))))
			  data))
	       (when-let ((text (or md rst org texi)))
		 (setf (alist-get .nameWithOwner elpaso-disc--readmes
				  nil nil #'equal)
		       (cl-subseq text 0 (min 10000 (length text))))))
             (funcall callback))))))))

(defun elpaso-disc-backport-iso8601 (string)
  "The module iso8601 is only emacs-27; copy the logic here.
Convert STRING into a 'time structure'."
  (let* ((concat-regexps
          (lambda (regexps)
            (mapconcat (lambda (regexp)
                         (concat "\\(?:"
                                 (replace-regexp-in-string "(" "(?:" regexp)
                                 "\\)"))
		       regexps "\\|")))
         (date-match "\\([+-]?[0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)")
         (time-match "\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?:?\\([0-9][0-9]\\)?[.,]?\\([0-9]*\\)")
         (zone-match "\\(Z\\|\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)?\\)")
         (regexp (concat "\\(" (funcall concat-regexps (list date-match)) "\\)"
                         "\\(?:T\\("
                         (replace-regexp-in-string "(" "(?:" time-match)
                         "\\)"
                         "\\(" zone-match "\\)?\\)?")))
    (if (not (string-match (concat "\\`" regexp "\\'") string))
        (signal 'wrong-type-argument string)
      (let ((date-string (match-string 1 string))
            (time-string (match-string 2 string))
            (result (make-elpaso-disc-time)))
        (string-match (concat "\\`" date-match "\\'") date-string)
        (let ((day (string-to-number (match-string 3 date-string)))
	      (month (string-to-number (match-string 2 date-string)))
	      (year (string-to-number (match-string 1 date-string))))
          (setf (elpaso-disc-time-year result) year)
          (setf (elpaso-disc-time-month result) month)
          (setf (elpaso-disc-time-day result) day))
        (string-match (concat "\\`" time-match "\\'") time-string)
        (let ((hour (string-to-number (match-string 1 time-string)))
	      (minute (string-to-number (match-string 2 time-string)))
	      (second (string-to-number (match-string 3 time-string))))
          (setf (elpaso-disc-time-hour result) hour)
          (setf (elpaso-disc-time-minute result) minute)
          (setf (elpaso-disc-time-second result) second))
        (setf (elpaso-disc-time-zone result) 0)
        result))))

(defun elpaso-disc-format-time-elapsed (date)
  "Return time elapsed since TIME.
Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (cl-flet ((dense-time
	     (time)
	     (+ (* (car time) 65536.0)
		(cadr time)
		(/ (or (car (cdr (cdr time))) 0) 1000000.0))))
    (let* ((then (dense-time (apply #'encode-time (elpaso-disc-backport-iso8601 date))))
	   (now (dense-time (current-time)))
	   (diff (- now then))
	   (str
            (cond
             ((>= diff (* 86400.0 7.0 52.0))
	      (if (>= diff (* 86400.0 7.0 52.0 10.0))
		  (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
		(format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
             ((>= diff (* 86400.0 30.0))
	      (if (>= diff (* 86400.0 30.0 10.0))
		  (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
		(format "%3.1fM" (/ diff (* 86400.0 30.0)))))
             ((>= diff (* 86400.0 7.0))
	      (if (>= diff (* 86400.0 7.0 10.0))
		  (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
		(format "%3.1fw" (/ diff (* 86400.0 7.0)))))
             ((>= diff 86400.0)
	      (if (>= diff (* 86400.0 10.0))
		  (format "%3dd" (floor (/ diff 86400.0)))
		(format "%3.1fd" (/ diff 86400.0))))
             ((>= diff 3600.0)
	      (if (>= diff (* 3600.0 10.0))
		  (format "%3dh" (floor (/ diff 3600.0)))
		(format "%3.1fh" (/ diff 3600.0))))
             ((>= diff 60.0)
	      (if (>= diff (* 60.0 10.0))
		  (format "%3dm" (floor (/ diff 60.0)))
		(format "%3.1fm" (/ diff 60.0))))
             (t
	      (format "%3ds" (floor diff)))))
	   (stripped
            (replace-regexp-in-string "\\.0" "" str)))
      (concat (cond
	       ((= 2 (length stripped)) "  ")
	       ((= 3 (length stripped)) " ")
	       (t ""))
	      stripped))))

(defun elpaso-disc--buffer ()
  (with-current-buffer (get-buffer-create elpaso-disc-search-buffer)
    (unless (eq major-mode 'elpaso-disc-mode)
      (elpaso-disc-mode))
    (current-buffer)))

(defun elpaso-disc--present ()
  (with-current-buffer (elpaso-disc--buffer)
    (run-hooks 'tabulated-list-revert-hook)
    (tabulated-list-print 'remember nil))
  (switch-to-buffer (elpaso-disc--buffer)))

(defun elpaso-disc--name-predicate (A B)
  (string< (alist-get 'nameWithOwner (car A))
	   (alist-get 'nameWithOwner (car B))))

(defun elpaso-disc--pushed-predicate (A B)
  (string< (alist-get 'pushedAt (car A)) (alist-get 'pushedAt (car B))))

(defun elpaso-disc--stars-predicate (A B)
  (< (alist-get 'stargazers (car A)) (alist-get 'stargazers (car B))))

(defun elpaso-disc--description-predicate (A B)
  (string< (alist-get 'description (car A))
	   (alist-get 'description (car B))))

(defun elpaso-disc-search (search-for &optional first)
  (elpaso-disc-set-access-token)
  (apply #'elpaso-disc-query-results
         (append (when first
                   (list :first first))
                 (list :callback #'elpaso-disc--present)
		 (split-string search-for))))

(provide 'elpaso-disc)
;;; elpaso-disc.el ends here
