;;; elpaso-disc.el --- Discover new packages  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2021 The Authors

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: maint tools
;; URL: https://github.com/dickmao/elpaso
;; Package-Requires: ((emacs "26.1") (elpaso "0.1.0") (ghub) (request "0.3.3") (web-server "0.1.2"))

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

;; Not bootstrappable component of elpaso.
;;
;; ::
;;
;;     M-x elpaso
;;     M-x elpaso-search
;;

;;; Code:

(require 'tabulated-list)
(require 'elpaso)
(require 'request)
(require 'ghub)
(require 'web-server)

(defconst elpaso-disc--readme-filenames
  (split-string "readme.md readme.rst readme.org readme readme.txt"))

(defvar elpaso-disc-search-history nil
  "History of user entered keywords.")

(defconst elpaso-disc-host-info
  '((github . (:url "https://github.com/login/oauth/authorize" :client-id "1f006d815c4bb23dfe96"))
    (gitlab . (:url "https://gitlab.com/oauth/authorize" :client-id "536c6701378df511c12ec10438804b1bce4af0404bc7b3b22a517727553ec0c4"))))

(defcustom elpaso-disc-number-results 10
  "How many results to return."
  :group 'elpaso
  :type 'integer)

(defcustom elpaso-disc-hosts
  (mapcar #'car elpaso-disc-host-info)
  "Search venues."
  :group 'elpaso
  :set (lambda (symbol value)
	 (when-let ((culprit
		     (cl-find-if
		      (lambda (x)
			(not (memq x (mapcar #'car elpaso-disc-host-info))))
		      value)))
	   (error "elpaso-disc-hosts: not setting %s for %s" symbol culprit))
         (set-default symbol value))
  :type '(repeat symbol))

(defconst elpaso-disc-endpoint "https://te2kbowzhi.execute-api.us-east-2.amazonaws.com/dev/route_access_token")

(defconst elpaso-disc-redirect-uri "http://127.0.0.1:17973")

(defconst elpaso-disc-search-buffer "*elpaso search*")
(defvar elpaso-disc--access-token (mapcar (lambda (x) (cons (car x) nil)) elpaso-disc-host-info))
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
    (define-key map "\C-m" 'elpaso-disc-drill-button)
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
        `[("Repository" 30 elpaso-disc--name-predicate)
          ("Upd" 6 elpaso-disc--pushed-predicate :right-align t)
          ("Stars" 6 elpaso-disc--stars-predicate :right-align t :pad-right 2)
          ("Description" 0 elpaso-disc--description-predicate)])
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "Stars" t))
  (add-hook 'tabulated-list-revert-hook #'elpaso-disc--refresh nil t)
  (tabulated-list-init-header))

(defun elpaso-disc--install-button-action (button)
  (let ((name (button-get button 'name))
        (url (button-get button 'url)))
    (when (y-or-n-p (format-message "Install package `%s'? " name))
      (unless (elpaso-admin-lookup-package-spec url)
        (elpaso-refresh)
        (unless (elpaso-admin-lookup-package-spec url)
          (elpaso-admin-tack-spec (cons (symbol-name name) `(:url ,url :prospective t)))))
      (elpaso-install name)
      (revert-buffer nil t)
      (goto-char (point-min)))))

(defun elpaso-disc--browse-button-action (button)
  (let ((url (button-get button 'url)))
    (browse-url url)))

(defun elpaso-disc--delete-button-action (button)
  (let ((name (button-get button 'name)))
    (if-let ((pkg-desc (car (alist-get name package-alist))))
        (when (y-or-n-p (format-message "Delete package `%s'? " name))
          (elpaso-delete pkg-desc)
          (revert-buffer nil t)
          (goto-char (point-min)))
      (message "elpaso-disc--delete-button-action: no pkg-desc for %s!" name))))

(defun elpaso-disc-drill-button (&optional button)
  (elpaso-disc--drill (if button
                          (button-get button 'node)
                        (tabulated-list-get-id))))

(defun elpaso-disc--drill (node)
  (help-setup-xref (list #'elpaso-disc--drill node) nil)
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let* ((parsed-url (url-generic-parse-url (alist-get 'url node)))
	     (host (url-host parsed-url))
             (name-with-owner (alist-get 'nameWithOwner node))
             (ez-name (intern (file-name-sans-extension
                               (file-name-nondirectory name-with-owner))))
             (name (if-let ((spec (elpaso-admin-lookup-package-spec
                                   (alist-get 'url node))))
                       (intern (car spec))
                     ez-name))
             (desc (car (alist-get name package-alist)))
	     (pkg-dir (when desc (package-desc-dir desc)))
	     (status (if desc (package-desc-status desc) "available"))
             (extras (when desc (package-desc-extras desc))))
        (package--print-help-section "Package" (symbol-name name))
	(package--print-help-section "Status")
	(if pkg-dir
	    (progn
              (insert (propertize (if (member status '("unsigned" "dependency"))
                                      "Installed"
                                    (capitalize status))
				  'font-lock-face 'package-status-built-in))
              (insert (substitute-command-keys " in `"))
              (let ((dir (abbreviate-file-name
			  (file-name-as-directory
                           (if (file-in-directory-p pkg-dir package-user-dir)
                               (file-relative-name pkg-dir package-user-dir)
                             pkg-dir)))))
		(help-insert-xref-button dir 'help-package-def pkg-dir))
              (insert (substitute-command-keys "'"))
              (when (package-desc-p desc)
		(insert "  ")
                (package-make-button
	         "Browse"
	         'action 'elpaso-disc--browse-button-action
                 'url (alist-get 'url node))
		(insert "  ")
		(package-make-button
		 "Delete"
                 'action 'elpaso-disc--delete-button-action
                 'name name)
		(insert "  ")
		(package-make-button
		 "Reinstall"
                 'action 'elpaso-disc--install-button-action
                 'name name
		 'url (alist-get 'url node))))
	  (insert (capitalize status))
	  (insert " from " host "  ")
          (package-make-button
	   "Browse"
	   'action 'elpaso-disc--browse-button-action
           'url (alist-get 'url node))
	  (insert "  ")
	  (package-make-button
	   "Install"
	   'action 'elpaso-disc--install-button-action
	   'name name
           'url (alist-get 'url node)))
        (insert "\n")
        (when-let ((version (when desc (package-desc-version desc))))
          (package--print-help-section "Version"
            (package-version-join version)))
        (when-let ((commit (cdr (assoc :commit extras))))
          (package--print-help-section "Commit" commit))
        (when desc
          (package--print-help-section "Summary"
            (package-desc-summary desc)))
        (when-let ((reqs (when desc (package-desc-reqs desc))))
          (package--print-help-section "Requires")
          (let ((first t))
            (dolist (req reqs)
              (let* ((name (car req))
                     (vers (cadr req))
                     (text (format "%s-%s" (symbol-name name)
                                   (package-version-join vers)))
                     (reason ""))
                (cond (first (setq first nil))
                      ((>= (+ 2 (current-column) (length text) (length reason))
                           (window-width))
                       (insert ",\n               "))
                      (t (insert ", ")))
                (help-insert-xref-button text 'help-package name)
                (insert reason)))
            (insert "\n")))
        (when-let ((required-by (when desc (package--used-elsewhere-p desc nil 'all))))
          (package--print-help-section "Required by")
          (let ((first t))
            (dolist (pkg required-by)
              (let ((text (package-desc-full-name pkg)))
                (cond (first (setq first nil))
                      ((>= (+ 2 (current-column) (length text))
                           (window-width))
                       (insert ",\n               "))
                      (t (insert ", ")))
                (help-insert-xref-button text 'help-package
                                         (package-desc-name pkg))))
            (insert "\n")))
        (when-let ((homepage (alist-get :url extras)))
          ;; Prefer https for the homepage of packages on gnu.org.
          (if (string-match-p "^http://\\(elpa\\|www\\)\\.gnu\\.org/" homepage)
              (let ((gnu (assoc-default "gnu" package-archives)))
                (and gnu (string-match-p "^https" gnu)
                     (setq homepage
                           (replace-regexp-in-string "^http" "https" homepage)))))
          (package--print-help-section "Homepage")
          (help-insert-xref-button homepage 'help-url homepage)
          (insert "\n"))
        (when-let ((keywords (when desc (package-desc--keywords desc))))
          (package--print-help-section "Keywords")
          (dolist (k keywords)
            (insert k " "))
          (insert "\n"))
        (when-let ((maintainer (alist-get :maintainer extras)))
          (package--print-help-section "Maintainer")
          (insert (car maintainer) "\n"))
        (when-let ((authors (alist-get :authors extras)))
          (package--print-help-section
              (if (= (length authors) 1)
                  "Author"
                "Authors"))
          (insert (car (pop authors)) "\n")
          ;; If there's more than one author, indent the rest correctly.
          (dolist (name authors)
            (insert (make-string 13 ?\s) (car name) "\n")))
        (insert "\n")
        (if-let* ((text (assoc-default name-with-owner elpaso-disc--readmes))
	          (decoded (with-temp-buffer
		             (insert text)
		             (decode-coding-region (point-min) (point-max) 'utf-8 t))))
            (save-excursion (insert decoded))
          (message "elpaso-disc--drill: no README for %s" name))))))

(defun elpaso-disc--print-info-simple (node)
  "Return a package entry suitable for `tabulated-list-entries'.
Return (NODE [REPO PUSHED STARS DESCRIPTION])."
  (list node
        `[(,(alist-get 'nameWithOwner node)
           face package-name
           font-lock-face package-name
           follow-link t
           node ,node
           action elpaso-disc-drill-button)
          ,(propertize (elpaso-disc-format-time-elapsed
			(alist-get 'pushedAt node))
		       'font-lock-face 'package-description)
          ,(propertize (number-to-string (alist-get 'stargazers node))
		       'font-lock-face 'package-description)
          ,(propertize (or (alist-get 'description node) "")
		       'font-lock-face 'package-description)]))

(defun elpaso-disc--refresh ()
  "Re-populate the `tabulated-list-entries'.  Construct list of (PKG-DESC . STATUS)."
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar #'elpaso-disc--print-info-simple elpaso-disc--results)))

(defmacro elpaso-disc-let-token-dir (host token-dir &rest body)
  (declare (indent defun))
  `(let* ((data-home (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
          (,token-dir (elpaso-admin--sling data-home "elpaso" (symbol-name ,host))))
     ,@body))

(defun elpaso-disc-squirrel (host access-token)
  "Persist ACCESS-TOKEN to a sensible location."
  (elpaso-disc-let-token-dir host token-dir
    (unless (file-directory-p token-dir)
      (make-directory token-dir t))
    (unless (string= "700" (format "%o" (file-modes token-dir)))
      (set-file-modes token-dir #o700))
    (with-temp-file (concat (file-name-as-directory token-dir) "access_token")
      (insert access-token "\n"))))

(cl-defun elpaso-disc-set-access-token ()
  (interactive)
  (dolist (host elpaso-disc-hosts)
    (unless (alist-get host elpaso-disc--access-token)
      (elpaso-disc-let-token-dir host token-dir
	(elpaso-disc-new-access-token host)
	(setf (alist-get host elpaso-disc--access-token)
	      (ignore-errors (elpaso-admin-form-from-file-contents
			      (concat (file-name-as-directory token-dir)
				      "access_token"))))))))

(defun elpaso-disc-new-access-token (host &optional anew)
  "Do the dance."
  (interactive (list (intern (completing-read
			      "Host: "
			      (mapcar #'symbol-name elpaso-disc-hosts)
			      nil t nil))
		     t))
  (elpaso-disc-let-token-dir host token-dir
    (when (or anew (not (ignore-errors (elpaso-admin-form-from-file-contents
					(concat (file-name-as-directory token-dir)
						"access_token")))))
      (let* (ws-servers
	     (plist (alist-get host elpaso-disc-host-info))
	     (url (plist-get plist :url))
	     (client-id (plist-get plist :client-id))
	     (parsed (url-generic-parse-url elpaso-disc-redirect-uri))
	     (port (url-port parsed))
	     (redirect-uri elpaso-disc-redirect-uri)
	     (prompt (format "[%s] After verifying, press (d)one or (r)etry: " host))
	     (inhibit-quit t))
	(when (y-or-n-p (format "[%s] Open browser to %s? " host url))
	  (unwind-protect
	      (let* ((state (secure-hash 'md5 (concat (system-name)
						      (number-to-string (float-time)))))
		     (callback
		      (lambda (request)
			(with-slots (process headers) request
			  (let* ((diag (cl-remove-if-not (lambda (x) (stringp (car x)))
							 headers))
				 (code (assoc-default "code" headers))
				 (fail (not code)))
			    (unless fail
			      (request elpaso-disc-endpoint
				:sync t
				:type "POST"
				;; params v. data as latter requires urldecode server-side
				:params (pcase host
					  ('github
					   `((client_id . ,client-id)
					     (code . ,(assoc-default "code" headers))
					     (redirect_uri . ,redirect-uri)
					     (state . ,state)))
					  ('gitlab
					   `((client_id . ,client-id)
					     (code . ,(assoc-default "code" headers))
					     (redirect_uri . ,redirect-uri)
					     (grant_type . "authorization_code")
					     (state . ,state))))
				:parser 'json-read
				:success (cl-function
					  (lambda (&key data &allow-other-keys)
					    (let-alist data
					      (when .access_token
						(elpaso-disc-squirrel host .access_token)))))
				:error (cl-function
					(lambda (&key data error-thrown &allow-other-keys)
					  (setq fail (or (alist-get 'message data)
							 error-thrown))))))
			    (ws-response-header process 200 '("Content-type" . "text/html"))
			    (process-send-string
			     process
			     (format "<h2>Authentication %s</h2><p>%s%s"
				     (if fail "failed" "succeeded")
				     (if (stringp fail)
					 (format "<p>%s" fail)
				       "")
				     (concat "<table><tr>"
					     (mapconcat
					      (lambda (pair)
						(format "<th>%s</th><td>%s</td>"
							(car pair) (cdr pair)))
					      diag
					      "</tr><tr>")
					     "</tr></table>"))))))))
		(ws-start `(((:GET . ".*") . ,callback)) port)
		(with-local-quit
		  (cl-loop do (browse-url
			       (concat url "?"
				       (request--urlencode-alist
					(pcase host
					  ('gitlab
					   `((client_id . ,client-id)
					     (response_type . "code")
					     (redirect_uri . ,redirect-uri)
					     (state . ,state)
					     (scope . "read_api")))
					  ('github
					   `((client_id . ,client-id)
					     (login . "")
					     (scope . "")
					     (redirect_uri . ,redirect-uri)
					     (state . ,state)))))))
			   if (eq ?d (read-char-choice prompt '(?D ?d ?R ?r)))
			   do (message "") and return nil
			   end)))
	    (ws-stop-all)))))))

(defun elpaso-disc--encode-vector (value)
  (when (vectorp value)
    (format "[%s]" (mapconcat #'gsexp--encode-value value " "))))

(defmacro elpaso-disc--query-query (host query &optional variables callback errorback)
  (declare (indent defun))
  (setq variables (or variables '(quote ((unused))))) ;; workaround ghub-graphql:350
  (setq callback (or callback '(function ignore)))
  `(unwind-protect
       (let* ((url (pcase ,host
		     ('github "api.github.com")
		     ('gitlab "gitlab.com/api"))))
         (add-function :before-until (symbol-function 'gsexp--encode-value)
                       #'elpaso-disc--encode-vector)
         (ghub--graphql-retrieve
          (ghub--make-graphql-req
           :url       (url-generic-parse-url (format "https://%s/graphql" url))
           :method    "POST"
           :headers   (list
		       (pcase ,host
		         ('github (cons "Accept" "application/vnd.github.v3+json"))
		         ('gitlab (cons "Content-Type" "application/json")))
		       (cons "Authorization" (format "bearer %s" (alist-get ,host elpaso-disc--access-token))))
           :handler   'ghub--graphql-handle-response
           :query     ,query
           :variables ,variables
           :buffer    (current-buffer)
           :callback  (apply-partially
                       (lambda (buffer* data)
	                 (ghub--graphql-set-mode-line buffer* nil)
	                 (funcall ,callback data))
                       (current-buffer))
           :errorback ,errorback)))
     (remove-function (symbol-function 'gsexp--encode-value)
                      #'elpaso-disc--encode-vector)))

(defmacro elpaso-disc--results-setter (bindings &rest body)
  (declare (indent defun))
  `(lambda (data)
     (pcase-let* ,bindings
       (mapc (lambda (node)
	       (dolist (pair node)
                 (cl-loop with pair2 = pair
                          unless (symbolp (car pair2))
                          do (setq pair2 (car pair2))
                          until (atom (cdr pair2))
                          if (symbolp (car pair2))
                            do (setq pair2 (cl-second pair2))
                          else
                            return nil
                          end
                          finally do (setcdr pair (cdr pair2)))))
	     nodes)
       (setq elpaso-disc--results (cl-remove-if-not #'identity nodes))
       ,@body)))

(cl-defun elpaso-disc--query-results (host
				     &rest words
                                     &key first (callback #'ignore)
                                     &allow-other-keys)
  (setq first (or first elpaso-disc-number-results))
  (cl-loop for i = 0 then (1+ i)
           until (>= i (length words))
           for word = (nth i words)
           if (keywordp word)
             do (cl-incf i)
           else
             collect word into result
           end
           finally do (setq words result))
  (let ((callback-github
         (elpaso-disc--results-setter ((`(data (search (nodes . ,nodes))) data))
           (elpaso-disc-query-readmes host)
           (funcall callback)))
        (callback-gitlab
         (elpaso-disc--results-setter ((`(data (search (nodes . ,nodes))) data))
           (funcall callback))))
    (pcase host
      ((and 'github (guard (memq host elpaso-disc-hosts)))
       (elpaso-disc--query-query
         host
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
         callback-github))
      ((and 'gitlab (guard (memq host elpaso-disc-hosts)))
       (elpaso-disc--query-query
         host
         `(query
	   (projects
	    [(search ,(format "emacs %s" (mapconcat #'identity words " ")))
	     (first $first Int!)]
	    (nodes
	     id
	     nameWithOwner:\ fullPath
	     url:\ httpUrlToRepo
	     pushedAt:\ lastActivityAt
	     description
	     stargazers:\ starCount
	     defaultBranchRef:\ (repository rootRef))))
         `((first . ,first))
         callback-gitlab)))))

(defun elpaso-disc-query-readmes (host)
  (cl-flet* ((dodge (s) (intern s))
	     (permute
	      (u b)
	      (let ((i 0))
		(mapcar (lambda (v)
			  (cl-incf i)
			  `(,(intern (concat (or (file-name-extension v) "none")
					     (number-to-string i)
					     ": object"))
			    [(expression ,(concat b ":" v))]
			    (,(dodge "... on Blob") text)))
			(list u
			      (concat (capitalize (file-name-sans-extension u))
				      (file-name-extension u t))
			      (concat (upcase (file-name-sans-extension u))
				      (file-name-extension u t)))))))
    (dolist (node elpaso-disc--results)
      (let-alist node
        (unless (assoc .nameWithOwner elpaso-disc--readmes)
	  (pcase host
	    ('github
	     (elpaso-disc--query-query
	       host
	       `(query
		 (node [(id ,.id)]
		       (,(dodge  "... on Repository")
			,@(let (result)
			    (dolist (readme elpaso-disc--readme-filenames result)
			      (setq result (append result (permute readme .defaultBranchRef))))))))
	       nil
	       (lambda (data)
		 (pcase-let ((`(data (node . ,goods)) data))
		   (when-let ((text (cl-some #'cdr (mapcar #'cadr goods))))
		     (setf (alist-get .nameWithOwner elpaso-disc--readmes
				      nil nil #'equal)
			   (concat (cl-subseq text 0 (min 10000 (length text))) "…")))))))
	    ('gitlab
             ;; handled in `elpaso-disc--query-project'
             )))))))

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

(defun elpaso-disc--query-project (host repo callback errback)
  (if (memq host elpaso-disc-hosts)
      (let ((callback-github
             (elpaso-disc--results-setter
               ((`(data (repository . ,node)) data)
                (nodes (list node)))
               (elpaso-disc-query-readmes host)
               (funcall callback)))
            (callback-gitlab
             (elpaso-disc--results-setter
               ((`(data (project . ,node)) data)
                (nodes (list node)))
               (let-alist node
		 (setf (alist-get .nameWithOwner elpaso-disc--readmes
				  nil nil #'equal)
		       (concat (cl-subseq .readme 0 (min 10000 (length .readme))) "…")))
               (funcall callback))))
        (pcase host
          ((and 'github (guard (memq host elpaso-disc-hosts)))
           (elpaso-disc--query-query
             host
             `(query
	       (repository
	        [(name ,(file-name-nondirectory repo))
                 (owner ,(directory-file-name (file-name-directory repo)))]
	        id
	        nameWithOwner
	        url
	        pushedAt
	        description
	        (stargazers totalCount)
	        (defaultBranchRef name)))
             nil
             callback-github
             errback))
          ((and 'gitlab (guard (memq host elpaso-disc-hosts)))
           (elpaso-disc--query-query
             host
             `(query
	       (project
	        [(fullPath ,repo)]
	        id
	        nameWithOwner:\ fullPath
	        url:\ httpUrlToRepo
	        pushedAt:\ lastActivityAt
	        description
	        stargazers:\ starCount
	        defaultBranchRef:\ (repository rootRef)
                readme:\ (repository
                          (blobs
                           [(paths [,@(cl-mapcan
				       (lambda (u)
					 (list
					  u
					  (concat (capitalize (file-name-sans-extension u))
						  (file-name-extension u t))
					  (concat (upcase (file-name-sans-extension u))
						  (file-name-extension u t))))
				       elpaso-disc--readme-filenames)
                                    ])]
                           (nodes
                            rawTextBlob)))))
             nil
             callback-gitlab
             errback))))
    (if errback
        (funcall errback)
      (message "elpaso-disc--query-project: that's all she wrote"))))

(defun elpaso-disc-search (search-for &optional first)
  (elpaso-disc-set-access-token)
  ;; "emacs c++" blows up gitlab search, among other showstoppers
  ;; like extension:el withheld from non-paying public.
  (pcase (string-trim search-for)
    ((pred (string-match-p "^[^/ \f\t\n\r\v]+/[^/ \f\t\n\r\v]+$"))
     (elpaso-disc--query-project
      'github search-for
      #'elpaso-disc--present
      (lambda (&rest _args)
        (elpaso-disc--query-project 'gitlab search-for
                                   #'elpaso-disc--present nil))))
    (_
     (apply #'elpaso-disc--query-results 'github
            (append (when first
                      (list :first first))
                    (list :callback #'elpaso-disc--present)
		    (split-string search-for))))))

;;;###autoload
(defun elpaso-search (&optional first)
  (interactive "P")
  (let ((history-delete-duplicates t))
    (elpaso-disc-search (read-from-minibuffer
                         "Keywords or Repository: "
                         nil nil nil 'elpaso-disc-search-history)
                        (when (integerp first) first))))

;;;###autoload
(defalias 'elpaso #'elpaso-search)

(defalias 'elapso-search #'elpaso-search)
(defalias 'elapso #'elpaso-search)

(provide 'elpaso-disc)
;;; elpaso-disc.el ends here
