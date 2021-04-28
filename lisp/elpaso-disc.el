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

(require 'request)
(require 'elpaso-admin)
(require 'ghub)

(defconst elpaso-disc-github-client-id "1f006d815c4bb23dfe96")
(defvar elpaso-disc--access-token nil)

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
    (when (or force
              (and acquire-p (not elpaso-disc--access-token)))
      (setq elpaso-disc--access-token nil)
      (elpaso-disc--new-access-token)
      (elpaso-disc-set-access-token :force nil :acquire-p nil))
    (unless elpaso-disc--access-token
      (error "elpaso-disc-set-access-token: authentication failure"))))

(defconst elpaso-disc-github-url-login-token
  "https://github.com/login/oauth/access_token")

(defconst elpaso-disc-github-url-login-code
  "https://github.com/login/device/code")

(defun elpaso-disc--new-access-token ()
  "Do the dance."
  (let* (result
	 (client-id elpaso-disc-github-client-id)
	 (headers (list (cons "Accept" "application/vnd.github.v3+json")))
	 (with-grant-yield-access
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
	      :success with-grant-yield-access)))
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

(ghub--username (ghub--host 'github) 'github)
(funcall (ghub--headers nil (ghub--host 'github) nil (ghub--username (ghub--host 'github) 'github) 'github))

(let (variables
      until
      errorback
      callback
      narrow
      (host (ghub--host 'github))
      (query (json-encode '(:query "{viewer { login }}"))))
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
    :query     query
    :variables variables
    :until     until
    :buffer    (current-buffer)
    :callback  (let ((buf (current-buffer)))
		 (if narrow
                     (lambda (data)
                       (let ((path narrow) key)
			 (while (setq key (pop path))
                           (setq data (cdr (assq key data)))))
                       (ghub--graphql-set-mode-line buf nil)
                       (funcall callback data))
                   (lambda (data)
                     (ghub--graphql-set-mode-line buf nil)
                     (funcall callback data))))
    :errorback errorback)))

(gsexp-encode (ghub--graphql-prepare-query ghub-fetch-repository-review-threads))
