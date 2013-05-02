;; -*-lexical-binding: t -*-

(require 'url)
(require 'json)


(defvar specify/http-status-line
  "^HTTP/1\\.[01] \\([0-9]+\\) \\(.+\\)$")

(defvar specify/http-header-line
  "^\\([^:]+\\): \\(.+\\)$")

(defun specify/status-success-p (status)
  (let ((code (car status)))
    (and (>= code 200) (< code 300))))

(defun specify/parse-status-line ()
  (if (not (looking-at specify/http-status-line))
      (message "bad http-status-line: %s"
               (buffer-substring (point) (line-end-position)))
    (progn
      (forward-line)
      (list (string-to-number (match-string 1)) (match-string 2)))))

(defun specify/parse-headers (&optional headers)
  (cond ((looking-at "^\r?$") headers)

        ((looking-at specify/http-header-line)
         (forward-line)
         (let ((key (match-string 1))
               (value (match-string 2)))
           (when (string-match "[ \r]+$" value)
             (setq value (replace-match "" t t value)))
           (specify/parse-headers (cons (cons key value) headers))))

        (t (error "bad http header: %s"
                  (buffer-substring (point) (line-end-position))))))

(defun specify/get (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (specify/process-response url buffer)))

(defun specify/process-response (url buffer)
  (with-current-buffer buffer
    (unwind-protect
        (progn
          (goto-char (point-min))
          (list url
                (specify/parse-status-line)
                (specify/parse-headers)
                (let ((json-object-type 'hash-table))
                  (condition-case nil (json-read) (end-of-file nil)))))
      (kill-buffer buffer))))

(defun specify/put (url data)
  (let* ((url-request-method "PUT")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode data))
         (buffer (url-retrieve-synchronously url)))
    (specify/process-response url buffer)))

(defvar specify/server-hist nil)

(defun specify (host)
  (interactive (list (read-string "Specify server: " nil 'specify/server-hist)))
  (apply 'specify/login (specify/get (concat "http://" host "/context/login/"))))

(defvar specify/username-hist nil)
(defvar specify/collection-hist nil)
(defvar specify/login-data nil)

(defun specify/login (url status headers data)
  (setq specify/login-data (copy-hash-table data))

  (puthash "username"
           (read-from-minibuffer "Username: "
                                 nil nil nil
                                 'specify/username-hist)
           data)
  (puthash "password" (read-passwd "Password: ") data)

  (let ((collections (gethash "collections" data))
        (completion-ignore-case t))
    (puthash "collection"
             (gethash (completing-read "Collection: "
                                collections
                                nil t nil
                                'specify/collection-hist)
                      collections)
             data))

  (apply 'specify/complete-login (specify/put url data)))


(defvar specify/api-url nil)

(defun specify/complete-login (url status headers data)
  (setq specify/api-url
        (replace-regexp-in-string "/context/login/" "/api/specify/" url)))

(defun specify-logout ()
  (interactive)
  (if (not specify/api-url) (error "Not logged in."))
  (let ((url (replace-regexp-in-string "/api/specify/" "/context/login/" specify/api-url))
        (data (copy-hash-table specify/login-data)))
    (puthash "username" nil data)
    (apply 'specify/logout-complete (specify/put url data))))

(defun specify/logout-complete (url status headers data)
  (if (not (specify/status-success-p status)) (error "Logout failed."))
  (setq specify/api-url nil)
  (message "Logged out."))

(defvar specify/get-uri-hist nil)

(defun specify-get (uri)
  (interactive (list (read-string "URI: " nil 'specify/get-uri-hist)))
  (if (not specify/api-url) (error "Not logged in.")
    (apply 'specify/show-result (specify/get (concat specify/api-url uri)))))

(defun specify/show-result (url status headers data)
  (with-output-to-temp-buffer url
      (print status)
      (print data)))

(provide 'specify)
