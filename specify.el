;; -*-lexical-binding: t -*-

(require 'url)
(require 'json)


(defvar specify/http-status-line
  "^HTTP/1\\.[01] \\([0-9]+\\) \\(.+\\)$")

(defvar specify/http-header-line
  "^\\([^:]+\\): \\(.+\\)$")

(defun specify/parse-status-line ()
  (if (not (looking-at specify/http-status-line))
      (message "bad http-status-line: %s"
               (buffer-substring (point) (line-end-position)))
    (progn
      (forward-line)
      (list (match-string 1) (match-string 2)))))

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
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (list (specify/parse-status-line)
                  (specify/parse-headers)
                  (let ((json-object-type 'hash-table)) (json-read))))
        (kill-buffer buffer)))))

(defvar specify/server-hist nil)

(defun specify (host)
  (interactive (list (read-string "Specify server: " nil 'specify/server-hist)))
  (apply 'specify/login (specify/get (concat "http://" host "/context/login/"))))

(defvar specify/username-hist nil)
(defvar specify/collection-hist nil)

(defun specify/login (status headers data)
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

  (message "login: %s" (json-encode data)))

;;(specify/get "http://127.0.0.1:8000/context/login/")

(provide 'specify)
