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
                  (json-read)))
        (kill-buffer buffer)))))

(defun specify (host)
  (interactive (list (read-string "Specify server:")))
  (with-current-buffer
      (url-retrieve-synchronously (contcat "http://" host "/"))))

(defun specify-login (httpc headers data)
  (with-current-buffer (get-buffer-create "specify-test")
    (goto-char (point-max))
    (insert data)))

;;(specify/get "http://dhwd99p1.nhm.ku.edu:8000/context/login/")

(provide 'specify)
