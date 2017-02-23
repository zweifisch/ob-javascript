;;; ob-javascript.el --- org-babel functions for javascript evaluation

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-javascript
;; Keywords: org babel javascript
;; Version: 0.0.1
;; Created: 26th Nov 2016
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; org-babel functions for javascript evaluation
;;

;;; Code:
(require 'ob)

(defvar ob-javascript-process-output nil)

(defvar ob-javascript-eoe "\u2029")
(defvar ob-javascript-eoe-js "\\u2029")
(defvar ob-javascript-timeout 5)

(defconst ob-javascript-path-to-lib
  (file-name-directory (or load-file-name buffer-file-name)))

(defun org-babel-execute:javascript (body params)
  (let ((session (or (cdr (assoc :session params)) "default"))
        (result-type (cdr (assoc :result-type params)))
        (file (cdr (assoc :file params)))
        (body (if (assoc :babel params) (ob-javascript--babel body) body)))
    (if (string= "none" session)
        (ob-javascript--eval body file)
      (ob-javascript--eval-with-session session body file))))

(defun ob-javascript--output (result file)
  (unless file result))

(defun ob-javascript--babel (source)
  (with-temp-buffer
    (insert source)
    (call-process-region (point-min) (point-max) "babel" t t nil "--filename" "./whereami")
    (buffer-string)))

(defun ob-javascript--eval (body file)
  (let ((tmp-source (org-babel-temp-file "javascript-"))
        (tmp (org-babel-temp-file "javascript-")))
    (with-temp-file tmp-source (insert body))
    (with-temp-file tmp
      (insert
       (with-temp-buffer
         (insert-file-contents (concat ob-javascript-path-to-lib "util.js"))
         (buffer-string)))
      (insert (format "__ob_eval__('%s', '', '%s')" tmp-source (or file ""))))
    (ob-javascript--output
      (ob-javascript--shell-command-to-string
       (list (format "NODE_PATH=%s" (ob-javascript--node-path)))
       (list "node" tmp))
      file)))

(defun ob-javascript--eval-with-session (session body file)
  (let ((tmp (org-babel-temp-file "javascript-")))
    (ob-javascript--ensure-session session)
    (with-temp-file tmp (insert body))
    (ob-javascript--output
     (ob-javascript--eval-in-repl
      session
      (format "__ob_eval__('%s', '%s', '%s')" tmp ob-javascript-eoe-js (or file "")))
     file)))

(defun ob-javascript--shell-command-to-string (environ command)
  (with-temp-buffer
    (let ((process-environment (append environ process-environment)))
      (apply 'call-process (car command) nil t nil (cdr command))
      (buffer-string))))

(defun ob-javascript--node-path ()
  (let ((node-path (or (getenv "NODE_PATH") "")))
    (format "%s:%snode_modules"
            node-path
            (file-name-directory
             (buffer-file-name)))))

(defun ob-javascript--ensure-session (session)
  (let ((name (format "*javascript-%s*" session))
        (node-path (ob-javascript--node-path)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (let ((process-environment
               (append (list "NODE_NO_READLINE=1"
                             (format "NODE_PATH=%s" node-path))
                       process-environment)))
          (start-process name name "node" (concat ob-javascript-path-to-lib "repl.js"))))
      (sit-for 0.5)
      (set-process-filter (get-process name) 'ob-javascript--process-filter))))

(defun ob-javascript--process-filter (process output)
  (setq ob-javascript-process-output (concat ob-javascript-process-output output)))

(defun ob-javascript--wait (timeout)
  (while (and (not (string-match-p ob-javascript-eoe ob-javascript-process-output))
              (> timeout 0))
    (setq timeout (- timeout 0.2))
    (sit-for 0.2)))

(defun ob-javascript--eval-in-repl (session body)
  (let ((name (format "*javascript-%s*" session)))
    (setq ob-javascript-process-output nil)
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (ob-javascript--wait ob-javascript-timeout)
    (message
     (replace-regexp-in-string ob-javascript-eoe "" ob-javascript-process-output))))

(provide 'ob-javascript)
;;; ob-javascript.el ends here
