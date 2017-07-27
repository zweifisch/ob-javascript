;;; ob-javascript.el --- org-babel functions for javascript evaluation

;; Copyright (C) 2016 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-javascript
;; Keywords: org babel javascript
;; Version: 0.0.1
;; Created: 26th Nov 2016
;; Package-Requires: ((org "8"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for javascript evaluation
;;

;;; Code:
(require 'ob)
(require 'json)

(defvar ob-javascript-process-output nil)

(defvar ob-javascript-eoe "\u2029")
(defvar ob-javascript-eoe-js "\\u2029")
(defvar ob-javascript-timeout 5)

(defgroup ob-javascript nil
  "org-babel functions for javascript evaluation"
  :group 'org)

(defcustom ob-javascript:browser-binary "chromium-browser"
  "browser binary"
  :group 'ob-javascript
  :type 'string)

(defconst ob-javascript-path-to-lib
  (file-name-directory (or load-file-name buffer-file-name)))

(defun org-babel-execute:javascript (body params)
  (let ((session (or (cdr (assoc :session params)) "default"))
        (result-type (cdr (assoc :result-type params)))
        (file (cdr (assoc :file params)))
        (body (if (assoc :babel params) (ob-javascript--babel body) (list 't body))))
    (if (car body)
        (if (string= "none" session)
            (ob-javascript--eval (cadr body) file)
          (if (or
               (string-prefix-p "http://" session)
               (string-prefix-p "https://" session))
              (progn
                (ob-javascript--ensure-browser-session session)
                (ob-javascript--get-result-value
                 (ob-javascript--eval-in-browser-repl session (cadr body))))
            (ob-javascript--eval-with-session session (cadr body) file)))
      (cadr body))))

(defun ob-javascript--output (result file)
  (unless file result))

(defun ob-javascript--babel (source)
  (with-temp-buffer
    (insert source)
    (list
     (eq 0
         (call-process-region (point-min) (point-max) "babel" t t nil "--filename" "./whereami"))
     (buffer-string))))

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
  (let ((node-path (or (getenv "NODE_PATH") ""))
        (node-modules (locate-dominating-file (buffer-file-name) "node_modules")))
    (if node-modules
        (format "%s:%snode_modules" node-path (file-truename node-modules))
      node-path)))

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

(defun ob-javascript--wait (timeout what)
  (while (and (not (string-match-p what ob-javascript-process-output))
              (> timeout 0))
    (setq timeout (- timeout 0.2))
    (sit-for 0.2)))

(defun ob-javascript--eval-in-repl (session body)
  (let ((name (format "*javascript-%s*" session)))
    (setq ob-javascript-process-output nil)
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (ob-javascript--wait ob-javascript-timeout ob-javascript-eoe)
    (message
     (replace-regexp-in-string ob-javascript-eoe "" ob-javascript-process-output))))

(defun ob-javascript--ensure-browser-session (session)
  (let ((name (format "*ob-javascript-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (let ((process (with-current-buffer (get-buffer-create name)
                       (start-process name name
                                      ob-javascript:browser-binary "--headless" "--disable-gpu" "--repl" session))))
        (sit-for 1)
        (set-process-filter process 'ob-javascript--process-filter)
        (ob-javascript--wait ob-javascript-timeout "Type a Javascript expression to evaluate or \"quit\" to exit.")))))

(defun ob-javascript--eval-in-browser-repl (session body)
  (let ((name (format "*ob-javascript-%s*" session)))
    (setq ob-javascript-process-output "")
    (process-send-string name (format "%s\n\"%s\"\n" body ob-javascript-eoe))
    (accept-process-output (get-process name) nil nil 1)
    (ob-javascript--wait ob-javascript-timeout ob-javascript-eoe)
    (replace-regexp-in-string
     "^>>> " ""
     (replace-regexp-in-string
      (format "^.*\"%s\".*$" ob-javascript-eoe) "" ob-javascript-process-output))))

(defun ob-javascript--get-result-value (result)
  (let* ((result (assoc-default 'result (json-read-from-string result)))
         (value (assoc 'value result)))
    (if value (cdr value)
      (assoc-default 'description result))))

(provide 'ob-javascript)
;;; ob-javascript.el ends here
