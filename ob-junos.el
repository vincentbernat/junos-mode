;;; ob-junos.el --- org-babel functions for junos evaluation

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Vincent Bernat

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This will evaluate JunOS snippets to a remote JunOS device.

;;; Code:
(require 'junos-inf)
(require 'ob)
(require 'cl-lib)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("junos" . "cfg"))

(defvar org-babel-default-header-args:junos '())

(defun org-babel-expand-body:junos (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body.

Variables are expected to be `$variable-form'. Unknown variable
are left as-is."
  (let ((vars (assoc :var (or processed-params (org-babel-process-params params)))))
    (cl-reduce (lambda (body pair)
                 (replace-regexp-in-string
                  (concat "\\b"
                          (regexp-quote (concat "$" (car pair)))
                          "\\b")
                  (cdr pair)
                  body))
               (cons body vars))))

;;;###autoload
(defun org-babel-execute:junos (body params)
  "Execute a block of JunOS code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((host-name (or (cdr (assoc :host params))
                        (error "An HOST parameter is mandatory")))
         (session (org-babel-junos-initiate-session host-name))
         (p (get-buffer-process session))
         (full-body (org-babel-expand-body:junos
		     body params)))
    (junos-inf-send-command p "edit")
    (junos-inf-wait-for-prompt p)
    (junos-inf-send-command p "top load replace terminal")
    (with-current-buffer session
      (accept-process-output p)         ; Wait for prompt
      (goto-char (point-max))
      (process-send-string p full-body)
      (process-send-string p "\n")
      (goto-char (point-max))
      (let ((parsing-end (marker-position (process-mark p))))
        (comint-send-eof)
        (junos-inf-wait-for-prompt p)
        (junos-inf-send-command p "show | diff")
        (junos-inf-wait-for-prompt p)
        (junos-inf-send-command p "commit check")
        (junos-inf-wait-for-prompt p)
        (goto-char (point-max))
        (save-excursion
          (end-of-line 0)
          (buffer-substring-no-properties
           (save-excursion (goto-char parsing-end)
                           (line-beginning-position 2))
           (point)))))))

(defun org-babel-junos-initiate-session (&optional host)
  "If there is not a current session for HOST then create.
Return the initialized session.  The session will be created with
HOST as a target."
  (save-window-excursion
    (or (org-babel-comint-buffer-livep (concat "*" "junos " host "*"))
        (run-junos host))))

(provide 'ob-junos)
;;; ob-junos.el ends here
