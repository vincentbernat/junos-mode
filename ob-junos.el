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
(require 'ob)
(require 'cl-lib)
(require 's)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("junos" . "cfg"))

(defconst org-babel-header-args:junos
  '((host		 . :any))
  "junos-specific header arguments.")

(defvar org-babel-default-header-args:junos '())

(defun org-babel-expand-body:junos (body params)
  "Expand BODY according to PARAMS, return the expanded body.

Variables are expected to be `$variable-form'. Unknown variable
are left as-is."
  (let ((vars (org-babel--get-vars params)))
    (cl-reduce (lambda (body pair)
                 (replace-regexp-in-string
                  (concat "\\b"
                          (regexp-quote (concat "$" (symbol-name (car pair))))
                          "\\b")
                  (format "%s" (cdr pair))
                  body))
               vars
               :initial-value body)))

;;;###autoload
(defun org-babel-execute:junos (body params)
  "Execute a block of JunOS code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((host-name (or (cdr (assq :host params))
                        (error "An HOST parameter is mandatory")))
         (session (org-babel-junos-initiate-session host-name))
         (p (get-buffer-process session))
         (full-body (org-babel-expand-body:junos
		     body params)))
    (junos-inf-send-command p "edit")
    (junos-inf-wait-for-prompt p)
    (junos-inf-send-command p "top load replace terminal")
    (let ((load-output
           (with-current-buffer session
             (accept-process-output p)         ; Wait for prompt
             (goto-char (point-max))
             (junos-inf-send-string p (concat full-body "\n"))
             (let ((parsing-end (marker-position (process-mark p))))
               (comint-send-eof)
               (junos-inf-wait-for-prompt p)
               (goto-char (point-max))
               (save-excursion
                 (end-of-line 0)
                 (buffer-substring-no-properties
                  (save-excursion (goto-char parsing-end)
                                  (line-beginning-position 2))
                  (point)))))))
      (concat "Load replace output:\n"
              "————————————————————\n"
              (s-chop-suffix "[edit]" load-output)
              "Differences:\n"
              "————————————\n"
              (s-chop-suffix "[edit]" (junos-inf-send-command-and-get-result p "show | diff"))
              "Checks:\n"
              "———————\n"
              (s-chop-suffix "[edit]" (junos-inf-send-command-and-get-result p "commit check"))))))

(defun org-babel-junos-initiate-session (&optional host)
  "If there is not a current session for HOST then create.
Return the initialized session.  The session will be created with
HOST as a target."
  (require 'junos-inf)
  (save-window-excursion
    (let* ((bufname (concat "*" "junos " host "*"))
           (all-buffers (mapcar #'buffer-name (buffer-list)))
           (host-buffers (remove-if-not (lambda (name) (s-starts-with? bufname name)) all-buffers))
           (live-buffers (remove-if-not #'org-babel-comint-buffer-livep host-buffers)))
      (or (car live-buffers)
          (run-junos host)))))

(provide 'ob-junos)
;;; ob-junos.el ends here
