;;; ob-junos.el --- org-babel functions for JunOS evaluation

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Vincent Bernat

;; This file is NOT part of GNU Emacs.

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
(require 'uuid)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("junos" . "cfg"))

(defconst org-babel-header-args:junos
  '((host . :any))
  "JunOS-specific header arguments.")

(defconst org-babel-junos-junos.py-path
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-directory (file-name-directory current-file))
         (junos-path (concat current-directory "junos.py")))
    junos-path)
  "Path to junos.py helper.")

(defvar org-babel-default-header-args:junos '())

(defun org-babel-expand-body:junos (body params)
  "Expand BODY according to PARAMS, return the expanded body.

Variables are expected to be `$variable-form'.  Unknown variable
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

This function is called by `org-babel-execute-src-block'.  It
should get a BODY and the associated PARAMS."
  (let* ((host-name (or (cdr (assq :host params))
                        (error "An HOST parameter is mandatory")))
         (session (org-babel-junos-initiate-session))
         (proc (get-buffer-process session))
         (uuid-load (uuid-string))
         (uuid-diff (uuid-string))
         (uuid-check (uuid-string))
         (full-body (org-babel-expand-body:junos
		     body params)))
    ;; Load
    (comint-send-string session
                        (concat uuid-load "+"
                                "load " host-name "\n"))
    (comint-send-string session
                        (concat
                         (s-join "\n"
                                 (mapcar (lambda (l) (concat uuid-load ">" l))
                                         (s-lines full-body))) "\n"))
    (comint-send-string session
                        (concat uuid-load ".\n"))
    ;; Diff
    (comint-send-string session
                        (concat uuid-diff " "
                                "diff " host-name "\n"))
    ;; Check
    (comint-send-string session
                        (concat uuid-check " "
                                "check " host-name "\n"))
    ;; Build result with placeholders
    (s-join "\n" (list
                  (format "Host: %s" host-name)
                  "Load replace"
                  "‾‾‾‾‾‾‾‾‾‾‾‾"
                  (format "<async:junos:%s>" uuid-load)
                  "Checks"
                  "‾‾‾‾‾‾"
                  (format "<async:junos:%s>" uuid-check)
                  ""
                  "Differences"
                  "‾‾‾‾‾‾‾‾‾‾‾"
                  (format "<async:junos:%s>" uuid-diff)))))

(defun org-babel-junos-initiate-session ()
  "If there is not a current session for junos.py, create one.

Return the initialized session.  The session will be
created.  Returns the (possibly newly created) process buffer."
  (save-window-excursion
    (let ((buffer (make-comint-in-buffer "junos.py" nil org-babel-junos-junos.py-path)))
      (with-current-buffer buffer
        (remove-hook 'comint-output-filter-functions #'ansi-color-process-output t)
        (remove-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt t)
        (add-hook 'comint-preoutput-filter-functions #'org-babel-junos-junos.py-output))
      buffer)))

(defun org-babel-junos-junos.py-output (string)
  "Receive a new output string STRING and dispatch it."
  (dolist (line (s-lines string))
    ;; Parse the string to extract UUID, separator and line.
    (when-let ((matches (s-match "^\\([a-zA-Z0-9_-]+\\)\\(.\\)\\(.*\\)" line)))
      (let ((uuid (format "\n<async:junos:%s>\n" (nth 1 matches)))
            (sep (nth 2 matches))
            (line (nth 3 matches))
            (buffers (org-buffer-list 'files t)))
        (when (not (and (string= sep "+") (string= line "ok")))
          (cl-loop for buffer in buffers
                   until (with-current-buffer buffer
                           (save-excursion
                             (goto-char 0)
                             (when (search-forward uuid nil t)
                               (forward-line -1)
                               (insert (concat "   " line "\n"))
                               (when (and (string= sep " ") (string= line "ok"))
                                 (forward-line -3)
                                 (end-of-line)
                                 (insert ": ✓")
                                 (let ((beg (point)))
                                   (forward-line 2)
                                   (delete-region beg (point)))
                                 (delete-char 5)
                                 (forward-line 1))
                               (when (or (string= sep " ")
                                         (string= sep "."))
                                 (let ((beg (point)))
                                   (forward-line 1)
                                   (delete-region beg (point)))
                                 (org-babel-junos-junos.py-add-links))
                               t))))))))
  string)

(defun org-babel-junos-junos.py-add-links ()
  "Add links for commit/rollback just below the current results."
  (save-restriction
    (org-narrow-to-block)
    (goto-char 0)
    (unless (search-forward "<async:junos:" nil t)
      ;; Grab the host from the first line
      (goto-char 0)
      (forward-line 1)
      (when (looking-at "Host: ")
        (let* ((beg (point))
               (end (line-end-position))
               (host (buffer-substring-no-properties (+ 6 beg) end)))
          (forward-line 1)
          (delete-region beg (point))
          ;; We can now add the links
          (goto-char (point-max))
          (widen)
          (forward-char)
          (insert (format " | [[junos-commit:%s][Commit]]  🍂" host))
          (insert (format  "  [[junos-commit:%s#2][Commit confirm]]  🍂" host))
          (insert (format  "  [[junos-rollback:%s][Rollback]]  🍂" host))
          (insert (format  "  [[junos-rollback:%s#1][Rollback 1]]\n" host))
          (when (looking-at " | \\[\\[junos-commit:")
            (delete-region (point) (line-end-position))))))))

<async:junos:
(org-add-link-type "junos-commit" 'org-junos-commit)
(org-add-link-type "junos-rollback" 'org-junos-rollback)

(defun org-junos-do (command host)
  "Execute a JunOS COMMAND for the given HOST.

If HOST contains ends with `::' followed by a number, it will be
used for additional arguments."
  (let* ((matches (s-match "\\(.*?\\)\\(#\\([0-9]+\\)\\)?$" host))
         (host (nth 1 matches))
         (number (nth 3 matches))
         (uuid-commit (uuid-string))
         (session (org-babel-junos-initiate-session)))
    (save-excursion)
    (message (format "junos: %s/%s for %s" command number host))
    (comint-send-string session
                        (concat uuid-commit " "
                                command " " host
                                (if number (format " %s" number) "")
                                "\n"))))

(defun org-junos-commit (host)
  "Commit a JunOS configuration for provided HOST."
  (org-junos-do "commit" host))
(defun org-junos-rollback (host)
  "Rollback JunOS configuration for provided HOST."
  (org-junos-do "rollback" host))

(provide 'ob-junos)
;;; ob-junos.el ends here
