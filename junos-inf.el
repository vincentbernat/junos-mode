;;; junos-inf.el --- JunOS inferior mode

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

;;; Code:
(require 'comint)

(defvar junos-inf-prompt-regexp "^\\(?:[^@]+@[^@>#]+[>#] \\)"
  "Prompt for JunOS session.")

(define-derived-mode junos-inf-mode comint-mode "JunOS"
  "A major mode for running JunOS CLI."
  (setq-local paragraph-start junos-inf-prompt-regexp)
  (setq-local comint-process-echoes t)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp junos-inf-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (add-hook 'comint-output-filter-functions 'junos-inf-spot-prompt nil t))

(defun run-junos (host)
  "Start an a JunOS connection into an inferior process.
The JunOS connection is made with ssh to the provided
HOST."
  (let* ((buffer
          (apply 'make-comint (concat "junos " host) "ssh" nil (list host)))
         (p (get-buffer-process buffer)))
    (with-current-buffer buffer
      (junos-inf-mode)
      (junos-inf-wait-for-prompt p)
      (dolist (cmd '("set cli screen-width 0"
                     "set cli screen-length 0"
                     "set cli timestamp"))
        (junos-inf-send-command p cmd)
        (junos-inf-wait-for-prompt p))
      (goto-char (process-mark p)))
    buffer))

(defvar junos-inf-seen-prompt nil)
(make-variable-buffer-local 'junos-inf-seen-prompt)

(defun junos-inf-spot-prompt (str)
  "Spot the prompt in the current process buffer.
STR is ignored."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq junos-inf-seen-prompt t))))))

(defun junos-inf-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer.  Don't
wait more than TIMEOUT."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or junos-inf-seen-prompt
                      (setq junos-inf-seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless junos-inf-seen-prompt
      (error "Can't find the prompt"))))

(defun junos-inf-send-command (proc str)
  "Send a command to a given PROC.
The command is STR and a new line is automatically appended."
  (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (junos-inf-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (move-marker comint-last-input-end (point))
    (setq junos-inf-seen-prompt nil)
    (comint-send-string proc str)))

(provide 'junos-inf)
;;; junos-inf.el ends here
