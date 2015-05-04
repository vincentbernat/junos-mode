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
  (setq-local comint-prompt-read-only t))

(defun run-junos (host)
  "Start an a JunOS connection into an inferior process.
The JunOS connection is made with ssh to the provided
HOST."
  (let ((buffer
         (apply 'make-comint (concat "junos " host) "ssh" nil (list host))))
    (with-current-buffer buffer
      (junos-inf-mode))
    buffer))

(let* ((buffer (run-junos "gv2p-pf-cloud01.exoscale.local"))
       (p (get-buffer-process buffer)))
  (comint-send-string p "set cli screen-width 0\n")
  (comint-send-string p "set cli screen-length 0\n")
  (comint-send-string p "set cli timestamp\n"))

(provide 'junos-inf)
;;; junos-inf.el ends here
