;;; junos-mode.el --- Major mode for JunOS configuration files

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Vincent Bernat
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Simple mode for JunOS-like files

;;; Code:
(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defvar junos-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?- "w" st)
     (modify-syntax-entry ?+ "w" st)
     (modify-syntax-entry ?_ "w" st)
     (modify-syntax-entry ?. "w" st)
     (modify-syntax-entry ?/ "w 124" st)
     (modify-syntax-entry ?* ". 23b" st)
     (modify-syntax-entry ?\# "<" st)
     (modify-syntax-entry ?\n ">" st)
     (modify-syntax-entry ?\; "." st)
     (modify-syntax-entry ?{ "(" st)
     (modify-syntax-entry ?} ")" st)
     st)
   "Syntax table for `junos-mode'.")

(defvar junos-font-lock-keywords
  '(("^\\s-*\\(\\(inactive\\|delete\\|replace\\):\\s-+\\)?\\(\\sw+\\)\\s-*\\(\\(\\sw+\\s-+\\)*\\){\\s-*\\(\\s<.*\\)?$"
     (1 'font-lock-keyword-face nil t)
     (3 'font-lock-function-name-face)
     (4 'font-lock-variable-name-face nil t))
    ("^\\s-*\\(\\(inactive\\|delete\\|replace\\):\\s-+\\)?\\(\\sw+\\)"
     (1 'font-lock-keyword-face nil t)
     (3 'font-lock-type-face))
    ;; IP
    ("\\([[:digit:]]+\\.\\)\\{3\\}[[:digit:]]+\\(/[[:digit:]]+\\)?" . 'font-lock-constant-face)
    ("\\(?:\\(?:[[:xdigit:]]+\\)?:\\)+[[:xdigit:]]*\\(/[[:digit:]]+\\)?" . 'font-lock-constant-face))
  "Keyword highlighting specification for `junos-mode'.")

;;;###autoload
(define-derived-mode junos-mode c-mode "JunOS"
  "A major mode for editing JunOS files."
  :syntax-table junos-mode-syntax-table
  (c-set-offset 'label '+)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(junos-font-lock-keywords)))

(provide 'junos-mode)
;;; junos-mode.el ends here
