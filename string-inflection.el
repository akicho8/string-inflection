;;; string-inflection.el --- underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names

;; Copyright (C) 2004,2014  Free Software Foundation, Inc.

;; Author: akicho8 <akicho8@gmail.com>
;; Keywords: elisp
;; Version: 1.0.2

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

;; (require 'string-inflection)
;; (global-unset-key (kbd "C-q"))
;; (global-set-key (kbd "C-q C-u") 'string-inflection-ruby-style-cycle) ; Vz Editor-like key binding

;;; Code:

(defconst string-inflection-word-chars "a-zA-Z0-9_")
(defconst string-inflection-non-word-chars (concat "^" string-inflection-word-chars))

;;--------------------------------------------------------------------------------

(fset 'string-inflection-cycle 'string-inflection-ruby-style-cycle)

;;;###autoload
(defun string-inflection-ruby-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (insert (string-inflection-ruby-style-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-java-style-cycle ()
  "fooBar => FOO_BAR => FooBar => fooBar"
  (interactive)
  (insert (string-inflection-java-style-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-all-cycle ()
  "foo_bar => FOO_BAR => FooBar => fooBar => foo_bar"
  (interactive)
  (insert (string-inflection-all-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-toggle ()
  "toggle foo_bar <=> FooBar"
  (interactive)
  (insert (string-inflection-toggle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-camelcase ()
  "FooBar format"
  (interactive)
  (insert (string-inflection-camelcase-function (string-inflection-get-current-word t))))

;;;###autoload
(defun string-inflection-lower-camelcase ()
  "fooBar format"
  (interactive)
  (insert (string-inflection-lower-camelcase-function (string-inflection-get-current-word t))))

;;;###autoload
(defun string-inflection-underscore ()
  "foo_bar format"
  (interactive)
  (insert (string-inflection-underscore-function (string-inflection-get-current-word t))))

;;;###autoload
(defun string-inflection-upcase ()
  "FOO_BAR format"
  (interactive)
  (insert (string-inflection-upcase-function (string-inflection-get-current-word t))))
  
;;;###autoload
(defun string-inflection-lisp ()
  "foo-bar format"
  (interactive)
  (insert (replace-regexp-in-string "_" "-" (string-inflection-underscore-function (string-inflection-get-current-word t)))))

;;--------------------------------------------------------------------------------

(defun string-inflection-get-current-word (&optional skip)
  "Gets the symbol near the cursor.  If SKIP is non-nil, skip non-word characters forward."
  (interactive)
  (and skip
       (skip-chars-forward string-inflection-non-word-chars))
  (let ((start (progn
                 (skip-chars-forward string-inflection-word-chars)
                 (point)))
        (end (progn
               (skip-chars-backward string-inflection-word-chars)
               (point))))
    (prog1
        (buffer-substring start end)
      (delete-region start end))))

(defun string-inflection-camelcase-function (str)
  "foo_bar => FooBar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") ""))

(fset 'string-inflection-camelize-function 'string-inflection-camelcase-function)

(defun string-inflection-lower-camelcase-function (str)
  "foo_bar => fooBar"
  (setq str (split-string (string-inflection-underscore-function str) "_"))
  (concat (downcase (car str))
          (mapconcat 'capitalize (cdr str) "")))

(fset 'string-inflection-lower-camelize-function 'string-inflection-lower-camelcase-function)

(defun string-inflection-upcase-function (str)
  "FooBar => FOO_BAR"
  (upcase (string-inflection-underscore-function str)))

(defun string-inflection-underscore-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun string-inflection-all-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => fooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-camelcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-lower-camelcase-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-ruby-style-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-camelcase-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-java-style-cycle-function (str)
  "fooBar => FOO_BAR => FooBar => fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-camelcase-function str))
   (t
    (string-inflection-lower-camelcase-function str))))

;; Toggle function. But cycle function.
(defun string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> FooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-camelcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-lower-camelcase-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-underscore-p (str)
  "if foo_bar => t"
  (let ((case-fold-search nil))
    (string-match "\\`[a-z0-9_]+\\'" str)))

(defun string-inflection-camelcase-p (str)
  "if FooBar => t"
  (not (or
        (string-inflection-upcase-p str)
        (string-inflection-underscore-p str)
        (string-inflection-lower-camelcase-p str))))

(defun string-inflection-upcase-p (str)
  "if FOO_BAR => t"
  (let ((case-fold-search nil))
    (string-match "\\`[A-Z0-9_]+\\'" str)))

(defun string-inflection-lower-camelcase-p (str)
  "if fooBar => t"
  (let ((case-fold-search nil))
    (string-match "\\`[a-z][a-zA-Z0-9]+\\'" str)))

(provide 'string-inflection)
;;; string-inflection.el ends here
