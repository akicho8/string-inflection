;;; string-inflection.el --- underscore -> UPCASE -> CamelCase conversion of names

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Akira Ikeda <pinpon.ikeda@gmail.com>
;; Keywords: elisp
;; Version: 1.0.1

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
;; (global-set-key (kbd "C-q C-u") 'string-inflection-cycle) ; Vz Editor-like key binding

;;; Code:

(defconst string-inflection-word-chars "a-zA-Z0-9_")

;;--------------------------------------------------------------------------------

;;;###autoload
(defun string-inflection-camelize ()
  "FooBar format"
  (interactive)
  (insert (string-inflection-camelize-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-underscore ()
  "foo_bar format"
  (interactive)
  (insert (string-inflection-underscore-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-upcase ()
  "FOO_BAR format"
  (interactive)
  (insert (string-inflection-upcase-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (insert (string-inflection-cycle-function (string-inflection-get-current-word))))

;;;###autoload
(defun string-inflection-toggle ()
  "toggle foo_bar and FooBar"
  (interactive)
  (insert (string-inflection-toggle-function (string-inflection-get-current-word))))

;;--------------------------------------------------------------------------------

(defun string-inflection-get-current-word ()
  "Gets the symbol near the cursor"
  (interactive)
  (let ((start (progn
                 (skip-chars-forward string-inflection-word-chars)
                 (point)))
        (end (progn
               (skip-chars-backward string-inflection-word-chars)
               (point))))
    (prog1
        (buffer-substring start end)
      (delete-region start end))))

(defun string-inflection-camelize-function (str)
  "foo_bar => FooBar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") ""))

(defun string-inflection-upcase-function (str)
  "FooBar => FOO_BAR"
  (upcase (string-inflection-underscore-function str)))

(defun string-inflection-underscore-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun string-inflection-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-camelize-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> FooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-camelize-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-underscore-p (str)
  "if foo_bar => t"
  (let ((case-fold-search nil))
    (string-match "\\`[a-z0-9_]+\\'" str)))

(defun string-inflection-camelize-p (str)
  "if FooBar => t"
  (not (or
        (string-inflection-upcase-p str)
        (string-inflection-underscore-p str))))

(defun string-inflection-upcase-p (str)
  "if FOO_BAR => t"
  (let ((case-fold-search nil))
    (string-match "\\`[A-Z0-9_]+\\'" str)))

(provide 'string-inflection)
;;; string-inflection.el ends here
