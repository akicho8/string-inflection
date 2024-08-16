;;; string-inflection.el --- underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names -*- lexical-binding: t -*-

;; Copyright (C) 2004,2014,2016,2017,2018,2020,2021,2022,2023,2024 Free Software Foundation, Inc.

;; Author: akicho8 <akicho8@gmail.com>
;; Keywords: elisp
;; Version: 1.1.0

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

;; There are three main functions:
;;
;;   1. For Ruby   -> string-inflection-ruby-style-cycle   (foo_bar => FOO_BAR => FooBar => foo_bar)
;;   2. For Elixir -> string-inflection-elixir-style-cycle (foo_bar => FooBar => foo_bar)
;;   3. For Python -> string-inflection-python-style-cycle (foo_bar => FOO_BAR => FooBar => foo_bar)
;;   4. For Java   -> string-inflection-java-style-cycle   (fooBar  => FOO_BAR => FooBar => fooBar)
;;   5. For All    -> string-inflection-all-cycle          (foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar)
;;
;;
;; Example 1:
;;
;;   (require 'string-inflection)
;;   (global-unset-key (kbd "C-q"))
;;   ;; C-q C-u is the key bindings similar to Vz Editor.
;;   (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)
;;
;;   (defun my-string-inflection-cycle-auto ()
;;     "switching by major-mode"
;;     (interactive)
;;     (cond
;;      ;; for emacs-lisp-mode
;;      ((eq major-mode 'emacs-lisp-mode)
;;       (string-inflection-all-cycle))
;;      ;; for java
;;      ((eq major-mode 'java-mode)
;;       (string-inflection-java-style-cycle))
;;      ;; for python
;;      ((eq major-mode 'python-mode)
;;       (string-inflection-python-style-cycle))
;;      ;; for elixir
;;      ((eq major-mode 'elixir-mode)
;;       (string-inflection-elixir-style-cycle))
;;      (t
;;       ;; default
;;       (string-inflection-ruby-style-cycle))))
;;
;;
;; Example 2:
;;
;;   (require 'string-inflection)
;;
;;   ;; default
;;   (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)
;;
;;   ;; for ruby
;;   (add-hook 'ruby-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
;;
;;   ;; for elixir
;;   (add-hook 'elixir-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-elixir-style-cycle)))
;;
;;   ;; for python
;;   (add-hook 'python-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle)))
;;
;;   ;; for java
;;   (add-hook 'java-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))
;;
;; You can configure where the cursor should end up after the inflection using the
;; `string-inflection-final-position' option.
;;
;; When a region is active during the inflect operation there are two effects:
;;
;; * If the region marks a part of a symbol the operation is only performed on that
;;   part.
;; * If the region contains more than one symbols, the operation is performed on all
;;   the symbols in the region.
;; * The region is preserved after the operation.

;;; Code:

(defgroup string-inflection nil
  "Change the casing of words."
  :group 'convenience)

(defcustom string-inflection-final-position 'remain
  "Where to finish after the inflection.
This can be `remain' – remain at the initial position but not beyond the end of the inflected string –,
`beginning' – jump to the beginning of the inflection – or
`end' – jump to the end of the inflection."
  :group 'string-inflection
  :type '(choice (const remain) (const beginning) (const end)))

;; --------------------------------------------------------------------------------

;;;###autoload
(defun string-inflection-ruby-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-ruby-style-cycle-function))

(fset 'string-inflection-cycle 'string-inflection-ruby-style-cycle)

;;;###autoload
(defun string-inflection-elixir-style-cycle ()
  "foo_bar => FooBar => foo_bar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-elixir-style-cycle-function))

;;;###autoload
(defun string-inflection-python-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-python-style-cycle-function))

;;;###autoload
(defun string-inflection-java-style-cycle ()
  "fooBar => FOO_BAR => FooBar => fooBar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-java-style-cycle-function))

;;;###autoload
(defun string-inflection-all-cycle ()
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-all-cycle-function))

;;;###autoload
(defun string-inflection-toggle ()
  "toggle foo_bar <=> FooBar"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-toggle-function))

;;;###autoload
(defun string-inflection-camelcase ()
  "FooBar format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-pascal-case-function))

;;;###autoload
(defun string-inflection-lower-camelcase ()
  "fooBar format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-camelcase-function))

;;;###autoload
(defun string-inflection-underscore ()
  "foo_bar format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-underscore-function))

;;;###autoload
(defun string-inflection-capital-underscore ()
  "Foo_Bar format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-capital-underscore-function))

;;;###autoload
(defun string-inflection-upcase ()
  "FOO_BAR format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-upcase-function))

;;;###autoload
(defun string-inflection-kebab-case ()
  "foo-bar format"
  (interactive)
  (string-inflection--single-or-region #'string-inflection-kebab-case-function))

(fset 'string-inflection-lisp 'string-inflection-kebab-case)

;; --------------------------------------------------------------------------------

(defun string-inflection--count-symbols-between-start-and-end (start end)
  "Count the symbols between START and END."
  (let ((symbol-num 0))
    (goto-char start)
    (save-excursion
      (while (< (point) end)
        (setq symbol-num (1+ symbol-num))
        (forward-symbol 1)))
    symbol-num))

(defun string-inflection--single-or-region (inflect-func)
  "Perform INFLECT-FUNC depending on if in region or single."
  (if (use-region-p)
      (string-inflection--region inflect-func)
    (string-inflection--single inflect-func)))

(defun string-inflection--single (inflect-func)
  "Perform INFLECT-FUNC for a  single occurrence."
  (let ((orig-point (point)))
    (insert (funcall inflect-func (string-inflection-get-current-word)))
    (pcase string-inflection-final-position
      ('remain (goto-char (min orig-point (cdr (bounds-of-thing-at-point 'symbol)))))
      ('beginning (goto-char (car (bounds-of-thing-at-point 'symbol)))))))

(defun string-inflection--region (inflect-func)
  "Perform INFLECT-FUNC for all occurrences in the region."
  (let ((orig-point (point))
        (start (region-beginning))
        (end (region-end)))
    (dotimes (_ (string-inflection--count-symbols-between-start-and-end start end))
      (let ((orig-length (length (symbol-name (symbol-at-point)))))
        (insert (funcall inflect-func (string-inflection-get-current-word-limited-by start end)))
        (setq end (+ end (- (length (symbol-name (symbol-at-point))) orig-length)))
        (forward-symbol 1)
        (if-let* ((bounds (bounds-of-thing-at-point 'symbol)))
            (goto-char (car bounds)))))
    (let ((new-region
           (pcase string-inflection-final-position
             ('remain (if (/= orig-point start) (cons start end) (cons end start)))
             ('beginning (cons end start))
             ('end (cons start end)))))
      (set-mark (car new-region))
      (goto-char (cdr new-region)))
    (activate-mark)
    (setq deactivate-mark nil)))

(defun string-inflection-get-current-word ()
  "Gets the symbol near the cursor"
  (interactive)
  (if-let* ((bounds (bounds-of-thing-at-point 'symbol))
            (start (car bounds))
            (end (cdr bounds))
            (str (buffer-substring start end)))
      (progn
        (delete-region start end)
        str)
    ""))

(defun string-inflection-get-current-word-limited-by (reg-start reg-end)
  "Gets the symbol near the cursor limited by REG-START and REG-END."
  (interactive)
  (if-let* ((bounds (bounds-of-thing-at-point 'symbol))
            (start (max (car bounds) reg-start))
            (end (min (cdr bounds) reg-end))
            (str (buffer-substring start end)))
      (progn
        (delete-region start end)
        str)
    ""))

;; --------------------------------------------------------------------------------

(defun string-inflection-pascal-case-function (str)
  "foo_bar => FooBar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") ""))

(fset 'string-inflection-upper-camelcase-function 'string-inflection-pascal-case-function)

(defun string-inflection-camelcase-function (str)
  "foo_bar => fooBar"
  (setq str (split-string (string-inflection-underscore-function str) "_"))
  (concat (downcase (car str))
          (mapconcat 'capitalize (cdr str) "")))

(fset 'string-inflection-lower-camelcase-function 'string-inflection-camelcase-function)

(defun string-inflection-upcase-function (str)
  "FooBar => FOO_BAR"
  (upcase (string-inflection-underscore-function str)))

(defun string-inflection-underscore-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([[:lower:][:digit:]]\\)\\([[:upper:]]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([[:upper:]]+\\)\\([[:upper:]][[:lower:]]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun string-inflection-capital-underscore-function (str)
  "foo_bar => Foo_Bar"
  (setq str (string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") "_"))

(defun string-inflection-kebab-case-function (str)
  "foo_bar => foo-bar"
  (let ((case-fold-search nil))
    (setq str (string-inflection-underscore-function str))
    (setq str (replace-regexp-in-string "_" "-" str))))

(defun string-inflection-all-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar
   foo     => FOO     => Foo    => foo"
  (cond
   ;; foo => FOO
   ((string-inflection-word-p str)
    (string-inflection-upcase-function str))
   ;; foo_bar => FOO_BAR
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ;; FOO_BAR => FooBar
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   ;; FooBar => fooBar
   ;; Foo    => foo
   ((string-inflection-pascal-case-p str)
    (string-inflection-camelcase-function str))
   ;; fooBar => foo-bar
   ((string-inflection-camelcase-p str)
    (string-inflection-kebab-case-function str))
   ;; foo-bar => Foo_Bar
   ((string-inflection-kebab-case-p str)
    (string-inflection-capital-underscore-function str))
   ;; foo-bar => foo_bar
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-ruby-style-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-underscore-function str))))

(defalias 'string-inflection-python-style-cycle-function
  'string-inflection-ruby-style-cycle-function)

(defun string-inflection-elixir-style-cycle-function (str)
  "foo_bar => FooBar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-underscore-function str))))

(defun string-inflection-java-style-cycle-function (str)
  "fooBar => FOO_BAR => FooBar => fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-camelcase-function str))))

;; Toggle function. But cycle function.
(defun string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> FooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-pascal-case-function str))
   ((string-inflection-pascal-case-p str)
    (string-inflection-camelcase-function str))
   (t
    (string-inflection-underscore-function str))))

;; --------------------------------------------------------------------------------

(defun string-inflection-word-p (str)
  "if foo => t"
  (let ((case-fold-search nil))
    (string-match "\\`[[:lower:][:digit:]]+\\'" str)))

(defun string-inflection-underscore-p (str)
  "if foo_bar => t"
  (let ((case-fold-search nil))
    (string-match "\\`[[:lower:][:digit:]_]+\\'" str)))

(defun string-inflection-upcase-p (str)
  "if FOO_BAR => t"
  (let ((case-fold-search nil))
    (string-match "\\`[[:upper:][:digit:]_]+\\'" str)))

(defun string-inflection-pascal-case-p (str)
  "if FooBar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[[:lower:]]" str)
     (string-match "\\`[[:upper:]][[:lower:][:upper:][:digit:]]+\\'" str))))

(fset 'string-inflection-upper-camelcase-p 'string-inflection-pascal-case-p)

(defun string-inflection-camelcase-p (str)
  "if fooBar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[[:upper:]]" str)
     (string-match "\\`[[:lower:]][[:lower:][:upper:][:digit:]]+\\'" str))))

(fset 'string-inflection-lower-camelcase-p 'string-inflection-camelcase-p)

(defun string-inflection-kebab-case-p (str)
  "if foo-bar => t"
  (string-match "-" str))

(defun string-inflection-capital-underscore-p (str)
  "if Foo_Bar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "_" str)
     (string-match "\\`[[:upper:]][[:lower:][:upper:][:digit:]_]+\\'" str))))

(provide 'string-inflection)
;;; string-inflection.el ends here
