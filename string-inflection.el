;;; string-inflection.el --- foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar conversion of names -*- lexical-binding: t -*-

;; Copyright (C) 2004-2025 Free Software Foundation, Inc.

;; Author: akicho8 <akicho8@gmail.com>
;; Keywords: elisp
;; Version: 1.2.1

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
;; You can configure where the point should end up after the inflection using the
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
  "Change the casing of symbols."
  :group 'convenience)

(defcustom string-inflection-final-position 'remain
  "Where to finish after the inflection.
This can be `remain' – remain at the initial position but not beyond the end of the inflected string –,
`beginning' – jump to the beginning of the inflection – or
`end' – jump to the end of the inflection."
  :group 'string-inflection
  :type '(choice (const remain) (const beginning) (const end)))

(defcustom string-inflection-bounds-function (lambda () (bounds-of-thing-at-point 'symbol))
  "The function used select strings for inflections.

It should take no arguments and return a cons cell, the car of which should be
the point in the current buffer of the beginning of the string, and the cdr the
point in the current buffer of the end of the string."
  :group 'string-inflection
  :type 'function)

(defun string-inflection-bounds-of-mode-independent-chunk ()
  "In the old specification, it does not depend on mode, but rather uses a chunk like a variable name as a symbol.

Use it like this:
(setq string-inflection-bounds-function 'string-inflection-bounds-of-mode-independent-chunk)"
  (cons
   (progn (skip-chars-forward "a-zA-Z0-9_-")
          (skip-chars-backward "_-")
          (point))
   (progn (skip-chars-backward "a-zA-Z0-9_-")
          (skip-chars-forward "_-")
          (point))))

(defcustom string-inflection-region-selection-behavior 'replace-all-spaces-with-underscores
  "Behavior applied when a region is selected.
- `replace-all-spaces-with-underscores` : Replace consecutive whitespace with a single underscore.
- `apply-to-each-symbols`               : Apply conversion to each symbol in the region (no automatic underscore replacement)."
  :type '(choice (const :tag "Convert whitespace to underscores" replace-all-spaces-with-underscores)
                 (const :tag "Apply conversion to each symbol" apply-to-each-symbols))
  :group 'string-inflection)

;; --------------------------------------------------------------------------------

;;;###autoload
(defun string-inflection-ruby-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-ruby-style-cycle-function))

(fset 'string-inflection-cycle 'string-inflection-ruby-style-cycle)

;;;###autoload
(defun string-inflection-elixir-style-cycle ()
  "foo_bar => FooBar => foo_bar"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-elixir-style-cycle-function))

;;;###autoload
(defun string-inflection-python-style-cycle ()
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-python-style-cycle-function))

;;;###autoload
(defun string-inflection-java-style-cycle ()
  "fooBar => FOO_BAR => FooBar => fooBar"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-java-style-cycle-function))

;;;###autoload
(defun string-inflection-all-cycle ()
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar

At first glance, this method may seem convenient, but in reality, it was created solely for testing purposes during development. Its role is to verify that all transformation patterns are properly traversed, and it was never intended for regular use. In fact, the developers themselves do not use it at all and strongly discourage its use.

Using this method in practice leads to unnecessary stress, as it forces traversal through every possible transformation pattern, making the conversion process excessively long. Moreover, under the current default settings, the method may not even complete a full cycle depending on the mode in use.

For example, in modes where hyphens are not considered part of variable names, a string like `foo-bar' will be split into `foo' and `bar', and the transformation will only proceed on `bar'. This means the intended conversion process can be interrupted midway depending on the context.

For these reasons, this method should not be used as part of your regular workflow. It is strictly meant for internal verification during development, and we advise against using it in day-to-day usage."
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-all-cycle-function))

;;;###autoload
(defun string-inflection-toggle ()
  "toggle foo_bar <=> FooBar"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-toggle-function))

;;;###autoload
(defun string-inflection-camel-case ()
  "FooBar format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-pascal-case-function))

;;;###autoload
(defun string-inflection-lower-camel-case ()
  "fooBar format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-camel-case-function))

;;;###autoload
(defun string-inflection-snake-case ()
  "foo_bar format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-snake-case-function))

;;;###autoload
(defun string-inflection-capital-snake-case ()
  "Foo_Bar format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-capital-snake-case-function))

;;;###autoload
(defun string-inflection-upcase ()
  "FOO_BAR format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-upcase-function))

;;;###autoload
(defun string-inflection-kebab-case ()
  "foo-bar format"
  (interactive)
  (string-inflection--symbol-or-region #'string-inflection-kebab-case-function))

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

(defun string-inflection-replace-all-spaces-with-underscores (start end)
  "Replace all whitespace characters in the region with underscores."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "[[:space:]]+" nil t)
        (replace-match "_"))))
  (deactivate-mark))

(defun string-inflection--symbol-or-region (inflect-func)
  "Perform INFLECT-FUNC depending on if in region or symbol."
  (if (use-region-p)
      (if (and (eq string-inflection-region-selection-behavior 'replace-all-spaces-with-underscores))
          (string-inflection-replace-all-spaces-with-underscores (region-beginning) (region-end))
        (string-inflection--region inflect-func))
    (string-inflection--symbol inflect-func)))

(defun string-inflection--symbol (inflect-func)
  "Perform INFLECT-FUNC for a  symbol occurrence."
  (let ((orig-point (point)))
    (insert (funcall inflect-func (string-inflection-get-current-symbol)))
    (pcase string-inflection-final-position
      ('remain (goto-char (min orig-point (cdr (funcall string-inflection-bounds-function)))))
      ('beginning (goto-char (car (funcall string-inflection-bounds-function)))))))

(defun string-inflection--region (inflect-func)
  "Perform INFLECT-FUNC for all occurrences in the region."
  (let ((orig-point (point))
        (start (region-beginning))
        (end (region-end)))
    (dotimes (_ (string-inflection--count-symbols-between-start-and-end start end))
      (let ((orig-length (length (symbol-name (symbol-at-point)))))
        (insert (funcall inflect-func (string-inflection-get-current-symbol-limited-by start end)))
        (setq end (+ end (- (length (symbol-name (symbol-at-point))) orig-length)))
        (forward-symbol 1)
        (if-let* ((bounds (funcall string-inflection-bounds-function)))
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

(defun string-inflection-get-current-symbol ()
  "Gets the symbol near the point"
  (interactive)
  (if-let* ((bounds (funcall string-inflection-bounds-function))
            (start (car bounds))
            (end (cdr bounds))
            (str (buffer-substring start end)))
      (progn
        (delete-region start end)
        str)
    ""))

(defun string-inflection-get-current-symbol-limited-by (reg-start reg-end)
  "Gets the symbol near the point limited by REG-START and REG-END."
  (interactive)
  (if-let* ((bounds (funcall string-inflection-bounds-function))
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
  (setq str (string-inflection-snake-case-function str))
  (mapconcat 'capitalize (split-string str "_") ""))

(fset 'string-inflection-upper-camel-case-function 'string-inflection-pascal-case-function)

(defun string-inflection-camel-case-function (str)
  "foo_bar => fooBar"
  (setq str (split-string (string-inflection-snake-case-function str) "_"))
  (concat (downcase (car str))
          (mapconcat 'capitalize (cdr str) "")))

(fset 'string-inflection-lower-camel-case-function 'string-inflection-camel-case-function)

(defun string-inflection-upcase-function (str)
  "FooBar => FOO_BAR"
  (upcase (string-inflection-snake-case-function str)))

(defun string-inflection-snake-case-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([[:lower:][:digit:]]\\)\\([[:upper:]]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([[:upper:]]+\\)\\([[:upper:]][[:lower:]]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun string-inflection-capital-snake-case-function (str)
  "foo_bar => Foo_Bar"
  (setq str (string-inflection-snake-case-function str))
  (mapconcat 'capitalize (split-string str "_") "_"))

(defun string-inflection-kebab-case-function (str)
  "foo_bar => foo-bar"
  (let ((case-fold-search nil))
    (setq str (string-inflection-snake-case-function str))
    (setq str (replace-regexp-in-string "_" "-" str))))

(defun string-inflection-all-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar
   foo     => FOO     => Foo    => foo"
  (cond
   ;; foo => FOO
   ((string-inflection-symbol-p str)
    (string-inflection-upcase-function str))
   ;; foo_bar => FOO_BAR
   ((string-inflection-snake-case-p str)
    (string-inflection-upcase-function str))
   ;; FOO_BAR => FooBar
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   ;; FooBar => fooBar
   ;; Foo    => foo
   ((string-inflection-pascal-case-p str)
    (string-inflection-camel-case-function str))
   ;; fooBar => foo-bar
   ((string-inflection-camel-case-p str)
    (string-inflection-kebab-case-function str))
   ;; foo-bar => Foo_Bar
   ((string-inflection-kebab-case-p str)
    (string-inflection-capital-snake-case-function str))
   ;; foo-bar => foo_bar
   (t
    (string-inflection-snake-case-function str))))

(defun string-inflection-ruby-style-cycle-function (str)
  "foo_bar => FOO_BAR => FooBar => foo_bar"
  (cond
   ((string-inflection-snake-case-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-snake-case-function str))))

(defalias 'string-inflection-python-style-cycle-function
  'string-inflection-ruby-style-cycle-function)

(defun string-inflection-elixir-style-cycle-function (str)
  "foo_bar => FooBar => foo_bar"
  (cond
   ((string-inflection-snake-case-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-snake-case-function str))))

(defun string-inflection-java-style-cycle-function (str)
  "fooBar => FOO_BAR => FooBar => fooBar"
  (cond
   ((string-inflection-snake-case-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-camel-case-p str)
    (string-inflection-upcase-function str))
   ((string-inflection-upcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-camel-case-function str))))

;; Toggle function. But cycle function.
(defun string-inflection-toggle-function (str)
  "Not so much the case that in all caps when using normal foo_bar <--> FooBar"
  (cond
   ((string-inflection-snake-case-p str)
    (string-inflection-pascal-case-function str))
   ((string-inflection-pascal-case-p str)
    (string-inflection-camel-case-function str))
   (t
    (string-inflection-snake-case-function str))))

;; --------------------------------------------------------------------------------

(defun string-inflection-symbol-p (str)
  "if foo => t"
  (let ((case-fold-search nil))
    (string-match "\\`[[:lower:][:digit:]]+\\'" str)))

(defun string-inflection-snake-case-p (str)
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

(fset 'string-inflection-upper-camel-case-p 'string-inflection-pascal-case-p)

(defun string-inflection-camel-case-p (str)
  "if fooBar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "[[:upper:]]" str)
     (string-match "\\`[[:lower:]][[:lower:][:upper:][:digit:]]+\\'" str))))

(fset 'string-inflection-lower-camel-case-p 'string-inflection-camel-case-p)

(defun string-inflection-kebab-case-p (str)
  "if foo-bar => t"
  (string-match "-" str))

(defun string-inflection-capital-snake-case-p (str)
  "if Foo_Bar => t"
  (let ((case-fold-search nil))
    (and
     (string-match "_" str)
     (string-match "\\`[[:upper:]][[:lower:][:upper:][:digit:]_]+\\'" str))))

;; --------------------------------------------------------------------------------

;; The function names on the left are deprecated and retained for compatibility, while those on the right are the new ones.
;; I'll erase it soon
(fset 'string-inflection-camelcase 'string-inflection-camel-case)
(fset 'string-inflection-camelcase-function 'string-inflection-camel-case-function)
(fset 'string-inflection-camelcase-p 'string-inflection-camel-case-p)
(fset 'string-inflection-capital-underscore-function 'string-inflection-capital-snake-case-function)
(fset 'string-inflection-lower-camelcase 'string-inflection-lower-camel-case)
(fset 'string-inflection-lower-camelcase-function 'string-inflection-lower-camel-case-function)
(fset 'string-inflection-lower-camelcase-p 'string-inflection-lower-camel-case-p)
(fset 'string-inflection-underscore 'string-inflection-snake-case)
(fset 'string-inflection-underscore-function 'string-inflection-snake-case-function)
(fset 'string-inflection-underscore-p 'string-inflection-snake-case-p)
(fset 'string-inflection-upper-camelcase 'string-inflection-upper-camel-case)
(fset 'string-inflection-upper-camelcase-function 'string-inflection-upper-camel-case-function)
(fset 'string-inflection-upper-camelcase-p 'string-inflection-upper-camel-case-p)
(fset 'string-inflection-word-p 'string-inflection-symbol-p)

(provide 'string-inflection)
;;; string-inflection.el ends here
