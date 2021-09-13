;; -*- compile-command: "emacs --script string-inflection-test.el" -*-

(setq load-path (cons ".." load-path))
(setq load-path (cons "." load-path))

(require 'string-inflection)

(require 'ert)

;; -------------------------------------------------------------------------------- cycle function

(ert-deftest test-ruby-style-cycle ()
  (should (equal "FOO_BAR" (string-inflection-ruby-style-cycle-function "foo_bar")))
  (should (equal "FooBar" (string-inflection-ruby-style-cycle-function "FOO_BAR")))
  (should (equal "foo_bar" (string-inflection-ruby-style-cycle-function "FooBar"))))

(ert-deftest test-java-style-cycle ()
  (should (equal "FOO_BAR" (string-inflection-java-style-cycle-function "foo_bar")))
  (should (equal "FOO_BAR" (string-inflection-java-style-cycle-function "fooBar")))
  (should (equal "FooBar" (string-inflection-java-style-cycle-function "FOO_BAR")))
  (should (equal "fooBar" (string-inflection-java-style-cycle-function "FooBar"))))

(ert-deftest test-all-cycle ()
  (should (equal "FOO_BAR" (string-inflection-all-cycle-function "foo_bar")))
  (should (equal "FooBar" (string-inflection-all-cycle-function "FOO_BAR")))
  (should (equal "fooBar" (string-inflection-all-cycle-function "FooBar")))
  (should (equal "fooBar" (string-inflection-all-cycle-function "FooBar")))
  (should (equal "foo-bar" (string-inflection-all-cycle-function "fooBar")))
  (should (equal "Foo_Bar" (string-inflection-all-cycle-function "foo-bar")))
  (should (equal "foo_bar" (string-inflection-all-cycle-function "Foo_Bar"))))

; --------------------------------------------------------------------------------

(ert-deftest test-underscore ()
  (should (equal "foo1_bar" (string-inflection-underscore-function "foo1_bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "FOO1_BAR")))
  (should (equal "foo1bar" (string-inflection-underscore-function "foo1bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "foo1__bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "Foo1Bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "FOO1Bar")))
  (should (equal "foo_bar" (string-inflection-underscore-function "FOO_BAR"))))

(ert-deftest test-consecutive-uppercase ()
  (should (equal "a_single_line" (string-inflection-underscore-function "ASingleLine")))
  (should (equal "php_mode" (string-inflection-underscore-function "PHPMode")))
  (should (equal "ends_with_php" (string-inflection-underscore-function "EndsWithPHP")))
  (should (equal "php_and_xml_too" (string-inflection-underscore-function "PHPAndXMLToo")))
  (should (equal "php_and_xml_too" (string-inflection-underscore-function "phpAndXmlToo")))
  (should (equal "ph_pand_xm_ltoo" (string-inflection-underscore-function "PHPandXMLtoo"))))

(ert-deftest test-pascal-case ()
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "Foo1Bar")))
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "FOO1_BAR")))
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "FOO1__BAR")))
  (should (equal "Foo" (string-inflection-pascal-case-function "foo"))))

(ert-deftest test-lower-camelcase ()
  (should (equal "fooBar" (string-inflection-camelcase-function "FooBar")))
  (should (equal "foo1Bar" (string-inflection-camelcase-function "FOO1BAR"))))

(ert-deftest test-kebab-case ()
  (should (equal "foo-bar" (string-inflection-kebab-case-function "FooBar"))))

(ert-deftest test-toggle ()
  (should (equal "FooBar" (string-inflection-toggle-function "foo_bar")))
  (should (equal "fooBar" (string-inflection-toggle-function "FooBar")))
  (should (equal "foo_bar" (string-inflection-toggle-function "FOO_BAR"))))

(ert-deftest test-upcase ()
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "foo1_bar")))
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "Foo1Bar")))
  (should (equal "FOO_BAR" (string-inflection-upcase-function "Foo_bar"))))

(ert-deftest test-capital-underscore ()
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "foo1_bar")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "FOO1_BAR")))
  (should (equal "Foo1bar" (string-inflection-capital-underscore-function "foo1bar")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "foo1__bar")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "Foo1Bar")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "FOO1Bar")))
  (should (equal "Foo_Bar" (string-inflection-capital-underscore-function "FOO_BAR"))))

;; --------------------------------------------------------------------------------

(ert-deftest test-word-p ()
  (should-not (equal nil (string-inflection-word-p "foo")))
  (should (equal nil (string-inflection-word-p "foo_bar"))))

(ert-deftest test-underscore-p ()
  (should-not (equal nil (string-inflection-underscore-p "foo")))
  (should-not (equal nil (string-inflection-underscore-p "foo_bar"))))

(ert-deftest test-pascal-case-p ()
  (should-not (equal nil (string-inflection-pascal-case-p "Foo")))
  (should-not (equal nil (string-inflection-pascal-case-p "FooBar"))))

(ert-deftest test-camelcase-p ()
  (should (equal nil (string-inflection-camelcase-p "foo")))
  (should-not (equal nil (string-inflection-camelcase-p "fooBar"))))

(ert-deftest test-lower-upcase-p ()
  (should-not (equal nil (string-inflection-upcase-p "FOO")))
  (should-not (equal nil (string-inflection-upcase-p "FOO_BAR"))))

(ert-deftest test-kebab-case-p ()
  (should-not (equal nil (string-inflection-kebab-case-p "foo-bar"))))

(ert-deftest test-capital-underscore-p ()
  (should-not (equal nil (string-inflection-capital-underscore-p "Foo_Bar"))))

;; -------------------------------------------------------------------------------- Target word of cursor position

(defun buffer-try (str position)
  (with-current-buffer (get-buffer-create "*test*")
    (insert str)
    (goto-char (apply position))
    (prog1
        (string-inflection-get-current-word)
      (kill-this-buffer))))

(ert-deftest test-get-current-word-on-cursor ()
  (should (equal "foo"  (buffer-try "foo"      '(point-max))))
  (should (equal "foo"  (buffer-try "foo"      '(point-min))))
  (should (equal ""     (buffer-try ""         '(point-max))))
  (should (equal "foo"  (buffer-try "foo->bar" '(point-min))))
  (should (equal "foo-" (buffer-try "foo-"     '(point-min))))
)

;; -------------------------------------------------------------------------------- Target all of region

(defun region-try (str)
  (with-current-buffer (get-buffer-create "*test*")
    (insert str)
    (transient-mark-mode t)
    (beginning-of-buffer)
    (set-mark-command nil)
    (end-of-buffer)
    (prog1
        (string-inflection-get-current-word)
      (kill-this-buffer))))

(ert-deftest test-get-current-word-in-region ()
  (should (equal "foo bar"  (region-try "foo bar"))) ; It was underscore when old version.
  (should (equal "foo_bar"  (region-try "foo_bar")))
  (should (equal "foo_bar"  (region-try "foo:bar")))
  (should (equal "foo_bar"  (region-try "foo::bar")))
  (should (equal "foo_bar"  (region-try "foo.bar")))
  (should (equal "foo_bar"  (region-try "foo/bar")))

  ;; https://github.com/akicho8/string-inflection/issues/34
  (should (equal "foo_bar"  (region-try ":foo:bar:")))
  (should (equal "foo_bar"  (region-try "/foo/bar/")))

  ;; https://github.com/akicho8/string-inflection/issues/31
  (should (equal " a "      (region-try " a ")))
  (should (equal "a\n"      (region-try "a\n")))
  (should (equal "a\nb\n"   (region-try "a\nb\n")))
  )

;; https://github.com/akicho8/string-inflection/issues/30
(ert-deftest test-buffer-underscore ()
  (should (equal "object_name->method"
                 (with-current-buffer (get-buffer-create "*test*")
                   (insert "objectName->method")
                   (goto-char (point-min))
                   (string-inflection-underscore)
                   (prog1
                       (buffer-string)
                     (kill-this-buffer))))))

(ert-run-tests-batch t)
