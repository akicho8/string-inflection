;; -*- compile-command: "emacs --script string-inflection-test.el" -*-

(setq load-path (cons "." load-path))
(require 'string-inflection)

(require 'ert)

(ert-deftest test-underscore ()
  (should (equal "foo1_bar" (string-inflection-underscore-function "foo1_bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "FOO1_BAR")))
  (should (equal "foo1bar" (string-inflection-underscore-function "foo1bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "foo1__bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "Foo1Bar")))
  (should (equal "foo1_bar" (string-inflection-underscore-function "FOO1Bar")))
  (should (equal "foo_bar" (string-inflection-underscore-function "FOO_BAR"))))

(ert-deftest test-camelize ()
  (should (equal "Foo1Bar" (string-inflection-camelize-function "Foo1Bar")))
  (should (equal "Foo1Bar" (string-inflection-camelize-function "FOO1_BAR")))
  (should (equal "Foo1Bar" (string-inflection-camelize-function "FOO1__BAR")))
  (should (equal "Foo" (string-inflection-camelize-function "foo"))))

(ert-deftest test-camelize-p ()
  (should (equal nil (string-inflection-camelize-p "foo1_bar")))
  (should (equal t (string-inflection-camelize-p "FooBar"))))

(ert-deftest test-cycle ()
  (should (equal "FooBar" (string-inflection-cycle-function "FOO_BAR")))
  (should (equal "FOO_BAR" (string-inflection-cycle-function "foo_bar")))
  (should (equal "foo_bar" (string-inflection-cycle-function "FooBar"))))

(ert-deftest test-toggle ()
  (should (equal "FooBar" (string-inflection-toggle-function "foo_bar")))
  (should (equal "foo_bar" (string-inflection-toggle-function "FooBar"))))

(ert-deftest test-upcase ()
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "foo1_bar")))
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "Foo1Bar")))
  (should (equal "FOO_BAR" (string-inflection-upcase-function "Foo_bar"))))

(ert-run-tests-batch t)
