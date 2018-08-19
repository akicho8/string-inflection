;; -*- compile-command: "emacs --script string-inflection-test.el" -*-

(setq load-path (cons ".." load-path))
(setq load-path (cons "." load-path))

(require 'string-inflection)

(require 'ert)

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

(ert-run-tests-batch t)
