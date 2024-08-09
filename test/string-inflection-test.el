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

(ert-deftest test-elixir-style-cycle ()
  (should (equal "foo_bar" (string-inflection-elixir-style-cycle-function "FOO_BAR")))
  (should (equal "FooBar" (string-inflection-elixir-style-cycle-function "foo_bar")))
  (should (equal "foo_bar" (string-inflection-elixir-style-cycle-function "FooBar"))))

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
  (should (equal "ph_pand_xm_ltoo" (string-inflection-underscore-function "PHPandXMLtoo")))
  (should (equal "eĥo_ŝanĝo_ĉiu_ĵaŭde" (string-inflection-underscore-function "EĤOŜanĝoĈIUĴaŭde"))))

(ert-deftest test-pascal-case ()
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "Foo1Bar")))
  (should (equal "Eĥo1Ŝanĝo" (string-inflection-pascal-case-function "Eĥo1Ŝanĝo")))
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "FOO1_BAR")))
  (should (equal "Eĥo1Ŝanĝo" (string-inflection-pascal-case-function "EĤO1_ŜANĜO")))
  (should (equal "Foo1Bar" (string-inflection-pascal-case-function "FOO1__BAR")))
  (should (equal "Eĥo1Ŝanĝo" (string-inflection-pascal-case-function "EĤO1__ŜANĜO")))
  (should (equal "Foo" (string-inflection-pascal-case-function "foo")))
  (should (equal "Eĥo" (string-inflection-pascal-case-function "eĥo"))))

(ert-deftest test-lower-camelcase ()
  (should (equal "fooBar" (string-inflection-camelcase-function "FooBar")))
  (should (equal "eĥoŜanĝo" (string-inflection-camelcase-function "EĥoŜanĝo")))
  (should (equal "foo1Bar" (string-inflection-camelcase-function "FOO1BAR")))
  (should (equal "eĥo1Ŝanĝo" (string-inflection-camelcase-function "EĤO1ŜANĜO"))))

(ert-deftest test-kebab-case ()
  (should (equal "foo-bar" (string-inflection-kebab-case-function "FooBar")))
  (should (equal "eĥo-ŝanĝo" (string-inflection-kebab-case-function "EĥoŜanĝo"))))

(ert-deftest test-toggle ()
  (should (equal "FooBar" (string-inflection-toggle-function "foo_bar")))
  (should (equal "EĥoŜanĝo" (string-inflection-toggle-function "eĥo_ŝanĝo")))
  (should (equal "fooBar" (string-inflection-toggle-function "FooBar")))
  (should (equal "eĥoŜanĝo" (string-inflection-toggle-function "EĥoŜanĝo")))
  (should (equal "ĉirkaŭIro" (string-inflection-toggle-function "ĈirkaŭIro")))
  (should (equal "foo_bar" (string-inflection-toggle-function "FOO_BAR")))
  (should (equal "eĥo_ŝanĝo" (string-inflection-toggle-function "EĤO_ŜANĜO"))))

(ert-deftest test-upcase ()
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "foo1_bar")))
  (should (equal "EĤO1_ŜANĜO" (string-inflection-upcase-function "eĥo1_ŝanĝo")))
  (should (equal "FOO1_BAR" (string-inflection-upcase-function "Foo1Bar")))
  (should (equal "EĤO1_ŜANĜO" (string-inflection-upcase-function "Eĥo1Ŝanĝo")))
  (should (equal "FOO_BAR" (string-inflection-upcase-function "Foo_bar")))
  (should (equal "EĤO_ŜANĜO" (string-inflection-upcase-function "Eĥo_ŝanĝo"))))

(ert-deftest test-capital-underscore ()
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "foo1_bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-underscore-function "eĥo1_ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "FOO1_BAR")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-underscore-function "EĤO1_ŜANĜO")))
  (should (equal "Foo1bar" (string-inflection-capital-underscore-function "foo1bar")))
  (should (equal "Eĥo1ŝanĝo" (string-inflection-capital-underscore-function "eĥo1ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "foo1__bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-underscore-function "eĥo1__ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "Foo1Bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-underscore-function "Eĥo1Ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-underscore-function "FOO1Bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-underscore-function "EĤO1Ŝanĝo")))
  (should (equal "Foo_Bar" (string-inflection-capital-underscore-function "FOO_BAR")))
  (should (equal "Eĥo_Ŝanĝo" (string-inflection-capital-underscore-function "EĤO_ŜANĜO"))))

;; --------------------------------------------------------------------------------

(ert-deftest test-word-p ()
  (should (string-inflection-word-p "foo"))
  (should (string-inflection-word-p "eĥo"))
  (should-not (string-inflection-word-p "foo_bar"))
  (should-not (string-inflection-word-p "eĥo_ŝanĝo")))

(ert-deftest test-underscore-p ()
  (should (string-inflection-underscore-p "foo"))
  (should (string-inflection-underscore-p "eĥo"))
  (should (string-inflection-underscore-p "foo_bar"))
  (should (string-inflection-underscore-p "eĥo_ŝanĝo")))

(ert-deftest test-pascal-case-p ()
  (should (string-inflection-pascal-case-p "Foo"))
  (should (string-inflection-pascal-case-p "Eĥo"))
  (should (string-inflection-pascal-case-p "FooBar"))
  (should (string-inflection-pascal-case-p "EĥoŜanĝo"))
  (should-not (string-inflection-pascal-case-p "FOO"))
  (should (string-inflection-pascal-case-p "Eĥĥ")))

(ert-deftest test-camelcase-p ()
  (should-not (string-inflection-camelcase-p "foo"))
  (should-not (string-inflection-camelcase-p "eĥo"))
  (should (string-inflection-camelcase-p "fooBar"))
  (should (string-inflection-camelcase-p "eĥoŜanĝo")))

(ert-deftest test-lower-upcase-p ()
  (should (string-inflection-upcase-p "FOO"))
  (should (string-inflection-upcase-p "EĤO"))
  (should (string-inflection-upcase-p "FOO_BAR"))
  (should (string-inflection-upcase-p "EĤO_ŜANĜO")))

(ert-deftest test-kebab-case-p ()
  (should (string-inflection-kebab-case-p "foo-bar"))
  (should (string-inflection-kebab-case-p "eĥo-ŝanĝo")))

(ert-deftest test-capital-underscore-p ()
  (should (string-inflection-capital-underscore-p "Foo_Bar"))
  (should (string-inflection-capital-underscore-p "Eĥo_Ŝanĝo")))

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
  (should (equal "eĥo"  (buffer-try "eĥo"      '(point-max))))
  (should (equal "foo"  (buffer-try "foo"      '(point-min))))
  (should (equal "eĥo"  (buffer-try "eĥo"      '(point-min))))
  (should (equal ""     (buffer-try ""         '(point-max))))
  (should (equal "foo"  (buffer-try "foo->bar" '(point-min))))
  (should (equal "eĥo"  (buffer-try "eĥo->ŝanĝo" '(point-min))))
  (should (equal "foo-" (buffer-try "foo-"     '(point-min))))
  (should (equal "eĥo-" (buffer-try "eĥo-"     '(point-min))))
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
  (should (equal "foo:bar"  (region-try "foo:bar")))  ; It was underscore when old version.
  (should (equal "foo::bar" (region-try "foo::bar")))  ; It was underscore when old version.
  (should (equal "foo_bar"  (region-try "foo.bar")))
  (should (equal "foo_bar"  (region-try "foo/bar")))

  ;; https://github.com/akicho8/string-inflection/issues/34
  (should (equal "foo_bar"  (region-try ".foo.bar.")))
  (should (equal "::aA:: ::aA::" (region-try "::aA:: ::aA::")))
  (should (equal "aA aA"    (region-try "///aA// //aA//")))

  ;; https://github.com/akicho8/string-inflection/issues/31
  (should (equal " a "      (region-try " a ")))
  (should (equal "a\n"      (region-try "a\n")))
  (should (equal "a\nb\n"   (region-try "a\nb\n")))

  (should (equal "eĥo_ŝanĝo"   (region-try "eĥo_ŝanĝo")))
  )

(defun buffer-try-inflect (str inflect)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (funcall inflect)
    (buffer-string)))

;; https://github.com/akicho8/string-inflection/issues/30
(ert-deftest test-buffer-underscore ()
  (should (equal "object_name->method" (buffer-try-inflect "objectName->method" 'string-inflection-underscore)))
  (should (equal "object1_name->method" (buffer-try-inflect "object1Name->method" 'string-inflection-underscore)))
  (should (equal "eĥo_ŝanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥoŜanĝo->ĉiuĴaŭde" 'string-inflection-underscore))))

(ert-deftest test-buffer-camelcase ()
  (should (equal "ObjectName->method" (buffer-try-inflect "object_name->method" 'string-inflection-camelcase)))
  (should (equal "Object1Name->method" (buffer-try-inflect "object1_name->method" 'string-inflection-camelcase)))
  (should (equal "EĥoŜanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥo_ŝanĝo->ĉiuĴaŭde" 'string-inflection-camelcase))))

(ert-deftest test-buffer-lower-camelcase ()
  (should (equal "objectName->method" (buffer-try-inflect "object_name->method" 'string-inflection-lower-camelcase)))
  (should (equal "object1Name->method" (buffer-try-inflect "object1_name->method" 'string-inflection-lower-camelcase)))
  (should (equal "eĥoŜanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥo_ŝanĝo->ĉiuĴaŭde" 'string-inflection-lower-camelcase))))


(ert-run-tests-batch t)
