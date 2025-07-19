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

(ert-deftest test-snake-case ()
  (should (equal "foo1_bar" (string-inflection-snake-case-function "foo1_bar")))
  (should (equal "foo1_bar" (string-inflection-snake-case-function "FOO1_BAR")))
  (should (equal "foo1bar" (string-inflection-snake-case-function "foo1bar")))
  (should (equal "foo1_bar" (string-inflection-snake-case-function "foo1__bar")))
  (should (equal "foo1_bar" (string-inflection-snake-case-function "Foo1Bar")))
  (should (equal "foo1_bar" (string-inflection-snake-case-function "FOO1Bar")))
  (should (equal "foo_bar" (string-inflection-snake-case-function "FOO_BAR"))))

(ert-deftest test-consecutive-uppercase ()
  (should (equal "a_single_line" (string-inflection-snake-case-function "ASingleLine")))
  (should (equal "php_mode" (string-inflection-snake-case-function "PHPMode")))
  (should (equal "ends_with_php" (string-inflection-snake-case-function "EndsWithPHP")))
  (should (equal "php_and_xml_too" (string-inflection-snake-case-function "PHPAndXMLToo")))
  (should (equal "php_and_xml_too" (string-inflection-snake-case-function "phpAndXmlToo")))
  (should (equal "ph_pand_xm_ltoo" (string-inflection-snake-case-function "PHPandXMLtoo")))
  (should (equal "eĥo_ŝanĝo_ĉiu_ĵaŭde" (string-inflection-snake-case-function "EĤOŜanĝoĈIUĴaŭde"))))

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

(ert-deftest test-capital-snake-case ()
  (should (equal "Foo1_Bar" (string-inflection-capital-snake-case-function "foo1_bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-snake-case-function "eĥo1_ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-snake-case-function "FOO1_BAR")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-snake-case-function "EĤO1_ŜANĜO")))
  (should (equal "Foo1bar" (string-inflection-capital-snake-case-function "foo1bar")))
  (should (equal "Eĥo1ŝanĝo" (string-inflection-capital-snake-case-function "eĥo1ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-snake-case-function "foo1__bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-snake-case-function "eĥo1__ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-snake-case-function "Foo1Bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-snake-case-function "Eĥo1Ŝanĝo")))
  (should (equal "Foo1_Bar" (string-inflection-capital-snake-case-function "FOO1Bar")))
  (should (equal "Eĥo1_Ŝanĝo" (string-inflection-capital-snake-case-function "EĤO1Ŝanĝo")))
  (should (equal "Foo_Bar" (string-inflection-capital-snake-case-function "FOO_BAR")))
  (should (equal "Eĥo_Ŝanĝo" (string-inflection-capital-snake-case-function "EĤO_ŜANĜO"))))

;; --------------------------------------------------------------------------------

(ert-deftest test-symbol-p ()
  (should (string-inflection-symbol-p "foo"))
  (should (string-inflection-symbol-p "eĥo"))
  (should-not (string-inflection-symbol-p "foo_bar"))
  (should-not (string-inflection-symbol-p "eĥo_ŝanĝo")))

(ert-deftest test-snake-case-p ()
  (should (string-inflection-snake-case-p "foo"))
  (should (string-inflection-snake-case-p "eĥo"))
  (should (string-inflection-snake-case-p "foo_bar"))
  (should (string-inflection-snake-case-p "eĥo_ŝanĝo")))

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

(ert-deftest test-capital-snake-case-p ()
  (should (string-inflection-capital-snake-case-p "Foo_Bar"))
  (should (string-inflection-capital-snake-case-p "Eĥo_Ŝanĝo")))

;; -------------------------------------------------------------------------------- Target symbol of cursor position

(defun buffer-try (str position &optional mode-func)
  (with-temp-buffer
    (funcall (or mode-func #'fundamental-mode))
    (insert str)
    (goto-char (apply position))
    (prog1
        (string-inflection-get-current-symbol)
      (kill-this-buffer))))

(ert-deftest test-get-current-symbol-on-cursor ()
  (should (equal "foo"  (buffer-try "foo"      '(point-max))))
  (should (equal "eĥo"  (buffer-try "eĥo"      '(point-max))))
  (should (equal "foo"  (buffer-try "foo"      '(point-min))))
  (should (equal "eĥo"  (buffer-try "eĥo"      '(point-min))))
  (should (equal ""     (buffer-try ""         '(point-max))))
  (should (equal "foo"  (buffer-try "foo->bar" '(point-min) #'c-mode)))
  (should (equal "eĥo"  (buffer-try "eĥo->ŝanĝo" '(point-min) #'c-mode)))
  (should (equal "foo-" (buffer-try "foo-"     '(point-min))))
  (should (equal "eĥo-" (buffer-try "eĥo-"     '(point-min))))
  (should (equal "foo" (buffer-try "foo-"     '(point-min) #'python-mode)))
  (should (equal "eĥo" (buffer-try "eĥo-"     '(point-min) #'python-mode)))
)

;; -------------------------------------------------------------------------------- Target all of region

(defun region-try-inflect (str &optional inflect mode-func)
  (with-temp-buffer
    (funcall (or mode-func #'fundamental-mode))
    (insert str)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (funcall (or inflect #'string-inflection-toggle))
    (buffer-string)))

(ert-deftest test-inflect-toggle-in-region ()
  (should (equal "Foo"  (region-try-inflect "foo"))) ; It was snake_case when old version.
  (should (equal "Foo Bar"  (region-try-inflect "foo bar"))) ; It was snake_case when old version.
  (should (equal "Foo Bar Baz"  (region-try-inflect "foo bar baz")))
  (should (equal "FooBar BarFoo"  (region-try-inflect "foo_bar bar_foo")))
  (should (equal "Foo:Bar"  (region-try-inflect "foo:bar")))  ; It was snake_case when old version.
  (should (equal "Foo::Bar" (region-try-inflect "foo::bar")))  ; It was snake_case when old version.
  (should (equal "Foo.Bar"  (region-try-inflect "foo.bar")))
  (should (equal "Foo().Bar" (region-try-inflect "foo().bar")))
  (should (equal "Foo()->Bar" (region-try-inflect "foo()->bar" #'string-inflection-toggle #'c-mode)))
  )

(ert-deftest test-inflect-in-region ()
  (should (equal "FooBar" (region-try-inflect "foo_bar" #'string-inflection-camelcase)))
  (should (equal "FooBar BarFoo" (region-try-inflect "foo_bar bar-foo" #'string-inflection-camelcase)))
  (should (equal "fooBar barFoo" (region-try-inflect "foo_bar bar-foo" #'string-inflection-lower-camelcase)))
  (should (equal "foo_bar bar_foo" (region-try-inflect "FooBar bar-foo" #'string-inflection-snake-case)))
  (should (equal "Foo_Bar Bar_Foo" (region-try-inflect "FooBar bar-foo" #'string-inflection-capital-snake-case)))
  (should (equal "FOO_BAR BAR_FOO" (region-try-inflect "FooBar bar-foo" #'string-inflection-upcase)))
  (should (equal "foo-bar bar-foo" (region-try-inflect "FooBar bar_foo" #'string-inflection-kebab-case)))

  ;; https://github.com/akicho8/string-inflection/issues/34
  (should (equal ":foo-bar bar" (region-try-inflect ":fooBar bar" #'string-inflection-kebab-case)))

  ;; https://github.com/akicho8/string-inflection/issues/31
  (should (equal "\nfoo_bar\nbar_foo\n" (region-try-inflect "\nFooBar\nbar-foo\n" #'string-inflection-snake-case)))

  ;; https://github.com/akicho8/string-inflection/issues/30
  (should (equal "obj_name->meth_name\nobj1_name->meth1_name"
                 (region-try-inflect "ObjName->MethName\nObj1Name->Meth1Name" #'string-inflection-snake-case #'c++-mode)))
  )

(ert-deftest test-cycle-in-region ()
  (should (equal "FOO_BAR FooBar foo_bar"
                 (region-try-inflect "foo_bar FOO_BAR FooBar" #'string-inflection-ruby-style-cycle)))
  (should (equal "FooBar foo_bar"
                 (region-try-inflect "foo_bar FooBar" #'string-inflection-elixir-style-cycle)))
  (should (equal "foo_bar FOO_BAR FooBar foo_bar"
                 (region-try-inflect "fooBar foo_bar FOO_BAR FooBar" #'string-inflection-python-style-cycle)))
  (should (equal "FOO_BAR FooBar fooBar"
                 (region-try-inflect "foo_bar FOO_BAR FooBar" #'string-inflection-java-style-cycle)))
  (should (equal "FOO_BAR FooBar fooBar foo-bar Foo_Bar foo_bar"
                 (region-try-inflect "foo_bar FOO_BAR FooBar fooBar foo-bar Foo_Bar" #'string-inflection-all-cycle)))
  )

(defun buffer-try-inflect (str inflect)
  (with-temp-buffer
    (c-mode)
    (insert str)
    (goto-char (point-min))
    (funcall inflect)
    (buffer-string)))

(ert-deftest test-buffer-toggle ()
  (should (equal "foo_bar" (buffer-try-inflect "fooBar" 'string-inflection-toggle))))

(ert-deftest test-buffer-snake-case ()
  ;; https://github.com/akicho8/string-inflection/issues/30
  (should (equal "object_name->method" (buffer-try-inflect "objectName->method" 'string-inflection-snake-case)))
  (should (equal "object1_name->method" (buffer-try-inflect "object1Name->method" 'string-inflection-snake-case)))
  (should (equal "eĥo_ŝanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥoŜanĝo->ĉiuĴaŭde" 'string-inflection-snake-case))))

(ert-deftest test-buffer-camelcase ()
  (should (equal "ObjectName->method" (buffer-try-inflect "object_name->method" 'string-inflection-camelcase)))
  (should (equal "Object1Name->method" (buffer-try-inflect "object1_name->method" 'string-inflection-camelcase)))
  (should (equal "EĥoŜanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥo_ŝanĝo->ĉiuĴaŭde" 'string-inflection-camelcase))))

(ert-deftest test-buffer-lower-camelcase ()
  (should (equal "objectName->method" (buffer-try-inflect "object_name->method" 'string-inflection-lower-camelcase)))
  (should (equal "object1Name->method" (buffer-try-inflect "object1_name->method" 'string-inflection-lower-camelcase)))
  (should (equal "eĥoŜanĝo->ĉiuĴaŭde" (buffer-try-inflect "eĥo_ŝanĝo->ĉiuĴaŭde" 'string-inflection-lower-camelcase))))


(defun buffer-try-final-pos (str final-pos inflect initial-pos)
  (with-temp-buffer
    (setq-local string-inflection-final-position final-pos)
    (insert (concat str " fooo"))
    (goto-char initial-pos)
    (funcall inflect)
    (should-not (use-region-p))
    (point)))

(ert-deftest test-buffer-remain-simple-lengthen ()
  (should (equal (buffer-try-final-pos "FooBar" 'remain #'string-inflection-snake-case 2) 2)))

(ert-deftest test-buffer-end-simple-lengthen ()
  (should (equal (buffer-try-final-pos "FooBar" 'end #'string-inflection-snake-case 2) 8)))

(ert-deftest test-buffer-beginning-simple-lengthen ()
  (should (equal (buffer-try-final-pos "FooBar" 'beginning #'string-inflection-snake-case 2) 1)))

(ert-deftest test-buffer-remain-simple-shorten-not-at-end ()
  (should (equal (buffer-try-final-pos "foo_bar" 'remain #'string-inflection-camelcase 8) 7)))

(ert-deftest test-buffer-remain-simple-shorten-at-end ()
  (should (equal (buffer-try-final-pos "foo_bar" 'remain #'string-inflection-camelcase 2) 2)))

(ert-deftest test-buffer-end-simple-shorten ()
  (should (equal (buffer-try-final-pos "foo_bar" 'end #'string-inflection-camelcase 2) 7)))

(ert-deftest test-buffer-beginning-simple-shorten ()
  (should (equal (buffer-try-final-pos "foo_bar" 'beginning #'string-inflection-camelcase 2) 1)))


(defun region-try-final-pos (str final-pos inverse)
    (with-temp-buffer
    (setq-local string-inflection-final-position final-pos)
    (insert str)
    (let ((final-point (point-max)))
      (insert " foooo")
      (if inverse
          (progn (set-mark final-point) (goto-char (point-min)))
        (set-mark (point-min)) (goto-char final-point))
      (activate-mark))
    (string-inflection-snake-case)
    (should (use-region-p))
    (should-not deactivate-mark)
    (cons (point) (cons (region-beginning) (region-end)))))


(ert-deftest test-buffer-remain-region-straight ()
  (let* ((state (region-try-final-pos "FooBar" 'remain nil))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 8))))


(ert-deftest test-buffer-remain-region-inversed ()
  (let* ((state (region-try-final-pos "FooBar" 'remain t))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 1))))


(ert-deftest test-buffer-end-region-straight ()
  (let* ((state (region-try-final-pos "FooBar" 'end nil))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 8))))


(ert-deftest test-buffer-end-region-inverse ()
  (let* ((state (region-try-final-pos "FooBar" 'end t))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 8))))


(ert-deftest test-buffer-beginning-region-straight ()
  (let* ((state (region-try-final-pos "FooBar" 'beginning nil))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 1))))


(ert-deftest test-buffer-beginning-region-inverse ()
  (let* ((state (region-try-final-pos "FooBar" 'beginning t))
         (final-pos (car state))
         (region (cdr state))
         (beginning (car region))
         (end (cdr region)))
    (should (equal beginning 1))
    (should (equal end 8))
    (should (equal final-pos 1))))


(defun mixed-region-cycle-try (start end)
  (with-temp-buffer
    (text-mode)
    (insert "someFunction_to_do_SomeThing FoofooBarbarBarbarFoofoo")
    (set-mark start)
    (goto-char end)
    (activate-mark)
    (string-inflection-cycle)
    (cons (buffer-string) (cons (region-beginning) (region-end)))))


(ert-deftest test-mixed-symbol-cycle-region-1 ()
  (should (equal (car (mixed-region-cycle-try 1 13))
                 "some_function_to_do_SomeThing FoofooBarbarBarbarFoofoo")))


(ert-deftest test-mixed-symbol-cycle-region-2 ()
  (should (equal (car (mixed-region-cycle-try 14 19))
                 "someFunction_TO_DO_SomeThing FoofooBarbarBarbarFoofoo")))


(ert-deftest test-mixed-symbol-cycle-region-3 ()
  (should (equal (car (mixed-region-cycle-try 20 29))
                 "someFunction_to_do_some_thing FoofooBarbarBarbarFoofoo")))


(ert-deftest test-mixed-symbol-cycle-region-4 ()
  (should (equal (car (mixed-region-cycle-try 20 41))
                 "someFunction_to_do_some_thing foofoo_barbarBarbarFoofoo")))


(ert-deftest test-mixed-symbol-cycle-region-restore-region ()
  (should (equal (cdr (mixed-region-cycle-try 20 41)) (cons 20 43))))


(ert-run-tests-batch t)
