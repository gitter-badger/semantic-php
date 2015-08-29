;;; tests.el ---
(require 'semantic-php)

;;; Commentary:
;; An introductory test to experiment with class name resolution.
;;
;; The purpose of these tests is to provide a set of functions to
;; resolve all type names into fully qualified names.
;;
;; From http://php.net/manual/en/language.namespaces.rules.php
;;
;; - Unqualified name
;; This is an identifier without a namespace separator, such as Foo
;;
;; - Qualified name
;; This is an identifier with a namespace separator, such as Foo\Bar
;;
;; - Fully qualified name
;; This is an identifier with a namespace separator that begins with a
;; namespace separator, such as \Foo\Bar. The namespace \Foo is also
;; a fully qualified name.
;;
;; TODO: mostly concerning rule 6 + due sense checkes: I can think of
;; the case when PHP raises a fatal error because, for example,
;; A\\B\\DateTime isn't declared and cannot be autoloaded, those cases
;; should be treated as a lost include and should not suggest to load
;; the class in the global NS.
;;
;; In general it would be best to stick to the semantics of the
;; language and not resolve symbols at all costs, instead warn the
;; user about the resolution problem and act on it:
;; - fix the class name
;; - add an import
;; - create the class

;;; Code:

(ert-deftest semantic-php-name-resolution-rule-1 ()
  "
Calls to fully qualified functions, classes or constants are
resolved at compile-time. For instance new \\A\\B resolves to class
A\\B."
  (should (assoc-string
           "DateTime"
           (semantic-php-resolve-name "\\DateTime"))))

(ert-deftest semantic-php-name-resolution-rule-2 ()
  "
All unqualified and qualified names (not fully qualified names)
are translated during compilation according to current import
rules. For example, if the namespace A\\B\\C is imported as C, a call
to C\\D\\e() is translated to A\\B\\C\\D\\e()."
  (let ((semantic-php-aliased-names (list (cons "C" "A\\B\\C"))))
    (should (assoc-string
             "A\\B\\C\\D\\e"
             (semantic-php-resolve-name "C\\D\\e")))))

(ert-deftest semantic-php-name-resolution-rule-3 ()
  "
Inside a namespace, all qualified names not translated according
to import rules have the current namespace prepended. For example,
if a call to C\\D\\e() is performed within namespace A\\B, it is
translated to A\\B\\C\\D\\e()."
  (should (assoc-string
           "A\\B\\C\\D\\e"
           (semantic-php-resolve-name "C\\D\\e" "A\\B"))))

(ert-deftest semantic-php-name-resolution-rule-4 ()
  "
Unqualified class names are translated during compilation
according to current import rules (full name substituted for short
imported name). In example, if the namespace A\\B\\C is imported as
C, new C() is translated to new A\\B\\C()."
  (let ((semantic-php-aliased-names (list (cons "C" "A\\B\\C"))))
    (should (assoc-string
             "A\\B\\C"
             (semantic-php-resolve-name "C")))))

(ert-deftest semantic-php-name-resolution-rule-5 ()
  "
Inside namespace (say A\\B), calls to unqualified functions are
resolved at run-time. Here is how a call to function foo() is
resolved:

i) It looks for a function from the current namespace: A\\B\\foo().
ii) It tries to find and call the global function foo()."
  (should (equal
           (list "A\\B\\foo" "foo")
           (semantic-php-resolve-name "foo" "A\\B"))))

(ert-deftest semantic-php-name-resolution-rule-6 ()
  "Inside namespace (say A\\B), calls to unqualified or qualified
class names (not fully qualified class names) are resolved at
run-time. Here is how a call to new C() or new D\\E() is
resolved. For new C():

i) It looks for a class from the current namespace: A\\B\\C.
ii) It attempts to autoload A\\B\\C.

For new D\\E():
iii) It looks for a class by prepending the current namespace: A\\B\\D\\E.
iv) It attempts to autoload A\\B\\D\\E.

To reference any global class in the global namespace, its fully
qualified name new \\C() must be used."
  (should (assoc-string
           "A\\B\\C"
           (semantic-php-resolve-name "C" "A\\B")))

  (should (assoc-string
           "A\\B\\D\\E"
           (semantic-php-resolve-name "D\\E" "A\\B"))))

(ert-deftest semantic-php-name-resolution-unqualified-in-global ()
  "Tests the resolution of an unqualified name in a the global namespace."
  (should (equal
           (list "in_array")
           (semantic-php-resolve-name "in_array")))
  (should (equal
           (list "DateTime")
           (semantic-php-resolve-name "DateTime"))))

(ert-deftest semantic-php-name-resolution-partially-qualified-in-global ()
  ""
  (should (equal
           (list "MainNs\\Version")
           (semantic-php-resolve-name "MainNs\\Version")))
  (should (equal
           (list "Zend\\Console\\Getopt")
           (semantic-php-resolve-name "Zend\\Console\\Getopt"))))

(ert-deftest semantic-php-name-resolution-fully-qualified-in-global ()
  "Tests the resolution of a fully qualified name in the global namespace."
  (should (equal
           (list "in_array")
           (semantic-php-resolve-name "\\in_array")))
  (should (equal
           (list "DateTime")
           (semantic-php-resolve-name "\\DateTime")))
  (should (equal
           (list "Zend\\Console\\Getopt")
           (semantic-php-resolve-name "\\Zend\\Console\\Getopt"))))

(ert-deftest semantic-php-name-resolution-unqualified-in-ns ()
  ""
  (should (equal
           (list "MainNs\\in_array" "in_array")
           (semantic-php-resolve-name "in_array" "MainNs")))
  (should (equal
           (list "MainNs\\DateTime" "DateTime")
           (semantic-php-resolve-name "DateTime" "MainNs"))))

(ert-deftest semantic-php-name-resolution-partially-qualified-in-ns ()
  ""
  (should (equal
           (list "MainNs\\Version" "Version")
           (semantic-php-resolve-name "Version" "MainNs")))
  (should (equal
           (list "MainNs\\MyNs\\MyClass" "MyNs\\MyClass")
           (semantic-php-resolve-name "MyNs\\MyClass" "MainNs"))))

(ert-deftest semantic-php-name-resolution-fully-qualified-in-ns ()
  "Tests the resolution of a fully qualified name in a namespace"
  (should (equal
           (list "in_array")
           (semantic-php-resolve-name "\\in_array" "MainNs")))
  (should (equal
           (list "DateTime")
           (semantic-php-resolve-name "\\DateTime" "MainNs")))
  (should (equal
           (list "MyNs\\MyClass")
           (semantic-php-resolve-name "\\MyNs\\MyClass" "MainNs"))))

(ert-deftest semantic-php-name-resolution-unqualified-with-alias-in-global ()
  "Tests the resolution of unqualified names with a matching alias."
  (let ((semantic-php-aliased-names
         (list (cons "TestDate" "Carbon\\Carbon")
               (cons "TestCase" "PHPUnit_Framework_TestCase"))))
    (should (equal
             (list "TestDate" "Carbon\\Carbon")
             (semantic-php-resolve-name "TestDate" nil)))
    (should (equal
             (list "Version")
             (semantic-php-resolve-name "Version" nil)))))

(ert-deftest semantic-php-name-resolution-unqualified-with-alias-in-global ()
  (let ((semantic-php-aliased-names
         (list (cons "TestDate" "Carbon\\Carbon")
               (cons "TestCase" "PHPUnit_Framework_TestCase"))))
    (should (equal
             (list "MainNs\\TestCase" "PHPUnit_Framework_TestCase" "TestCase")
             (semantic-php-resolve-name "TestCase" "MainNs")))
    (should (equal
             (list "MainNs\\TestDate" "Carbon\\Carbon" "TestDate")
             (semantic-php-resolve-name "TestDate" "MainNs")))))

(ert-deftest semantic-php-name-resolution-with-alias-and-no-known-aliases ()
    "Tests resolving a unqualified name without any known aliases."
    (let ((semantic-php-aliased-names nil))
      (should (equal
               "DateTime"
               (semantic-php--name-unalias "DateTime")))))

(ert-deftest semantic-php-name-resolution-with-alias ()
  "Tests the resolution of a symbol that has been aliased."
  (let ((semantic-php-aliased-names (list (cons "TestDate" "Carbon\\Carbon"))))
    (should (equal
             "Carbon\\Carbon"
             (semantic-php--name-unalias "TestDate")))))

(ert-deftest semantic-php-name-resolution-with-aliased-ns ()
  "Tests the resolution of a symbol that has been aliased."
  (let ((semantic-php-aliased-names (list (cons "C" "A\\B\\C"))))
    (should (equal
             "A\\B\\C\\D\\E"
             (semantic-php--name-unalias "C\\D\\E")))))

(provide 'tests)

;;; tests.el ends here
