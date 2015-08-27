;;; tests.el ---

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
;; Name Resolution Rules:
;;
;; 1. Calls to fully qualified functions, classes or constants are
;; resolved at compile-time. For instance new \A\B resolves to class
;; A\B.
;; (fully qualified types and functions names must be resolved)
;;
;; 2. All unqualified and qualified names (not fully qualified names)
;; are translated during compilation according to current import
;; rules. For example, if the namespace A\B\C is imported as C, a call
;; to C\D\e() is translated to A\B\C\D\e().
;;
;; 3. Inside a namespace, all qualified names not translated according
;; to import rules have the current namespace prepended. For example,
;; if a call to C\D\e() is performed within namespace A\B, it is
;; translated to A\B\C\D\e().
;;
;; 4. Unqualified class names are translated during compilation
;; according to current import rules (full name substituted for short
;; imported name). In example, if the namespace A\B\C is imported as
;; C, new C() is translated to new A\B\C().
;;
;; 5. Inside namespace (say A\B), calls to unqualified functions are
;; resolved at run-time. Here is how a call to function foo() is
;; resolved:
;;
;; i) It looks for a function from the current namespace: A\B\foo().
;; ii) It tries to find and call the global function foo().
;;
;; 6. Inside namespace (say A\B), calls to unqualified or qualified
;; class names (not fully qualified class names) are resolved at
;; run-time. Here is how a call to new C() or new D\E() is
;; resolved. For new C():
;;
;; i) It looks for a class from the current namespace: A\B\C.
;; ii) It attempts to autoload A\B\C.
;;
;; For new D\E():
;; iii) It looks for a class by prepending the current namespace: A\B\D\E.
;; iv) It attempts to autoload A\B\D\E.
;;
;; To reference any global class in the global namespace, its fully
;; qualified name new \C() must be used.

;;; Code:

(defun semantic-php--name-prepend-ns (symbol-name ns-name)
  "Join NS-NAME and SYMBOL-NAME using a namespace separator.

If NS-NAME is nil, SYMBOL-NAME is returned as is. This is
handy to prepend the current namespace name to partially
qualified or unqualified symbols."
  (if ns-name
      (concat ns-name "\\" symbol-name)
    symbol-name))

(defun semantic-php--name-fully-qualified-p (symbol-name)
  "Tests if SYMBOL-NAME is a fully qualified name."
  (equal "\\" (substring symbol-name 0 1)))

(defun semantic-php--name-strip-leading-ns (symbol-name)
  "Strips the leading namespace separator from fully qualified names."
  (replace-regexp-in-string "^\\\\" "" symbol-name))

(defun semantic-php--name-qualified-alternatives (symbol-name current-ns)
  "Given an unqualified SYMBOL-NAME it returns a list of is
possible qualified names as available in CURRENT-NS."
  (if current-ns
      (list (semantic-php--name-prepend-ns symbol-name current-ns) symbol-name)

    ;; We're in the global namespace, so we return the singleton
    ;; list with symbol-name.
    (list symbol-name)))

(defun semantic-php-resolve-name (symbol-name &optional current-ns &optional import-rules)
  "Resolves SYMBOL-NAME into a fully qualified name.

Returns a list of possible names, this will normally contain two
elements for all partially and unqualified names, i.e. the
possible namespace local and global version of the symbol same."
  (if (semantic-php--name-fully-qualified-p symbol-name)
      (list (semantic-php--name-strip-leading-ns symbol-name))
    (semantic-php--name-qualified-alternatives symbol-name current-ns)))

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

(ert-deftest semantic-php-name-resolution-unqualified-in-global ()
  "Tests the resolution of an unqualified name in a the global namespace."
  (should (equal
           (list "in_array")
           (semantic-php-resolve-name "in_array")))
  (should (equal
           (list "DateTime")
           (semantic-php-resolve-name "DateTime")))
  (should (equal
           (list "Zend\\Console\\Getopt")
           (semantic-php-resolve-name "Zend\\Console\\Getopt"))))

(ert-deftest semantic-php-name-resolution-unqualified-in-ns ()
  (should (equal
           (list "MainNs\\in_array" "in_array")
           (semantic-php-resolve-name "in_array" "MainNs")))
  (should (equal
           (list "MainNs\\DateTime" "DateTime")
           (semantic-php-resolve-name "DateTime" "MainNs")))
  (should (equal
           (list "MainNs\\MyNs\\MyClass" "MyNs\\MyClass")
           (semantic-php-resolve-name "MyNs\\MyClass" "MainNs"))))

;; (ert-deftest semantic-php-function-name-resolution-in-namespace ()
;;     "Tests resolution of a function call in namespace.
;;      namespace A;
;;      foo();      // first tries to call 'foo' defined in namespace 'A'
;;                  // then calls global function 'foo'"
;;   (should (equal ("A\foo" "foo") (semantic-php-resolve-name "A" "foo"))))

(provide 'tests)

;;; tests.el ends here
