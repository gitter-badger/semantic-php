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

;; 2. All unqualified and qualified names (not fully qualified names)
;; are translated during compilation according to current import
;; rules. For example, if the namespace A\B\C is imported as C, a call
;; to C\D\e() is translated to A\B\C\D\e().
(ert-deftest semantic-php-name-resolution-rule-2 ()
  (let ((semantic-php-aliased-names (list (cons "C" "A\\B\\C"))))
    ;; (should (equal (list "A\\B\\C\\D\\e")
    (semantic-php-resolve-name "C\\D\\e" nil)))

;;
;; 3. Inside a namespace, all qualified names not translated according
;; to import rules have the current namespace prepended. For example,
;; if a call to C\D\e() is performed within namespace A\B, it is
;; translated to A\B\C\D\e().
(ert-deftest semantic-php-name-resolution-rule-3 ()
  ""
  (should (equal (list "A\\B\\C\\D\\e" "C\\D\\e")
                 (semantic-php-resolve-name "C\\D\\e" "A\\B"))))

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

(defvar semantic-php-aliased-names nil
  "A list of cons (ALIAS-NAME . TYPE-NAME) representing aliased names.

This includes anything classes, constants and functions imported
with a use declaration.

NOTE TODO: At this moment I don't know to takle method aliasing
with the trait use declaration inside classes.")

(defun semantic-php-resolve-name (symbol-name &optional current-ns)
  "Resolves SYMBOL-NAME into a fully qualified name.

Returns a list of possible names, this will normally contain two
elements for all partially and unqualified names, i.e. the
possible namespace local and global version of the symbol same.

When CURRENT-NS is provided this information will be used to
resolve unqualified and partially qualified names correctly.

If SYMBOL-NAME matches a known alias, the alias is resolved and
the appropriate fully qualified name returned. Please see the
variable `semantic-php-aliased-names' on how to alias names."
  (if (semantic-php--name-fully-qualified-p symbol-name)
      (list (semantic-php--name-strip-leading-ns symbol-name))
    (semantic-php--name-qualified-alternatives symbol-name current-ns)))

(defun semantic-php--name-aliased-ns-p (symbol-name)
  "Cheks if the partially qualified SYMBOL-NAME has an alias as
  its first component."
  (semantic-php--name-aliased-p (split-string symbol-name "\\\\")))

(defun semantic-php--name-aliased-p (symbol-name)
  "Checks if SYMBOL-NAME matches any known aliased name in `semantic-php-aliased-names'."
  (when (semantic-php--name-dereference symbol-name) t))

(defun semantic-php--name-fully-qualified-p (symbol-name)
  "Tests if SYMBOL-NAME is a fully qualified name."
  (equal "\\" (substring symbol-name 0 1)))

(defun semantic-php--name-strip-leading-ns (symbol-name)
  "Strips the leading namespace separator from fully qualified names."
  (replace-regexp-in-string "^\\\\" "" symbol-name))

(defun semantic-php--name-prepend-ns (symbol-name ns-name)
  "Join NS-NAME and SYMBOL-NAME using a namespace separator.

If NS-NAME is nil, SYMBOL-NAME is returned as is. This is
handy to prepend the current namespace name to partially
qualified or unqualified symbols."
  (if ns-name
      (concat ns-name "\\" symbol-name)
    symbol-name))

(defun semantic-php--name-dereference (symbol-name)
  "Resolves a possibly aliased symbol SYMBOL-NAME info a FQN.

If the symbol is an alias in KNOWN-ALIASES the corresponding
fully qualified name is returned.

If the first component of SYMBOL-NAME is a known alias, then the
this part is replaced with the full namespace name."
  (let* ((name-parts (split-string symbol-name "\\\\"))
         (aliased-ns (assoc-default (car name-parts) semantic-php-aliased-names))
         (aliased-name (assoc-default symbol-name semantic-php-aliased-names)))
    (if aliased-name
        aliased-name
      (if aliased-ns
          (semantic-php--name-prepend-ns
           (mapconcat 'identity (cdr name-parts) "\\")
           (semantic-php--name-dereference (car name-parts)))
        symbol-name))))

(defun semantic-php--name-qualified-alternatives (symbol-name current-ns)
  "Given an unqualified SYMBOL-NAME it returns a list of is
possible qualified names as available in CURRENT-NS."
    (if current-ns
        (append (list (semantic-php--name-prepend-ns symbol-name current-ns))
                (list (semantic-php--name-dereference symbol-name)))
      (delete-dups (list
       symbol-name
       (semantic-php--name-dereference symbol-name)))))

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

;; (ert-deftest semantic-php-name-resolution-unqualified-with-alias-in-global ()
;;   (let ((semantic-php-aliased-names
;;          (list (cons "TestDate" "Carbon\\Carbon")
;;                (cons "TestCase" "PHPUnit_Framework_TestCase"))))
;;     ;; (should (equal
;;     ;;          (list "MainNs\\TestCase" "PHPUnit_Framework_TestCase")
;;     ;;          (semantic-php-resolve-name "TestCase" "MainNs")))
;;     ;; (should (equal
;;              ;; (list "MainNs\\TestDate" "TestDate" "Carbon\\Carbon")
;;              (semantic-php-resolve-name "TestDate" "MainNs")))))

(ert-deftest semantic-php-name-resolution-with-alias-and-no-known-aliases ()
    "Tests resolving a unqualified name without any known aliases."
    (let ((semantic-php-aliased-names nil))
      (should (equal
               "DateTime"
               (semantic-php--name-dereference "DateTime")))))

(ert-deftest semantic-php-name-resolution-with-alias ()
  "Tests the resolution of a symbol that has been aliased."
  (let ((semantic-php-aliased-names (list (cons "TestDate" "Carbon\\Carbon"))))
    (should (equal
             "Carbon\\Carbon"
             (semantic-php--name-dereference "TestDate")))))

(ert-deftest semantic-php-name-resolution-with-aliased-ns ()
  "Tests the resolution of a symbol that has been aliased."
  (let ((semantic-php-aliased-names (list (cons "C" "A\\B\\C"))))
    (should (equal
             "A\\B\\C\\D\\E"
             (semantic-php--name-dereference "C\\D\\E")))))

(provide 'tests)

;;; tests.el ends here
