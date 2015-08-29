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

(defun semantic-php--name-unalias (symbol-name)
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
           (semantic-php--name-unalias (car name-parts)))
        symbol-name))))

(defun semantic-php--name-qualified-alternatives (symbol-name current-ns)
  "Given an unqualified SYMBOL-NAME it returns a list of is
possible qualified names as available in CURRENT-NS."
  (if current-ns
      (delete-dups
       (list (semantic-php--name-prepend-ns symbol-name current-ns)
             (semantic-php--name-unalias symbol-name)
             symbol-name))
    (if (semantic-php--name-aliased-p symbol-name)
        (delete-dups
         (list symbol-name (semantic-php--name-unalias symbol-name)))
      (list symbol-name))))

(defun semantic-php--name-aliased-ns-p (symbol-name)
  "Cheks if the partially qualified SYMBOL-NAME has an alias as
  its first component."
  (semantic-php--name-aliased-p (split-string symbol-name "\\\\")))

(defun semantic-php--name-aliased-p (symbol-name)
  "Checks if SYMBOL-NAME matches any known aliased name in `semantic-php-aliased-names'."
  (when (semantic-php--name-unalias symbol-name) t))

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

(provide 'semantic-php)
