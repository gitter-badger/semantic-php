;; NOTE Simplistic namespace detection applies the first namespace
;; declaration found by the parser to the entire buffer, this produces
;; undesired effects if the buffer declares multiple namespaces.
;;
;; TODO Handle type definitions underneath a namespace as a block.
;; (use the two namespace, or the namespace and $EOI, tokens to
;; demarkate the block).
(defvar semantic-php-buffer-scope-namespace nil
  "The PHP namespace declared in this buffer.
Is used to compose the tag name of all unqualified symbols found
in the buffer.")

(define-mode-local-override semantic-ctxt-imported-packages php-mode (&optional point)
  (message "imported packages?")
  nil)

(defun semantic-php-analyse-init ()
  ""
  (make-local-variable 'semantic-php-analyse-init))

(defvar-mode-local php-mode semanticdb-find-default-throttle
  '(project system recursive omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because
of the omniscience database.")

(define-mode-local-override semantic-analyze-split-name php-mode (name)
  "Split up tag NAME into multiple parts by T_NS_SEPARATOR."
  (let ((ans (split-string name "\\\\")))
    (if (= (length ans) 1)
	name
      (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name php-mode (namelist)
  "Reassembles the components of NAMELIST into a qualified name."
  (mapconcat 'identity namelist "\\"))

(define-overloadable-function semantic-analyze-current-context (&optional position)
  ;; 1. Find the current namespace
  ;; Find the top level package tag.
  ;; Update the namespace in scope.
  nil)

(define-mode-local-override semantic-get-local-variables php-mode (&optional point)
  "Overrides the default based on Bovine which hangs editing."
  (message "Scanning local variable at point: %s" (or point "no point given"))
  (let ((vars nil)
	(ct (semantic-current-tag))
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))

    (while (not (semantic-up-context (point) 'function))
      (message "Scanning context")
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'variable_declaration
                       0 t)
                      vars))))

    ;; Add 'this' if in a fcn
    (when (semantic-tag-of-class-p ct 'function)
      ;; Append a new tag $this into our space.
      (setq vars (cons (semantic-tag-new-variable
			"$this" (semantic-tag-name (semantic-current-tag-parent))
			nil)
		       vars)))
    vars))
