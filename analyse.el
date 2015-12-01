(define-mode-local-override semantic-analyze-split-name php-mode (name)
  "Split up tag NAME into multiple parts by T_NS_SEPARATOR."
  (let ((ans (split-string name "\\\\")))
    (if (= (length ans) 1)
	name
      (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name php-mode (namelist)
  "Reassembles the components of NAMELIST into a qualified name."
  (mapconcat 'identity namelist "\\"))

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
