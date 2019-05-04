;; ==============
;; Type Transform Graph
;; ==============

;; Association list of input-predicate -> '(
;;   (predicate-transformation . transformation)
;;   ...
;; )
(define %transform-graph)

(define (reset-transform-graph!)
  (set! %transform-graph (make-alist-store equal?)))

(reset-transform-graph!)

(define (add-to-transform-graph! input-predicate predicate-transformation transformation)
  (register-predicate! input-predicate)
  (let*
    ((existing-transforms (get-predicate-transforms input-predicate))
     (new-transform (make-transform input-predicate predicate-transformation transformation))
     (new-transforms (cons new-transform existing-transforms)))
    ((%transform-graph 'put!) input-predicate new-transforms)))

(define (register-predicate! predicate)
  (if (not (predicate? predicate))
    (begin
      ((%transform-graph 'put!) predicate '())
      ((%supertype-graph 'put!) predicate '()))))

(define (predicate? function)
  ((%transform-graph 'has?) function))

(define (get-predicate-transforms predicate)
  (if (predicate? predicate)
    ((%transform-graph 'get) predicate)
    '()))

(define (register-type-transform! input-predicate output-predicate
				  transformation)
  (assert (or (predicate? input-predicate) (list? input-predicate)) input-predicate)
  (assert (or (predicate? output-predicate) (list? output-predicate)))
  (add-to-transform-graph! input-predicate
			   (lambda (x) output-predicate)
			   transformation))

(define (register-type-transform-f! input-predicate output-predicate transformation)
  (add-to-transform-graph! input-predicate
			   output-predicate
			   transformation))

(define (all-predicates)
  ((%transform-graph 'get-keys)))

(define (all-compound-predicates)
  (filter list? (all-predicates)))

;; ==============
;; Supertypes 
;; ==============

;; Association list of input-predicate -> '(
;;   predicate-supertype?
;; )
(define %supertype-graph)

(define (reset-supertype-graph!)
  (set! %supertype-graph (make-alist-store equal?)))

(reset-supertype-graph!)

(define (register-super! predicate-sub predicate-super)
  (register-predicate! predicate-sub)
  (register-predicate! predicate-super)

  ((%supertype-graph 'put!) predicate-sub
    (cons predicate-super (get-predicate-supers predicate-sub))))

(define (get-predicate-supers predicate)
  ((%supertype-graph 'get-default) predicate '()))

;; ==============
;; Transforms
;; ==============

;; Transforms are stored as (cons transform-input-predicate-to-output-fn transform-data-fn)
;; Note transforms can be a compound list of transforms to apply to list of predicates

(define (make-transform input-predicate predicate-transformation transformation)
  (cons input-predicate (cons predicate-transformation transformation)))

(define (make-joiner-transform compound-predicate paths-list)
  (cons 'joiner (cons compound-predicate paths-list)))

(define (joiner-transform-output-predicate transform)
  (cadr transform))

(define (joiner-transform-paths-list transform)
  (cddr transform))

(define (is-joiner-transform? transform)
  (equal? (car transform) 'joiner))

(define (is-compound-transform? transform)
  (list? transform))

(define (transformation-input-predicate transformation)
  (car transformation))

(define (transformation-predicate-transform transformation)
  (lambda (in)
    (cond
     ((is-joiner-transform? transformation)
      (joiner-transform-output-predicate transformation))
     ((is-compound-transform? transformation)
	(map
	 (lambda (value transform) ((transformation-predicate-transform transform) value)) 
	 in 
	 transformation))
     (else
	((cadr transformation) in)))))

(define (transformation-data-transform transformation)
  (cond
   ((is-joiner-transform? transformation)
    (lambda (in)
      (map
       (lambda (path)
	 ((create-compound-transformation path) in))
       (joiner-transform-paths-list transformation))))
   ((is-compound-transform? transformation)
    (lambda (in) 
      (map 
        (lambda (value transform) ((transformation-data-transform transform) value)) 
        in 
        transformation)))
   (else
    (cddr transformation))))

(define (apply-transformation-data-transform transformation in-value)
  (let ((dt-fn (transformation-data-transform transformation)))
    (if (list? (transformation-input-predicate transformation))
	(apply dt-fn in-value)
	(dt-fn in-value))))

(define identity-transform (make-transform always-true identity identity))

;; ==============
;; Paths
;; ==============

(define (last lst) (car (last-pair lst)))

(define (remove-from-path-before-joiner path)
  (let ((reversed-path (reverse path)))
    (define (recurse-build remaining-reversed-path built-path)
      (if (null? remaining-reversed-path)
	  built-path
	  (let ((transform (car remaining-reversed-path)))
	    (if (is-joiner-transform? transform)
		(cons transform built-path)
		(recurse-build
		 (cdr remaining-reversed-path)
		 (cons transform built-path))))))
    (recurse-build reversed-path '())))

(define (create-compound-transformation path)
  (if (null? path)
      identity
      (let ((transform-rest-of-path
	     (create-compound-transformation (cdr path)))
	    (transform (car path)))
	(if (is-joiner-transform? transform)
	    (lambda (in)
	      (transform-rest-of-path
	       (map
		(lambda (joiner-sub-path)
		  ((create-compound-transformation joiner-sub-path) in))
		(joiner-transform-paths-list transform))))
	    (lambda (in)
	      (transform-rest-of-path
	       (apply-transformation-data-transform
		transform in)))))))

(define (create-compound-transformation-debugger-transforms path)
  (if (null? path)
      (lambda (x) '())
      (let ((transform-rest-of-path
	     (create-compound-transformation-debugger-transforms (cdr path)))
	    (transform (car path)))
	(if (is-joiner-transform? transform)
	    (lambda (in)
	      (list 
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation-debugger-transforms joiner-sub-path) in))
		 (joiner-transform-paths-list transform))
	       (transform-rest-of-path
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation joiner-sub-path) in))
		 (joiner-transform-paths-list transform)))))
	    (lambda (in)
	      (cons
	       (get-name (transformation-data-transform transform))
	       (transform-rest-of-path
		(apply-transformation-data-transform
		 transform in))))))))

(define (create-compound-transformation-debugger-predicates path)
  (if (null? path)
      (lambda (x) '())
      (let ((transform-rest-of-path
	     (create-compound-transformation-debugger-predicates (cdr path)))
	    (transform (car path)))
	(if (is-joiner-transform? transform)
	    (lambda (in)
	      (list 
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation-debugger-predicates joiner-sub-path) in))
		 (joiner-transform-paths-list transform))
	       (transform-rest-of-path
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation joiner-sub-path) in))
		 (joiner-transform-paths-list transform)))))
	    (lambda (in)
	      (cons
	       (get-name ((transformation-predicate-transform transform) (transformation-input-predicate transform)))
	       (transform-rest-of-path
		(apply-transformation-data-transform
		 transform in))))))))

(define (create-compound-transformation-debugger-values path)
  (if (null? path)
      identity
      (let ((transform-rest-of-path
	     (create-compound-transformation-debugger-values (cdr path)))
	    (transform (car path)))
	(if (is-joiner-transform? transform)
	    (lambda (in)
	      (list 
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation-debugger-values joiner-sub-path) in))
		 (joiner-transform-paths-list transform))
	       (transform-rest-of-path
		(map
		 (lambda (joiner-sub-path)
		   ((create-compound-transformation joiner-sub-path) in))
		 (joiner-transform-paths-list transform)))))
	    (lambda (in)
	      (cons
	       in
	       (transform-rest-of-path
		(apply-transformation-data-transform
		 transform in))))))))

;; ==============
;; Search Engine
;; ==============

(define (transforms-using-reached-predicates input-predicate
					     reached-predicates) 1)
  

(define (all-transforms-for-compound-predicate input-predicate)
  (assert (list? input-predicate))
  (crossproduct 
   (map
    (lambda (pred) (cons
		    identity-transform
		    ;; Note we don't add the reached-predicates table
		    ;; here so it doesn't try to make nested compound predicates
		    (all-transforms-for-predicate pred
						  (make-equal-hash-table)
						  (list))))
    input-predicate)))

(define (all-valid-compound-predicates input-predicate reached-predicates)
  ;; Find compound-predicates that we can make by using our
  ;; input-predicate at least once and filling the rest of the slots
  ;; with things from reached-predicates
  (filter
   (lambda (compound-predicate)
     (and (member input-predicate compound-predicate)
	  (every
	   (lambda (sub-predicate)
	     (or (equal? sub-predicate input-predicate)
		 (hash-table/get reached-predicates sub-predicate #f)))
	   compound-predicate)))
  (all-compound-predicates)))

(define (all-joiner-transforms input-predicate reached-predicates
				 path-so-far)
  (flatten-one-layer
   (map (lambda (compound-predicate)
	  (map (lambda (paths-list)
;		 (write "found way to make" compound-predicate "from" input-predicate)
		 (make-joiner-transform
		  compound-predicate
		  paths-list))
	       ;; Find all possible paths we can combine to form this
	       ;; compound-predicate
	       (crossproduct
		(map
		 (lambda (sub-predicate)
		   (if (equal? sub-predicate input-predicate)
		       (list path-so-far)
		       (hash-table-ref reached-predicates sub-predicate)))
		 compound-predicate))
	       ))
	(all-valid-compound-predicates input-predicate reached-predicates))))

(define (all-transforms-for-predicate input-predicate
				      reached-predicates path-so-far)
;  (write "ATFP" (get-name input-predicate) (map get-name (hash-table-keys reached-predicates)))
  (append
   (if (list? input-predicate)
    (all-transforms-for-compound-predicate input-predicate)
    '())
   (get-predicate-transforms input-predicate)
   (all-joiner-transforms input-predicate reached-predicates path-so-far)
   (flatten-one-layer (map get-predicate-transforms
			    (get-predicate-supers input-predicate)))))

(define (apply-all-transforms-to-predicate input-predicate transforms)
  (map
   (lambda (transform)
     ((transformation-predicate-transform transform) input-predicate))
   transforms))

(define (predicate-equal-or-supertype? pred target-pred)
    (or
      (equal? pred target-pred)
      ;; Or are any of the supertypes equal to target-pred?
      (any 
        (lambda (super-pred) 
          (predicate-equal-or-supertype? super-pred target-pred))
        (get-predicate-supers pred)
        )))

(define (get-transformations-internal input-predicate output-predicate
				      path-so-far reached-predicates
				      seen-predicates)
  (if (> (length path-so-far) 10)
      (list)
  (append
   ;; If we've hit the goal, add a "termination" to our path list, but
   ;; also keep search in case we're only actually at a subtype of our goal
   (if (predicate-equal-or-supertype? input-predicate
				      output-predicate)
       (list (list)) ;; Valid path with no more transforms needed
       (list))
   (let*
       ((transforms (all-transforms-for-predicate
		     input-predicate reached-predicates path-so-far))
	(transform-intermediates
	 (apply-all-transforms-to-predicate input-predicate
					    transforms)))
     (hash-table-set!
      reached-predicates
      input-predicate
      (cons path-so-far (hash-table/get reached-predicates
					input-predicate '())))
     
     (write "Search reached" (get-name input-predicate) "after #steps= " (length path-so-far))
     
     (map
      remove-from-path-before-joiner
      (flatten-one-layer
       ;; Loop over each of the intermediate transforms and find all
       ;; paths from there
       (map 
	(lambda (intermediate-pred transformation)
	  ;; Check we haven't already been to this predicate
	  (if (member intermediate-pred seen-predicates)
	      '()
	      (let ((new-path-so-far
		     (if (is-joiner-transform? transformation)
			 (list transformation)
			 (cons transformation path-so-far))))
		;; Recursively find all paths from this
		;; intermediate-predicate to the end-predicate
		(map (lambda (path) (cons transformation path))
		     (get-transformations-internal
		      intermediate-pred
		      output-predicate
		      new-path-so-far
		      reached-predicates
		      (cons intermediate-pred seen-predicates))))))
	transform-intermediates
	transforms)))))))

(define (get-transformations input-predicate output-predicate)
  (get-transformations-internal input-predicate output-predicate '()
				(make-equal-hash-table) (list input-predicate)))

;; ==============
;; Visualizing Transformations
;; ==============

(define (debug-get-transformations-values input-predicate
					  output-predicate
            input-value)
  (write-line "")
  (write "*********************")
  (write "*********************")
  (write "Attempting to transform" (get-name input-predicate) "to"
		    (get-name output-predicate) "and showing with value" input-value)
  (let ((paths (get-transformations input-predicate output-predicate)))
    (write "Found" (length paths) "paths:")
    (for-each (lambda (path) (print-path-data path input-predicate input-value)) paths)))

(define (print-path-data path input-predicate input-value)
  (write-line "------")
  (write-line "Output value:")
  (write-line ((create-compound-transformation path) input-value))
  (write-line "Transforms:")
     (pp ((create-compound-transformation-debugger-transforms path) input-value))
;   (write-line (path-to-data-transform-names path))
   (write-line "Predicates:")
   (pp ((create-compound-transformation-debugger-predicates path) input-value))
;   (write-line (map get-name (path-to-intermediate-predicates path input-predicate)))
   (write-line "Values:")
   (pp ((create-compound-transformation-debugger-values path) input-value)))
;   (pp (path-to-intermediate-values path input-value)))
   
(define (path-to-data-transform-names path)
  (map 
    (lambda (transform)
      (get-name (transformation-data-transform transform)))
    path))

(define (path-to-intermediate-predicates path input-predicate)
  (reverse
   (fold-left 
    (lambda (intermediate-predicates transformation)
      (cons 
       ((transformation-predicate-transform transformation) (car intermediate-predicates))
       intermediate-predicates))
    (list input-predicate)
    path)))

(define (path-to-intermediate-values path input-value)
  (reverse
  (fold-left 
   (lambda (intermediate-values transformation)
      (cons 
       (apply-transformation-data-transform transformation (car intermediate-values))
       intermediate-values))
   (list input-value)
   path)))


(define (transform-with-first-path input-predicate output-predicate input-value)
  ((create-compound-transformation (car (get-transformations input-predicate output-predicate))) 
   input-value))
  
(define (debug-transform-to input-value output-predicate)
  (write-line "")
  (write "*********************")
  (write "*********************")
  (write "Attempting to transform" input-value "to" (get-name output-predicate))
  (let*
    ((matching-predicates (filter (lambda (pred) (pred input-value)) (all-predicates)))
     (paths-by-predicate
      (map
       (lambda (input-predicate) (get-transformations input-predicate output-predicate))
       matching-predicates))
      (all-paths (flatten-one-layer paths-by-predicate)))

    (write "Found" (length all-paths) "paths:")
    (for-each 
      (lambda (paths input-predicate)
        (for-each
          (lambda (path) (print-path-data path input-predicate input-value))
          paths))
      paths-by-predicate 
      matching-predicates)))

'loaded-type-search-engine-successfully
