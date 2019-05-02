;; Association list of input-predicate -> '(
;;   (predicate-transformation . transformation)
;;   ...
;; )
(define %transform-graph)

;; Association list of input-predicate -> '(
;;   predicate-supertype?
;; )
(define %supertype-graph)

(define (reset-transform-graph!)
  (set! %transform-graph (make-alist-store eq?)))

(define (reset-supertype-graph!)
  (set! %supertype-graph (make-alist-store eq?)))

(reset-transform-graph!)
(reset-supertype-graph!)

(define (register-super! predicate-sub predicate-super)
  (register-predicate! predicate-sub)
  (register-predicate! predicate-super)

  ((%supertype-graph 'put!) predicate-sub
    (cons predicate-super (get-predicate-supers predicate-sub))))

(define (add-to-transform-graph! input-predicate predicate-transformation transformation)
  (register-predicate! input-predicate)
  (let*
    ((existing-transforms (get-predicate-transforms input-predicate))
     (new-transform (cons predicate-transformation transformation))
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

(define (get-predicate-supers predicate)
  ((%supertype-graph 'get) predicate))

(define (register-type-transform! input-predicate output-predicate transformation)
  (add-to-transform-graph! input-predicate
    (if (predicate? output-predicate)
      (lambda (x) output-predicate)
      output-predicate) transformation))

(define transformation-predicate-transform car)
(define transformation-data-transform cdr)

(define (get-transformations input-predicate output-predicate)
  (get-transformations-internal input-predicate output-predicate '()))

(define (two-crossproduct a b)
  (apply append (map (lambda (a-el) (map (lambda (b-el) (cons a-el b-el)) b)) a)))

(define (crossproduct lists)
  (fold-right two-crossproduct (list (list)) lists))

(define (identity x) x)

(define identity-transform (cons identity identity))

(define (get-transformations-internal input-predicate output-predicate path-so-far)
  (if (equal? input-predicate output-predicate)
      (list (list)) 
      (let*
	  ((transforms
	    (if (list? input-predicate)
		(crossproduct (map
			       (lambda (pred) (cons
					       identity-transform
					       (get-predicate-transforms pred)))
			       input-predicate))
      
		(append
		 (get-predicate-transforms input-predicate)
		 (apply append (map get-predicate-transforms
				    (get-predicate-supers input-predicate))))))
	   
	   (transform-intermediates
	    (if (list? input-predicate)
		(map (lambda (compound-transform)
		       ;; Go through each transfom in the compound
		       ;; transform and transform the corresponding
		       ;; predicate
		       (map
			(lambda (transform predicate)
			  ((transformation-predicate-transform transform) predicate))
			compound-transform
			input-predicate))
		     transforms)

		(map
		 (lambda (transform)
		   ((transformation-predicate-transform transform) input-predicate))
		 transforms))))
	
	(apply
	 append
	 (map
	  (lambda (intermediate-pred transformation)
	    (if (find (lambda (pred) (equal? pred intermediate-pred)) path-so-far)
		'()
		(map (lambda (path) (cons transformation path))
		     (get-transformations-internal
		      intermediate-pred
		      output-predicate
		      (cons intermediate-pred path-so-far)))))
	  transform-intermediates transforms)))))

(define (get-transformations input-predicate output-predicate)
  (get-transformations-internal input-predicate output-predicate '()))

(define identity (lambda (x) x))

(define (get-transformation-f transformation)
  (if (list? transformation)
    (lambda (in) (map (lambda (value transform) ((transformation-data-transform transform) value)) in transformation))
    (transformation-data-transform transformation)))

(define (create-compound-transformation path)
  (fold-left 
    (lambda (compound-transformation transformation)
      (lambda (in) 
        ((get-transformation-f transformation) (compound-transformation in))))
    identity
    path))

;; ==============
;; Visualizing Transformations
;; ==============

(define write
  (lambda args
    (write-line
     (apply string-append
	    (map (lambda (x) (string-append (string x) " ")) args)))))

(define (debug-get-transformations input-predicate output-predicate)
  (let ((paths (get-transformations-internal input-predicate output-predicate '())))
      (map 
       (lambda (path)
	 (path-to-intermediate-predicates path input-predicate))
       paths)))

(define (debug-get-transformations-values input-predicate
					  output-predicate
					  input-value)
  (write "------")
  (write "Attempting to transform" (get-name input-predicate) "to"
		    (get-name output-predicate) "and showing with value" input-value)
  (let ((paths (get-transformations-internal input-predicate
					    output-predicate '())))
    (write "Found" (length paths) "paths:")
      (for-each 
       (lambda (path)
	 (write-line "")
	 (write-line (path-to-intermediate-predicates path input-predicate))
	 (write-line
	  (path-to-intermediate-values path input-value)))
       paths)))

(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))

(define (path-to-intermediate-predicates path input-predicate)
  (reverse
   (fold-left 
    (lambda (intermediate-predicates transformation)
      (cons 
       (get-name ((transformation-predicate-transform transformation) (car intermediate-predicates)))
       intermediate-predicates))
    (list (get-name input-predicate))
    path)))

(define (path-to-intermediate-values path input-value)
  (reverse
  (fold-left 
   (lambda (intermediate-values transformation)
      (cons 
       ((transformation-data-transform transformation) (car intermediate-values))
       intermediate-values))
   (list input-value)
   path)))

(define (transform input-predicate output-predicate input-value)
  ((create-compound-transformation (car (get-transformations input-predicate output-predicate))) 
   input-value))

(define (get-name f)
  (let ((matches (filter
      (lambda (el) (and (> (length el) 1) (eq? (car (cdr el)) f)))
      (environment-bindings user-initial-environment))))
    (if (>= (length matches) 1)
      (car (car matches))
      #f)))

;; ==============
;; Tests
;; ==============

(register-predicate! list?)
(register-predicate! number?)
(register-predicate! string?)

(define (is-three? num) (eq? num 3))

(register-predicate! is-three?)
(register-super! is-three? number?)

(register-type-transform! list? number? length)
(register-type-transform! number? string? number->string)
(register-type-transform! string? number? string->number)

(debug-get-transformations-values list? string? '(1 2 3))

(define (get-name f)
  (let ((matches (filter
      (lambda (el) (and (> (length el) 1) (eq? (car (cdr el)) f)))
      (environment-bindings user-initial-environment))))

    (if (>= (length matches) 1)
      (car (car matches))
      #f)))

(debug-get-transformations-values is-three? string? 3)

(transform (list number? string?) (list string? string?) (list 1 "2"))

