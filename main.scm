;; ==============
;; Type Transform Graph
;; ==============

;; Association list of input-predicate -> '(
;;   (predicate-transformation . transformation)
;;   ...
;; )
(define %transform-graph)

(define (reset-transform-graph!)
  (set! %transform-graph (make-alist-store eq?)))

(reset-transform-graph!)

(define (add-to-transform-graph! input-predicate predicate-transformation transformation)
  (register-predicate! input-predicate)
  (let*
    ((existing-transforms (get-predicate-transforms input-predicate))
     (new-transform (make-transform predicate-transformation transformation))
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

(define (register-type-transform! input-predicate output-predicate transformation)
  (add-to-transform-graph! input-predicate
    (if (predicate? output-predicate)
      (lambda (x) output-predicate)
      output-predicate) transformation))

(define (all-predicates)
  ((%transform-graph 'get-keys)))

;; ==============
;; Supertypes 
;; ==============

;; Association list of input-predicate -> '(
;;   predicate-supertype?
;; )
(define %supertype-graph)

(define (reset-supertype-graph!)
  (set! %supertype-graph (make-alist-store eq?)))

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

(define (make-transform predicate-transformation transformation)
  (cons predicate-transformation transformation))

(define (transformation-predicate-transform transformation)
  (if (list? transformation)
    (lambda (in) 
      (map
        (lambda (value transform) ((transformation-predicate-transform transform) value)) 
        in 
        transformation))
    (car transformation)))

(define (transformation-data-transform transformation)
  (if (list? transformation)
    (lambda (in) 
      (map 
        (lambda (value transform) ((transformation-data-transform transform) value)) 
        in 
        transformation))
    (cdr transformation)))

(define identity-transform (make-transform identity identity))

(define (create-compound-transformation path)
  (fold-left 
    (lambda (compound-transformation transformation)
      (lambda (in) 
        ((transformation-data-transform transformation) (compound-transformation in))))
    identity
    path))

;; ==============
;; Search Engine
;; ==============

(define (all-transforms-for-predicate input-predicate) 
  (if (list? input-predicate)
    (crossproduct 
      (map
        (lambda (pred) (cons
          identity-transform
				  (get-predicate-transforms pred)))
			  input-predicate))
		(append
		 (get-predicate-transforms input-predicate)
		 (flatten-one-layer (map get-predicate-transforms
				    (get-predicate-supers input-predicate))))))

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

(define (get-transformations-internal input-predicate output-predicate path-so-far)
  (if (predicate-equal-or-supertype? input-predicate output-predicate)
      (list (list)) 
      (let*
	      ((transforms (all-transforms-for-predicate input-predicate))
	        (transform-intermediates (apply-all-transforms-to-predicate input-predicate transforms)))
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
   (write-line "Transforms:")
   (write-line (path-to-data-transform-names path))
   (write-line "Predicates:")
   (write-line (map get-name (path-to-intermediate-predicates path input-predicate)))
   (write-line "Values:")
   (pp (path-to-intermediate-values path input-value)))
   
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
       ((transformation-data-transform transformation) (car intermediate-values))
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
