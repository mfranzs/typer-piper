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

(define (get-transformations-internal input-predicate output-predicate path-so-far)
  (if (eq? input-predicate output-predicate) (list (list))
  (let*
    ((transforms (append
       (get-predicate-transforms input-predicate)
       (apply append (map get-predicate-transforms
         (get-predicate-supers input-predicate)))))

    (transform-outs (map
      (lambda (pair) ((car pair) input-predicate))
      transforms)))
     
    (apply append (map
      (lambda (out-type transformation)
        (if (find (lambda (pred) (eq? out-type pred)) path-so-far) '()
          (map (lambda (path) (cons transformation path))
               (get-transformations-internal out-type output-predicate (cons out-type path-so-far)))))
      transform-outs transforms)))))

(define (get-transformations input-predicate output-predicate)
  (get-transformations-internal input-predicate output-predicate '()))

(define identity (lambda args args))

(define (create-compound-transformation path)
  (fold-left 
    (lambda (compound-transformation transformation)
      (lambda (in) 
        ((cdr transformation) (compound-transformation in))))
    identity
    path))

;; ==============
;; Visualizing Transformations
;; ==============

(define (debug-get-transformations input-predicate output-predicate)
  (let ((paths (get-transformations-internal input-predicate output-predicate '())))
      (write-line paths)
      (map 
        (lambda (path) (path-to-intermediate-predicates path input-predicate)) paths)))

(define (debug-get-transformations-values input-predicate
					  output-predicate input-value)
  (let ((paths (get-transformations-internal input-predicate output-predicate '())))
      (write-line paths)
      (map 
        (lambda (path) (path-to-intermediate-values path input-value)) paths)))

(define (path-to-intermediate-predicates path input-predicate) 
  (fold-left 
    (lambda (intermediate-predicates transformation)
      (cons 
        ((transformation-predicate-transform transformation) (car intermediate-predicates))
        intermediate-predicates))
    (list input-predicate)
    path))

(define (path-to-intermediate-values path input-value) 
  (fold-left 
   (lambda (intermediate-values transformation)
     (write-line  (car intermediate-values))
      (cons 
        ((transformation-data-transform transformation) (car intermediate-values))
        intermediate-values))
    (list input-value)
    path))

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

(debug-get-transformations-values is-three? string? 3)
