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
  ((%transform-graph 'get) predicate))

(define (get-predicate-supers predicate)
  ((%supertype-graph 'get) predicate))

(define (register-type-transform input-predicate output-predicate transformation)
  (add-to-transform-graph! input-predicate
    (if (predicate? output-predicate)
      (lambda (x) output-predicate)
      output-predicate) transformation))

(define (get-transformations input-predicate output-predicate path-so-far)
  (if (eq? input-predicate output-predicate) (list (list))
  (let*
    ((transforms (get-predicate-transforms input-predicate))
     (transform-outs (map (lambda (pair) ((car pair) input-predicate))
                      transforms)))
     
    (apply append (map
      (lambda (out-type transformation)
        (if (find (lambda (pred) (eq? out-type pred)) path-so-far) '()
          (map (lambda (path) (cons transformation path))
               (get-transformations out-type output-predicate (cons out-type path-so-far)))))
      transform-outs transforms)))))

(define (create-compound-transformation path)
  (fold-left (lambda (compound-transformation transformation)
     (lambda (in) ((cdr transformation) (compound-transformation in))))
     (lambda (in) in) path))

(register-predicate! list?)
(register-predicate! number?)
(register-predicate! string?)

(register-type-transform list? number? length)
(register-type-transform number? string? number->string)
(register-type-transform string? number? string->number)

((create-compound-transformation (car (get-transformations list? list? '()))) '(1 2 3))

