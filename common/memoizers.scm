;;;; Memoizers

(define (make-list-memoizer make-list= dedup?)
  (lambda (elt= get-key get-datum)
    (let ((table (make-hash-table (make-list= elt=))))
      (lambda (list)
        (let ((list
               (if dedup?
                   (delete-duplicates list elt=)
                   list)))
          (hash-table-intern! table
                              (get-key list)
                              (lambda () (get-datum list))))))))

(define (make-list= elt=)
  (guarantee equality-predicate? elt= 'make-list=)
  (letrec ((list=
            (lambda (a b)
              (if (n:pair? a)
                  (and (n:pair? b)
                       (elt= (car a) (car b))
                       (list= (cdr a) (cdr b)))
                  (not (n:pair? b))))))
    (set-equality-predicate-properties! list=
      (make-list-hash (equality-predicate-hasher elt=))
      (equality-predicate-rehash-after-gc? elt=))
    list=))

(define (make-lset= elt=)
  (guarantee equality-predicate? elt= 'make-list=)
  (let ((list=
         (lambda (a b)
           (lset= elt= a b))))
    (set-equality-predicate-properties! list=
      (make-list-hash (equality-predicate-hasher elt=))
      (equality-predicate-rehash-after-gc? elt=))
    list=))

(define (make-list-hash elt-hash)
  (lambda (list #!optional modulus)
    (let ((hash
           (apply n:+
                  (map (lambda (elt)
                         (elt-hash elt modulus))
                       list))))
      (if (default-object? modulus)
          hash
          (modulo hash modulus)))))

(define list-memoizer (make-list-memoizer make-list= #f))
(define lset-memoizer (make-list-memoizer make-lset= #t))

(define (make-simple-list-memoizer list-memoizer)
  (lambda (elt= get-key get-datum)
    (let ((memoizer
           (list-memoizer elt=
                          (lambda (args)
                            (apply get-key args))
                          (lambda (args)
                            (apply get-datum args)))))
      (lambda args
        (memoizer args)))))

(define simple-list-memoizer
  (make-simple-list-memoizer list-memoizer))

(define simple-lset-memoizer
  (make-simple-list-memoizer lset-memoizer))

;;; This is intended to weakly match a list of items, where each
;;; item is distinguished by eqv?, and ideally where the items
;;; themselves are held weakly.  This is kind of difficult to do
;;; without doing a bunch of implementation-specific hacking, so
;;; for now this is implemented as a strong hash.
(define (memoize-multi-arg-eqv procedure)
  (simple-list-memoizer eqv? list procedure))

(define (memoize-multi-arg-equal procedure)
  (simple-list-memoizer equal? list procedure))
