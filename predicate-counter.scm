;;;; Log number of uses of registered predicates

(define %predicate-counts)

(define (reset-predicate-counts!)
  (set! %predicate-counts (make-eqv-hash-table)))

(reset-predicate-counts!)

(define (increment-predicate-count! predicate)
  (hash-table-update! %predicate-counts
                      predicate
                      (lambda (count) (fix:+ count 1))
                      (lambda () 1)))

(define (get-predicate-count predicate)
  (hash-table-ref/default %predicate-counts predicate 0))

(define (get-predicate-counts)
  (hash-table->alist %predicate-counts))

(define (with-predicate-counts thunk)
  (fluid-let ((%predicate-counts (make-eqv-hash-table)))
    (let ((value (thunk)))
      (for-each (lambda (p)
                  (write-line (list (cdr p)
                                    (or (predicate-name (car p))
                                        (car p)))
                              (notification-output-port)))
                (get-predicate-counts))
      value)))
