;; ===================
;; Helper for easily memoizing functions
;; ===================

(define (memoize function)

  ;; Hash table mapping arguments -> result
  (define stored-evaluations (make-equal-hash-table))
  
  (define memoize-inner
    (lambda args
      (hash-table/lookup
       stored-evaluations
       args
       (lambda (found-value) found-value)
       (lambda ()
	 ;; Stored value not found
	 (let ((result (apply function args)))
	   (hash-table-set! stored-evaluations args result)
	   result)))))
			 
  memoize-inner)

;; Tests:

(define (test x)
  (write-line (list "Evaluate" x))
  x)

(define test-memoized (memoize test))

(test-memoized 1)			; -> Prints ("Evaluate" 1)
(test-memoized 1)			; -> Immediately returns 1
(test-memoized 2)			; -> Prints ("Evaluate 2)
