;; ===================
;; List predicates with certain lengths
;; ===================

(load "load.scm")
(load "main.scm")
(load "memoize.scm")

;; a length-list is just a list whose type describes its length
(define (length-list? x) (list? x))

(register-predicate! length-list?)
(register-super! length-list? list?)

;; generate a new length-list? predicate with a specific length

(define generate-list-predicate
  (memoize
   (lambda (length)
     (define (list-predicate-with-length? item)
       (and 
	(length-list? item)
	(= length item)))
     
     (register-list-predicate-length! list-predicate-with-length? length)
     (register-super! list-predicate-with-length? length-list?)
     
     list-predicate-with-length?)))

;; hash table for storing lengths associated with length-list? predicates
(define list-predicate-lengths (make-strong-eq-hash-table)) 

(define (register-list-predicate-length! list-predicate-with-length? length)
  (hash-table-set! list-predicate-lengths list-predicate-with-length? length))

(define (get-list-predicate-length list-predicate-with-length?)
  (hash-table-ref list-predicate-lengths list-predicate-with-length?))

;; ===================
;; example: duplicating list length
;; ===================

(define (duplicate-items-in-list list)
  (cons list list))

(register-type-transform!
           length-list?
	   (lambda (input_type) 
	     (generate-list-predicate (* 2 (get-list-predicate-length input_type))))
	   duplicate-items-in-list)

(debug-get-transformations-values
 (generate-list-predicate 2)
 (generate-list-predicate 4)
 (list 2 3))
