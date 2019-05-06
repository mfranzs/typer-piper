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
  (hash-table/get list-predicate-lengths list-predicate-with-length? -1))

;; ===================
;; Example: duplicating list length
;; ===================

(define (duplicate-items-in-list lst)
  (apply append (list lst lst)))

(duplicate-items-in-list (list 1 2))

(register-type-transform-f!
           length-list?
     (lambda (input_type) 
	     (generate-list-predicate
        (* 2 (get-list-predicate-length input_type))))
	   duplicate-items-in-list)

(define list-len-2? (generate-list-predicate 2))
(define list-len-4? (generate-list-predicate 4))

(debug-get-transformations-values
 list-len-2?
 list-len-4?
 (list 2 3))

;; ===================
;; Example: Points
;; ===================

; (define point? (generate-list-predicate 2))

; (define (add-elements-in-lists list-a list-b)
;   (map (lambda (a b) (+ a b)) list-a list-b))

; (duplicate-items-in-list (list 1 2))

; (register-type-transform!
;     length-list?
; 	  (lambda (input_type) input_type)
; 	  add-elements-in-listsx`xxl)

; (define list-len-2? (generate-list-predicate 2))
; (define list-len-4? (generate-list-predicate 4))

; (debug-get-transformations-values
;  list-len-2?
;  list-len-4?
;  (list 2 3))
