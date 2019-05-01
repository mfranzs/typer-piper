;; ===================
;; Printable strings
;; ===================

(define printable-string? string?)
(register-predicate! printable-string?)
(register-supertype! string? printable-string?)

;; ===================
;; Predicates for record types
;; ===================

;(defined-typed-record-type
 ; 'person '(first-name? last-name? age?))(st

;; ===================
;; Example: Printing a person
;; ===================

(define-record-type Person
  (make-person first-name last-name age)
  person?
  (first-name person:first-name)
  (last-name person:last-name)
  (age person:age))

(define person:first-name? printable-string?)
(define person:last-name? printable-string?)
(define person:age? number?)

(register-type-transformation! person? person:first-name?
			       person:first-name)
(register-type-transformation! person? person:last-name?
			       person:last-name)
(register-type-transformation! person? person:age?
			       person:age)

(define-record-type FullName
  (make-full-name first-name last-name)
  full-name?
  (first-name person:first-name)
  (last-name person:last-name))

(define gs (make-person "Gerald" "Sussman" 18))
(person:first-name gs)

(get-transformations person? printable-string?)
(

(apply append
	     (list
	      (list 'a 'b 'c)
	      (list 'z)
	      (list 'd 'e 'f)
	      (list)))
