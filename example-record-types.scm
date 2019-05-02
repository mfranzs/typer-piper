;; ===================
;; Printable strings
;; ===================

(load "load.scm")
(load "main.scm")

(define (printable-string? x) (string? x))
(register-predicate! printable-string?)
(register-super! string? printable-string?)

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

(define (person:first-name? x) (printable-string? x))
(define (person:last-name? x) (printable-string? x))
(define (person:age? x) (number? x))

(register-predicate! person:first-name?)
(register-super! person:first-name? printable-string?)
(register-predicate! person:last-name?)
(register-super! person:last-name? printable-string?)
(register-predicate! person:age?)
(register-super! person:age? integer?)

(register-type-transform! person? person:first-name?
			       person:first-name)
(register-type-transform! person? person:last-name?
			       person:last-name)
(register-type-transform! person? person:age?
			       person:age)

(define-record-type FullName
  (make-full-name first-name last-name)
  full-name?
  (first-name full-name:first-name)
  (last-name full-name:last-name))

(define gs (make-person "Gerald" "Sussman" 18))

(debug-get-transformations-values person? printable-string? gs)

