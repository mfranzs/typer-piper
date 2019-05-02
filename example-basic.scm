;; ===================
;; Basic examples
;; ===================

(load "load.scm")
(load "main.scm")

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

(debug-transform-to 3 string?)
