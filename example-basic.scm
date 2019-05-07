;; ===================
;; Basic examples
;; ===================

(load "load.scm")
(load "main.scm")

(register-predicate! list?)
(register-predicate! number?)
(register-predicate! string?)
(register-predicate! symbol?)

(define (is-three? num) (eq? num 3))

(register-predicate! is-three?)
(register-super! is-three? number?)

(register-type-transform! list? number? length)
(register-type-transform! number? string? number->string)
(register-type-transform! string? number? string->number)
(register-type-transform! string? symbol? string->symbol)

; (debug-get-transformations-values number? string? 1)

; (debug-get-transformations-values list? string? '(1 2 3))

; (debug-get-transformations-values is-three? string? 3)

; (debug-transform-to 3 string?)

(debug-get-transformations-values number? symbol? 1)
