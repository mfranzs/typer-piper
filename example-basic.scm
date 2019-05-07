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

; (debug-transform number? string? 1)

; (debug-transform list? string? '(1 2 3))

; (debug-transform is-three? string? 3)

; (debug-transform-to 3 string?)

(debug-transform number? symbol? 1)
