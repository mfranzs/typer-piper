;; ===================
;; Examples with compound types
;; ===================

(load "load.scm")
(load "main.scm")

;; ===================
;; Example of applying transformations in parallel on a compound type.
;; ===================

(register-predicate! list?)
(register-predicate! number?)
(register-predicate! string?)

(register-type-transform! string? number? string->number)

(debug-transform (list number? string?) (list number? number?) (list 1 "2"))

;; ===================
;; Example of a transformation whose input is a compound predicate.
;; ===================

; (register-predicate! pair?)

; (register-type-transform! (list number? number?) pair? cons)

; (debug-transform (list number? number?) pair? (list 1 1))

;; ===================
;; Example of auto-broadcasting to a compound predicate.
;; ===================

(define (thing? x) (string? x))

(register-predicate! number?)
(register-predicate! thing?)
(register-predicate! string?)
(register-predicate! pair?)

(define (add-three n) (number->string (+ 3 n)))

(register-type-transform! number? string? number->string)
(register-type-transform! number? thing? add-three)
(register-type-transform! (list thing? string?) pair? cons)

(debug-transform number? (list thing? string?) 1)
