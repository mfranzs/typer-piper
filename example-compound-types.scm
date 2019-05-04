;; ===================
;; Examples with compound types
;; ===================

(load "load.scm")
(load "main.scm")

; ; (register-predicate! list?)
; ; (register-predicate! number?)
; ; (register-predicate! string?)

; (register-type-transform! list? number? length)
; (register-type-transform! number? string? number->string)
; (register-type-transform! string? number? string->number)

;; ===================
;; Example 1
;; ===================

; (debug-get-transformations-values (list number? string?) (list string? string?) (list 1 "2"))

;; ===================
;; Example of a transformation whose input is a compound predicate
;; ===================

; (register-predicate! pair?)

; (register-type-transform! (list number? number?) pair? cons)

; (debug-get-transformations-values (list number? number?) pair? (list 1 1))

;; ===================
;; Example of auto-broadcasting to a compound predicate
;; ===================

(define (thing? x) (string? x))

(register-predicate! number?)
(register-predicate! thing?)
(register-predicate! string?)
(register-predicate! pair?)

(register-type-transform! number? string? number->string)
(register-type-transform! number? thing? (lambda (n) (number->string (+ 3 n))))
(register-type-transform! (list thing? string?) pair? cons)

(debug-get-transformations-values number? (list thing? string?) 1)
