;; =================
;; 6.905 Final Project
;; Automating Data Structure Transformations Through Type Chaining
;; Joshua Gruenstein and Martin Schneider
;; =================

;; REFERENCE: How to make a generic procedure:
(define g:analyze
  (simple-generic-procedure 'g:analyze 1 default-analyze))

(define-generic-procedure-handler g:analyze
  (match-args person?)
  analyze-if-fail)

(register-transformation person? person->
