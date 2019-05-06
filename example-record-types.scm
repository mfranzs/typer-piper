;; ===================
;; Printable strings
;; ===================

(load "load.scm")
(load "main.scm")

(define (printable-string? x) (string? x))
(register-predicate! printable-string?)
(register-super! string? printable-string?)

;; ===================
;; Example: Printing a person
;; ===================

(define-record-type Person
  (make-person first-name last-name age)
  person?
  (first-name person:first-name)
  (last-name person:last-name)
  (age person:age))

(define (first-name? x) (printable-string? x))
(define (last-name? x) (printable-string? x))
(define (formal-title? x) (printable-string? x))
(define (person:age? x) (number? x))

(register-predicate! person?)
(register-predicate! first-name?)
(register-super! first-name? printable-string?)
(register-predicate! last-name?)
(register-super! last-name? printable-string?)
(register-predicate! person:age?)
(register-super! person:age? integer?)

(register-type-transform! person? first-name?
             person:first-name)
(register-type-transform! person? last-name?
             person:last-name)
(register-type-transform! person? person:age?
             person:age)

(define-record-type FullName
  (make-full-name first-name last-name)
  full-name?
  (first-name full-name:first-name)
  (last-name full-name:last-name))

(register-predicate! full-name?)

(register-type-transform! full-name? first-name? full-name:first-name)
(register-type-transform! full-name? last-name? full-name:last-name)
(register-type-transform! (list first-name? last-name?)
                          full-name? make-full-name)
(register-type-transform! 
  full-name?
   printable-string? 
   (lambda (fn) 
    (string-append 
      (full-name:first-name fn) 
      " "
      (full-name:last-name fn))) )


(define-record-type FormalTitleName
  (make-formal-title-name formal-title last-name)
  formal-title-name?
  (formal-title formal-title-name:formal-title)
  (last-name formal-title-name:last-name))

(register-predicate! formal-title-name?)
(register-predicate! formal-title?)

(register-type-transform! formal-title-name?
                          formal-title?
                          formal-title-name:formal-title)
(register-type-transform! formal-title-name?
                          last-name?
                          formal-title-name:last-name)
(register-type-transform! full-name? formal-title-name? 
  (lambda (fn)
    (make-formal-title-name "Mr." (full-name:last-name fn))))
(register-type-transform! 
  formal-title-name?
   printable-string? 
   (lambda (ftn) 
    (string-append 
      (formal-title-name:formal-title ftn) 
        " "
      (formal-title-name:last-name ftn))) )

;; Tests

(define gs (make-person "Gerald" "Sussman" 18))
(define fullname (make-full-name "Gerald" "Sussman"))

(debug-get-transformations-values person? printable-string? gs)

; (debug-get-transformations-values person? first-name? gs)
; (debug-get-transformations-values person? last-name? gs)

; (debug-get-transformations-values person? full-name? gs)

; (debug-get-transformations-values full-name? printable-string? fullname)
