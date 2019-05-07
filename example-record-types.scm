
;; ===================
;; Example of Transforming Scheme Record-Types
;; ===================

(load "load.scm")
(load "main.scm")

(define (printable-string? x) (string? x))
(register-predicate! printable-string?)
(register-super! string? printable-string?)

;; ===================
;; Example: Printing data about a person
;; ===================

(define (first-name? x) (printable-string? x))
(define (last-name? x) (printable-string? x))
(define (formal-title? x) (printable-string? x))
(define (person:age? x) (number? x))

;; Define the Person record type
(define-record-type Person
  (make-person first-name last-name age)
  person?
  (first-name person:first-name)
  (last-name person:last-name)
  (age person:age))

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

;; Define the FullName record type
(define-record-type FullName
  (make-full-name first-name last-name)
  full-name?
  (first-name full-name:first-name)
  (last-name full-name:last-name))

(register-predicate! full-name?)

(register-type-transform! full-name? first-name? full-name:first-name)
(register-type-transform! full-name? last-name? full-name:last-name)
(register-type-transform! (list first-name? last-name?) full-name? make-full-name)
(define (print-full-name fn)
    (string-append 
      (full-name:first-name fn) 
      " "
      (full-name:last-name fn)))
(register-type-transform! 
  full-name?
  printable-string? 
  print-full-name )

;; Define the FormalTitleName record type
(define-record-type FormalTitleName
  (make-formal-title-name formal-title last-name)
  formal-title-name?
  (formal-title formal-title-name:formal-title)
  (last-name formal-title-name:last-name))

(register-predicate! formal-title-name?)
(register-predicate! formal-title?)

(register-type-transform! formal-title-name? formal-title? formal-title-name:formal-title)
(register-type-transform! formal-title-name? last-name? formal-title-name:last-name)
(register-type-transform! (list formal-title? last-name?) formal-title-name? make-formal-title-name)

;; Define a transformation from FirstName to FormalTitle
(define (is-female-first-name? x) #f) ;; In a real system, we would look this up in a database...

(define (first-name-to-formal-title fn)
  (if (is-female-first-name? fn) "Mrs." "Mr."))

(register-type-transform!
  first-name? 
  formal-title? 
  first-name-to-formal-title)

;; Define a transformation from FormalTitleName to PrintableString
(define (print-formal-title-name ftn)
  (string-append 
    (formal-title-name:formal-title ftn) 
      " "
    (formal-title-name:last-name ftn)))

(register-type-transform! 
  formal-title-name?
  printable-string? 
  print-formal-title-name )


;; Tests

(define gs (make-person "Gerald" "Sussman" 18))
(define fullname (make-full-name "Gerald" "Sussman"))

(debug-transform person? first-name? gs)
;; "Found 1 paths: "
;; "------"
;; "Code Gen:"
;; (define (person?-to-first-name? input)
;;   (person:first-name input))
;; "Output value:"
;; "Gerald"
;; "Transforms:"
;; (person:first-name)
;; "Predicates:"
;; (first-name?)
;; "Values:"
;; (#[person 12] . "Gerald")
;; ""

(debug-transform person? full-name? gs)
;; "Found 2 paths: "
;; "------"
;; "Code Gen:"
;; (define (person?-to-full-name? input)
;;   (make-full-name (person:first-name input) (formal-title-name:last-name (make-formal-title-name (first-name-to-formal-title (person:first-name input)) (person:last-name input)))))
;; "Output value:"
;; #[fullname 13]
;; "Transforms:"
;; (((person:first-name) (((person:first-name first-name-to-formal-title) (person:last-name)) (make-formal-title-name formal-title-name:last-name))) (make-full-name))
;; "Predicates:"
;; (((first-name?) (((first-name? formal-title?) (last-name?)) (formal-title-name? last-name?))) (full-name?))
;; "Values:"
;; (((#[person 12] . "Gerald") (((#[person 12] "Gerald" . "Mr.") (#[person 12] . "Sussman")) (("Mr." "Sussman") #[formaltitlename 15] . "Sussman"))) (("Gerald" "Sussman") . #[fullname 14]))
;; "------"
;; "Code Gen:"
;; (define (person?-to-full-name? input)
;;   (make-full-name (person:first-name input) (person:last-name input)))
;; "Output value:"
;; #[fullname 16]
;; "Transforms:"
;; (((person:first-name) (person:last-name)) (make-full-name))
;; "Predicates:"
;; (((first-name?) (last-name?)) (full-name?))
;; "Values:"
;; (((#[person 12] . "Gerald") (#[person 12] . "Sussman")) (("Gerald" "Sussman") . #[fullname 17]))
;; ""


(debug-transform person? printable-string? gs)
;; Outputs values including "Gerald", "Sussman", "Mr. Sussman", and "Gerald Sussman"