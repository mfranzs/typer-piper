;;;; Simple in-line tests

;;; The idea behind this tester is that it executes a file of
;;; expressions, in order, except that some of the expressions
;;; will be annotated with "expectations" that must be satisfied
;;; by the evaluation of the corresponding expression.

;;; For example,
;;;
;;; (fib 20)
;;; expect-value: 6765
;;;
;;; is a trivial example.  There are also expectations involving
;;; printed output, and perhaps others as we develop the idea.

(define *test-eval*
  (lambda (expr)
    (eval expr (nearest-repl/environment))))

(define (load-inline-test filename)
  (summarize-test-results
   (with-notification
    (lambda (port)
      (display "Loading test: " port)
      (write (->namestring filename) port))
    (lambda ()
      (execute-grouped-expressions
       (group-expressions
        (read-test-expressions filename)))))))

(define (read-test-expressions filename)
  (read-file (pathname-default-type filename "scm")))

(define (group-expressions exprs)
  (if (pair? exprs)
      (let ((to-eval (car exprs)))
        (let ((r (parse-expectations (cdr exprs))))
          (let ((expectations (car r))
                (rest (cadr r)))
            (cons (cons to-eval (reverse expectations))
                  (group-expressions rest)))))
      '()))

(define (parse-expectations exprs)
  (let ((r (parse-expectation exprs)))
    (if r
        (let ((expectation (car r))
              (rest (cadr r)))
          (let ((r* (parse-expectations rest)))
            (list (cons expectation (car r*))
                  (cadr r*))))
        (list (list) exprs))))

(define (parse-expectation exprs)
  (let loop ((rules expectation-rules))
    (and (pair? rules)
         (or (match-head (car rules) exprs)
             (loop (cdr rules))))))

(define (match-head rule exprs)
  (let ((keyword (expectation-rule-keyword rule))
        (n-args (expectation-rule-n-args rule))
        (handler (expectation-rule-handler rule)))
    (and (pair? exprs)
         (is-quotation? (car exprs))
         (eq? (quotation-text (car exprs)) keyword)
         (n:>= (length (cdr exprs)) n-args)
         (let ((tail (cdr exprs)))
           (let ((args (list-head tail n-args))
                 (rest (list-tail tail n-args)))
             (list (cons handler
                         (map (lambda (expr)
                                (if (is-quotation? expr)
                                    (quotation-text expr)
                                    expr))
                              args))
                   rest))))))

(define (is-quotation? object)
  (and (pair? object)
       (eq? (car object) 'quote)
       (pair? (cdr object))
       (null? (cddr object))))

(define (quotation-text expr)
  (cadr expr))

(define (define-expectation-rule keyword n-args handler)
  (let ((rule (make-expectation-rule keyword n-args handler))
        (tail
         (find-tail (lambda (rule)
                      (eq? keyword
                           (expectation-rule-keyword rule)))
                    expectation-rules)))
    (if tail
        (set-car! tail rule)
        (set! expectation-rules
              (cons rule
                    expectation-rules)))))

(define expectation-rules
  '())

(define (make-expectation-rule keyword n-args handler)
  (list 'expectation-rule keyword n-args handler))

(define expectation-rule-keyword cadr)
(define expectation-rule-n-args caddr)
(define expectation-rule-handler cadddr)

;;; Lots or hair here to let the test driver deal with
;;; "interesting" uses of continuations.  In particular, the
;;; state of the driver is moved outside of the control
;;; structure, so that if there are multiple returns from
;;; evaluating an expression, the "current" expectations are used
;;; for each.

(define *groups-to-test*)
(define *current-group*)
(define *test-results*)

(define (execute-grouped-expressions groups)
  (fluid-let ((*groups-to-test* groups)
              (*current-group*)
              (*test-results* '())
              (cpp pp))
    (let loop ()
      (if (pair? *groups-to-test*)
          (begin
            (set! *current-group* (car *groups-to-test*))
            (set! *groups-to-test* (cdr *groups-to-test*))
            (set! *test-results*
                  (cons (execute-grouped-expression)
                        *test-results*))
            (loop))))
    (reverse *test-results*)))

(define (execute-grouped-expression)
  (maybe-show-test-expression (car *current-group*)
    (lambda ()
      (let ((output-port (open-output-string)))
        (let ((value
               (with-output-to-port output-port
                 (lambda ()
                   (*test-eval* (car *current-group*))))))
          (let ((context
                 (make-expectation-context
                  (get-output-string! output-port)
                  value)))
            (cons (car *current-group*)
                  (filter-map (lambda (expectation)
                                (apply (car expectation)
                                       context
                                       (cdr expectation)))
                              (cdr *current-group*)))))))))

(define show-test-expressions? #f)

(define (maybe-show-test-expression expr thunk)
  (if show-test-expressions?
      (with-notification
       (lambda (port)
         (display "evaluate: " port)
         (display
          (cdr
           (call-with-truncated-output-string 50
             (lambda (port)
               (write expr port))))
          port))
       thunk)
      (thunk)))

(define (make-expectation-context output value)
  (let ((port (open-input-string output)))
    (define (get-port) port)
    (define (get-value) value)
    (bundle expectation-context? get-port get-value)))

(define expectation-context?
  (make-bundle-predicate 'expectation-context))

(define (summarize-test-results results)
  (let ((failures
         (filter (lambda (result)
                   (pair? (cdr result)))
                 results)))
    (fresh-line)
    (display "Ran ")
    (write (length results))
    (display " test")
    (if (not (n:= 1 (length results)))
        (display "s"))
    (display "; ")
    (write (length failures))
    (display " failure")
    (if (not (n:= 1 (length failures)))
        (display "s"))
    (for-each summarize-failing-result failures)))

(define (summarize-failing-result failure)
  (newline)
  (newline)
  (display "evaluating ")
  (newline)
  (pp (car failure))
  (display "failed the following expectations:")
  (newline)
  (for-each (lambda (error)
              (display error)
              (newline))
            (cdr failure)))

(define (read-written-value context written-value)
  (let ((x
         (ignore-errors
          (lambda ()
            (read (context 'get-port))))))
    (cond ((condition? x)
           (string-append "expected to see output "
                          (write-to-string written-value)
                          " but it did not appear"))
          ((not (equal? x written-value))
           (string-append "expected to see output "
                          (write-to-string written-value)
                          "\nbut instead saw "
                          (write-to-string x)))
          (else #f))))

(define-expectation-rule 'expect-write: 1
  read-written-value)

(define-expectation-rule 'expect-description: 1
  (lambda (context description)
    (read-line (context 'get-port))     ;discard printed object
    (let loop ((description description))
      (if (pair? description)
          (or (read-written-value context (car description))
              (loop (cdr description)))
          #f))))

(define-expectation-rule 'expect-value: 1
  (lambda (context value)
    (if (equal? (context 'get-value) value)
        #f
        (string-append "expected value\n"
                       (write-to-string value)
                       "\nbut instead got value\n"
                       (write-to-string (context 'get-value))))))
