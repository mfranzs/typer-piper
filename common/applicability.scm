;;;; Applicability

;;; An applicability attribute is a list of lists, representing
;;; an OR of some per-argument ANDs.

(define (applicability? object)
  (and (n:list? object)
       (every (lambda (pattern)
                (and (n:list? pattern)
                     (every procedure? pattern)))
              object)
       (or (not (n:pair? object))
           (let ((arity (length (car object))))
             (every (lambda (pattern)
                      (n:= arity (length pattern)))
                    (cdr object))))))

(define (applicability-arity applicability)
  (guarantee applicability? applicability)
  (if (n:pair? applicability)
      (length (car applicability))
      0))

(define (is-applicable? applicability args)
  (any (lambda (and-clause)
         (predicates-match? and-clause args))
       applicability))

(define (predicates-match? predicates args)
  (and (n:= (length predicates) (length args))
       (every (lambda (predicate arg)
                (increment-predicate-count! predicate)
                (predicate arg))
              predicates
              args)))

(define (match-args . predicates)
  (list predicates))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-arg arity predicate base-predicate)
  (if (n:= 0 arity)
      (list)
      (cdr (all-sequences-of arity base-predicate predicate))))

(define (applicability-union . applicabilities)
  (applicability-union* applicabilities))

(define (applicability-union* applicabilities)
  (apply lset-union equal? applicabilities))
