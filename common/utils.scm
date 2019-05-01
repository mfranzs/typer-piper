(define (n:sign n)
  (guarantee n:real? n 'n:sign)
  (cond ((n:positive? n) +1)
        ((n:negative? n) -1)
        (else 0)))

(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (n:negate x)
  (n:- 0 x))

(define (n:invert x)
  (n:/ 1 x))

(define (compose . args)
  (compose* args))

(define (compose* args)
  (case (length args)
    ((0) (lambda (x) x))
    ((1) (car args))
    (else (reduce-right (lambda (f g)
                          (lambda (x) (f (g x))))
                        (lambda (x) x)
                        args))))

(define (~<? x y) (and (n:< x y) (not (~=? x y))))
(define (~>? x y) (and (n:> x y) (not (~=? x y))))
(define (~=? x y)
  (if (and (exact? x) (exact? y))
      (n:= x y)
      (close-enuf? x y default-equality-tolerance)))

(define default-equality-tolerance 1e-10)

(define (close-enuf? h1 h2 tolerance)
  (n:<= (n:magnitude (n:- h1 h2))
        (n:* .5
             (n:max tolerance *machine-epsilon*)
             (n:+ (n:magnitude h1)
                  (n:magnitude h2)
                  2.))))

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2.0 e)
         (loop (/ e 2.0)))))

;;;; List utilities

(define (non-empty-list? object)
  (and (n:pair? object)
       (n:list? (cdr object))))
(define n:non-empty-list? non-empty-list?)

(define (all-permutations-of items)
  (guarantee n:list? items)
  (let loop ((items items))
    (if (n:pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (elementwise-lists-of lists)
  (guarantee-list-of n:non-empty-list? lists)
  (let loop ((lists lists))
    (if (n:pair? lists)
        (append-map (let ((tails (loop (cdr lists))))
                      (lambda (head)
                        (map (lambda (tail)
                               (cons head tail))
                             tails)))
                    (car lists))
        '(()))))

(define (partition-by-key = get-key objects)
  (let ((partitions '()))
    (for-each (lambda (object)
                (let ((key (get-key object)))
                  (let ((partition
                         (find (lambda (partition)
                                 (= (car partition) key))
                               partitions)))
                    (if partition
                        (set-cdr! partition
                                  (cons object (cdr partition)))
                        (set! partitions
                              (cons (list key object)
                                    partitions))))))
              objects)
    partitions))

(define (subsuming-adjoiner set= set<=)
  (lambda (new-set current-sets)
    (if (any (lambda (current-set)
               (set<= current-set new-set))
             current-sets)
        current-sets
        (lset-adjoin set=
                     (lset-difference set=
                       current-sets
                       (filter (lambda (current-set)
                                 (set<= new-set current-set))
                               current-sets))
                     new-set))))

(define (sort-by lst compute-key)
  (map cdr
       (sort (map (lambda (thing)
                    (cons (compute-key thing) thing))
                  lst)
             (lambda (pair1 pair2)
               (n:< (car pair1) (car pair2))))))

(define (for-each-distinct-pair proc lst)
  (if (pair? lst)
      (let loop ((first (car lst)) (rest (cdr lst)))
        (for-each (lambda (other-element)
                    (proc first other-element))
                  rest)
        (if (pair? rest)
            (loop (car rest) (cdr rest))))))

(define (plist? object)
  (and (list? object)
       (even? (length object))))

(define (plist->alist plist)
  (guarantee plist? plist 'plist->alist)
  (let loop ((plist plist))
    (if (pair? plist)
        (cons (cons (car plist)
                    (cadr plist))
              (loop (cddr plist)))
        '())))

(define (all-sequences-of arity zero one)
  (map (lambda (index)
         (index->choices index arity zero one))
       (iota (n:expt 2 arity))))

(define (index->choices index arity zero one)
  (let loop ((i 0) (index index) (choices '()))
    (if (n:< i arity)
        (loop (n:+ i 1)
              (quotient index 2)
              (cons (if (odd? index) one zero)
                    choices))
        choices)))

;;; MIT/GNU Scheme implementation specific:

(define n:make-bundle-predicate
  make-bundle-predicate)

(define (make-bundle-predicate name)
  (let ((predicate (n:make-bundle-predicate name)))
    (register-predicate! predicate name)
    predicate))

(define (implementation-type-name object)
  (microcode-type/code->name (object-type object)))

(define microcode-type/code->name
  (access microcode-type/code->name (->environment '(runtime))))

(define (implementation-type-predicate name)
  (hash-table-intern! %implementation-type-predicates name
    (lambda ()
      (let ((code (microcode-type name)))
        (lambda (object)
          (object-type? code object))))))

(define %implementation-type-predicates
  (make-strong-eq-hash-table))

(define (has-implementation-value? name)
  (environment-bound? system-global-environment name))

(define (get-implementation-value name)
  (environment-lookup system-global-environment name))

(define (save-environment! name environment)
  ;; A hook to use if we want to keep track of
  ;; loaded environments.  Called by the loader.
  (declare (ignore environment))
  name)

(define load-quietly
  (cond ((environment-bound? system-global-environment
                             'param:suppress-loading-message?)
         ;; Post-9.2 parameters:
         (lambda args
           (parameterize ((param:suppress-loading-message? #t))
             (apply load args))))
        (else
         ;; Older shallow fluid bindings:
         (lambda args
           (fluid-let ((load/suppress-loading-message? #t))
             (apply load args))))))

;;;; Printing

(define (define-record-printer record-type get-parts
          #!optional get-name)
  (define-print-method (record-predicate record-type)
    (standard-print-method
        (if (default-object? get-name)
            (lambda (record)
              (record-type-name
               (record-type-descriptor record)))
            get-name)
      get-parts)))

(define (define-entity-printer record-type get-parts
          #!optional get-name)
  (define-print-method
    (let ((predicate (record-predicate record-type)))
      (lambda (object)
        (and (entity? object)
             (predicate (entity-extra object)))))
    (standard-print-method
        (if (default-object? get-name)
            get-name
            (lambda (record)
              (record-type-name
               (record-type-descriptor record))))
      get-parts)))

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)

(define (cpp x #!optional port)
  (let ((s
         (string-trim
          (call-with-output-string
            (lambda (port*)
              (fluid-let ((*pp-forced-x-size* 65))
                (pp x port*))))))
        (port
         (if (default-object? port)
             (current-output-port)
             port)))
    (fresh-line port)
    (if (string-find-next-char s #\newline)
        (begin
          (display "#|\n" port)
          (display s port)
          (display "\n|#\n" port))
        (begin
          (display "#| " port)
          (display s port)
          (display " |#\n" port)))))