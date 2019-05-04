
(define (two-crossproduct a b)
  (flatten-one-layer
     (map 
      (lambda (a-el) 
        (map 
          (lambda (b-el) (cons a-el b-el)) 
          b)) 
      a)))

(define (crossproduct lists)
  (fold-right two-crossproduct (list (list)) lists))

(define (identity x) x)

(define (flatten-one-layer list-of-lists) (apply append list-of-lists))

(define (get-name f)
  (if (list? f) 
    (map get-name f)
    (let ((matches (filter
        (lambda (el) (and (> (length el) 1) (eq? (car (cdr el)) f)))
        (environment-bindings user-initial-environment))))
      (if (>= (length matches) 1)
        (car (car matches))
        'missing-name-in-get-name-lookup))))

(define write
  (lambda args
    (write-line
     (apply string-append
	    (map (lambda (x) (string-append (string x) " ")) args)))))

(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))

(define (boolean->string val) (if val "#t" "#f"))

(define (alist:keys alist) (map car alist))

(define (always-true x) #t)
