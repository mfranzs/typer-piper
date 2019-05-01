(let ((env (make-top-level-environment)))
  (with-working-directory-pathname
   (directory-pathname (current-load-pathname))
   (lambda ()
     (load
      '("common/overrides"
        "common/utils"
        "common/collections"
        "common/memoizers"
        "common/predicates"
        "common/predicate-metadata"
        "common/applicability"
        "common/generic-procedures"
        "common/predicate-counter"
        "common/simple-tests"
        "common/trie")
      env)))
  (environment-define system-global-environment 'current-book-environment env)
  (ge env))
