; (if (not (environment-bound? system-global-environment 'our-env))
;   (load
;     '("common/overrides"
;       "common/utils"
;       "common/collections"
;       "common/memoizers"
;       "common/applicability"
;       "common/simple-tests"
;       "common/trie"
;       "helpers"))
; )
; (define our-env (make-top-level-environment))
; ; (load
; ;   '("common/overrides"
; ;     "common/utils"
; ;     "common/collections"
; ;     "common/memoizers"
; ;     "common/applicability"
; ;     "common/simple-tests"
; ;     "common/trie")
; ;   our-env)
; (environment-define system-global-environment 'our-env our-env)
; (ge our-env)

 (load
    '("common/overrides"
      "common/utils"
      "common/collections"
      "common/memoizers"
      "common/applicability"
      "common/simple-tests"
      "common/trie"
      "helpers"))