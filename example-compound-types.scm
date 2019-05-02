;; ===================
;; Examples with compound types
;; ===================

(load "load.scm")
(load "main.scm")

(debug-get-transformations-values (list number? string?) (list string? string?) (list 1 "2"))

