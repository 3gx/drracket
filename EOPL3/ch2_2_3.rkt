#lang racket

(define (empty-env)
  (lambda (var)
     (error "No binding for" var)))

(define (extend-env saved-var saved-val saved-env)
  (lambda (var)
    (if (eqv? var saved-var)
      saved-val
      (apply-env saved-env var))))

(define (apply-env env var)
  (env var))

(define v1 (extend-env 'a 42 (empty-env)))
(define v2 (extend-env 'b' 43 v1))
(define v3 (extend-env 'a' 44 v2))
v3


(apply-env v3 'a)
(apply-env v2 'a)
(apply-env v3 'b)
(apply-env v3 'c)
