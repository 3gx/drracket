#lang racket

(define (empty-env)
  (list 'empty-env))

(define (extend-env var val env)
  (list 'extend-env var val env))

(define (apply-env env var)
  (cond
    [(eqv? (car env) 'empty-env)
     (error "No binding for" var)]
    [(eqv? (car env) 'extend-env)
     (let ([saved-var (cadr env)]
           [saved-val (caddr env)]
           [saved-env (cadddr env)])
       (if (eqv? var saved-var)
         saved-val
         (apply-env saved-env var)))]
    [else
      (error "Bad environment: " env)]))

(define v1 (extend-env 'a 42 (empty-env)))
(define v2 (extend-env 'b' 43 v1))
(define v3 (extend-env 'a' 44 v2))
v3

(apply-env v3 'a)
(apply-env v2 'a)
(apply-env v3 'b)
(apply-env v3 'c)
