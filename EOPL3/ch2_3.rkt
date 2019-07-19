#lang racket

(define (occurs-free? var exp)
  (cond
    [(var-exp? exp) (eqv? var (var-exp->var exp))]
    [(lambda-exp? exp)
     (and
       (not (eqv? var (lambda-expr->boudn-var exp)))
       (occurs-free? var (lambda-expr->body exp)))]
    [else
      (or
        (occurs-free? var (app-exp->rator exp))
        (occurs-free? var (app-exp->rand exp)))]))
