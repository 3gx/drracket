#lang racket

(define (occurs-free? var exp)
  (cond
    [(var-exp? exp) (eqv? var (var-exp->var exp))]
    [(lambda-exp? exp)
     (and
       (not (eqv? var (lambda-expr->bound-var exp)))
       (occurs-free? var (lambda-expr->body exp)))]
    [else
      (or
        (occurs-free? var (app-exp->rator exp))
        (occurs-free? var (app-exp->rand exp)))]))

(define (var-exp? exp)
  (symbol? exp))
(define (var-exp->var exp)
  exp)
(define (lambda-exp? exp)
  (eqv? (car exp) 'lambda))
(define (lambda-expr->bound-var exp)
  (caadr exp))
(define (lambda-expr->body exp)
  (caddr exp))
(define (app-exp->rator exp)
  (car exp))
(define (app-exp->rand exp)
  (cadr exp))

(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '((lambda (x) x) (x y)))

