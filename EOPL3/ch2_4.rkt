#lang racket

(require eopl)

(define (identifier? x)
  (symbol? x))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(define (occurs-free? search-var exp)
  (cases lc-exp exp
         (var-exp (var) (eqv? var search-var))
         (lambda-exp (bound-var body)
            (and
              (not (eqv? search-var bound-var))
              (occurs-free? search-var body)))
         (app-exp (rator rand)
           (or
             (occurs-free? search-var rator)
             (occurs-free? search-var rand)))))

(occurs-free? 'x (var-exp 'x))
(occurs-free? 'x (var-exp 'y))
(define l1 (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
(occurs-free? 'x l1)
(occurs-free? 'y l1)
(occurs-free? 'x (app-exp (lambda-exp 'x (var-exp 'x))
                          (app-exp (var-exp 'x) (var-exp 'y))))
