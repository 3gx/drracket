#lang racket/load
(require racket/match)
(require rnrs/mutable-pairs-6)

(define (lookup x env)
  (cdr (assq x env)))

(define (update-env! x v env)
  (if (eq? x (caar env))
    (set-cdr! (car env) v)
    (update-env! x v (cdr env))))

(define (value-of e env)
  (match e
    [`(,x) #:when (symbol? x)
        (lookup x env)]
    [`(,n) #:when (number? n)
           n]
    [`(,op ,e1 ,e2) #:when (memq op '(+ * -))
                    ((eval op) e1 e2)]
    [`(set! ,x ,e)
      (update-env! x e env)]
    [`(lambda (,x) ,e)
     (lambda (v) (value-of e (cons (cons x v) env)))]
    [`(,e1 ,e2)
     (eval (datum->syntax #'42 `(,e1 ,e2)))]))

(value-of '(((lambda (x)
                 (lambda (y)
                       (+ x y)))
               4)
              5)
            '())

(define env '((c 42) (d 43) (a 42) (f 47)))
(println (lookup 'a env))
(define val 47)
(println (update-env! 'a val env))

