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
(define l2 (app-exp (lambda-exp 'x (var-exp 'x))
                     (app-exp (var-exp 'x) (var-exp 'y))))
(occurs-free? 'x l2)

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (error "Invalid syntax" datum)))))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list
                (unparse-lc-exp rator) (unparse-lc-exp rand))))))

l1
(unparse-lc-exp l1)
l2
(unparse-lc-exp l2)

(parse-expression '((lambda (a) (a b)) c))
(parse-expression '(lambda (x)
                     (lambda (y)
                       ((lambda (x)
                          (x y))
                        x))))
