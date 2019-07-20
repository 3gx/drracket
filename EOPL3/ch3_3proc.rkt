#lang racket

(require eopl)


;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)

    (expression
      ("(" expression expression ")")
      call-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))



(scan&parse "-(55, -(x,11))")
(scan&parse "if 42 then 43 else 45")
(scan&parse "
  let f = proc(x) -(x,11)
  in (f (f 77))
  ")
(scan&parse "
  let f = proc(x) -(x,11)
  in (f (f 77))
  ")
(scan&parse "
  let x = 200
  in let f = proc (z) -(z,x)
     in let x = 100
        in let g = proc (z) -(z,x)
           in -((f 1), (g 1))
           ")

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

(define (environment? x)
  (or (null? x)
      (and (pair? x)
           (symbol? (car (car x)))
           (expval? (cadr (car x)))
           (environment? (cdr x)))))


(define (init-env)
  (extend-env
    'i (num-val 1)
    (extend-env
      'v (num-val 5)
      (extend-env
        'x (num-val 10)
        (empty-env)))))

(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (env environment?)))



(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val
    (proc proc?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (error "failed to extract num" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (error "failed to extract bool" val))))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body env)
      (value-of body (extend-env var val env)))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (error "failed to extract proc" val))))

(define (run ast)
  (value-of-program ast))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (- num1 num2))))
    (zero?-exp (exp1)
      (let* ([val1 (value-of exp1 env)]
             [num1 (expval->num val1)])
        (if (zero? num1)
          (bool-val #t)
          (bool-val #f))))
    (if-exp (exp1 exp2 exp3)
      (let ([val1 (value-of exp1 env)])
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env))))
    (let-exp (var exp1 body)
      (let ([val1 (value-of exp1 env)])
        (value-of body (extend-env var val1 env))))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (let ([proc (expval->proc (value-of rator env))]
            [arg (value-of rand env)])
        (apply-procedure proc arg)))))



(define pgm1 (scan&parse "
  let x = 200
  in let f = proc (z) -(z,x)
     in let x = 100
        in let g = proc (z) -(z,x)
           in -((f 1), (g 1))
           "))
pgm1
(run pgm1)
