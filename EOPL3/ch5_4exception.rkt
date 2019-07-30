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
     ("+" "(" expression "," expression ")")
     plus-exp)
    (expression
     ("*" "(" expression "," expression ")")
     mul-exp)

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

    (expression
      ("letrec"
       (arbno identifier "(" identifier ")" "=" expression)
       "in" expression)
      letrec-exp)

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

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val expval?)
    (env environment?))
  (extend-env-rec*
    (p-names (list-of symbol?))
    (b-vars (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)))

(define (apply-env env search-var)
  (cases environment env
    [empty-env ()
      (error "No binding for" search-var)]
    [extend-env (saved-var saved-val saved-env)
      (if (eqv? saved-var search-var)
        saved-val
        (apply-env saved-env search-var))]
    [extend-env-rec* (p-names b-vars p-bodies saved-env)
      (cond
        [(location search-var p-names)
         => (lambda (n)
              (proc-val
                (procedure
                  (list-ref b-vars n)
                  (list-ref p-bodies n)
                  env)))]
        [else
          (apply-env saved-env search-var)])]))

(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n)
            (+ n 1)))
      (else #f))))

(define-datatype continuation continuation?
  [end-cont]
  [zero1-cont
    (saved-cont continuation?)]
  [let-exp-cont
    (var symbol?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [diff1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [diff2-cont
    (val1 expval?)
    (saved-cont continuation?)]
  [plus1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [plus2-cont
    (val1 expval?)
    (saved-cont continuation?)]
  [mul1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [mul2-cont
    (val1 expval?)
    (saved-cont continuation?)]
  [rator-cont
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?)]
  [rand-cont
    (val1 expval?)
    (saved-cont continuation?)])

(define (apply-cont cont val)
  (cases continuation cont
    [end-cont ()
              (begin
                (println (format "End-of-computation: ~a" val))
                (println ""))]
    [zero1-cont (saved-cont)
                (apply-cont saved-cont
                            (bool-val
                              (zero? (expval->num val))))]
    [let-exp-cont (var body saved-env saved-cont)
                  (value-of/k body
                              (extend-env var val saved-env)
                              saved-cont)]
    [if-test-cont (exp2 exp3 saved-env saved-cont)
                  (if (expval->bool val)
                    (value-of/k exp2 saved-env saved-cont)
                    (value-of/k exp3 saved-env saved-cont))]
    [diff1-cont (exp2 saved-env saved-cont)
                (value-of/k exp2 saved-env
                            (diff2-cont val saved-cont))]
    [diff2-cont (val1 saved-cont)
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num val)])
                  (apply-cont saved-cont
                              (num-val (- num1 num2))))]
    [plus1-cont (exp2 saved-env saved-cont)
                (value-of/k exp2 saved-env
                            (plus2-cont val saved-cont))]
    [plus2-cont (val1 saved-cont)
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num val)])
                  (apply-cont saved-cont
                              (num-val (+ num1 num2))))]
    [mul1-cont (exp2 saved-env saved-cont)
                (value-of/k exp2 saved-env
                            (mul2-cont val saved-cont))]
    [mul2-cont (val1 saved-cont)
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num val)])
                  (apply-cont saved-cont
                              (num-val (* num1 num2))))]
    [rator-cont (rand saved-env saved-cont)
                (value-of/k rand saved-env
                            (rand-cont val saved-cont))]
    [rand-cont (val1 saved-cont)
               (let ([proc (expval->proc val1)])
                 (apply-procedure/k proc val saved-cont))]))


(define (apply-procedure/k proc1 val cont)
  (cases proc proc1
    [procedure (var body saved-env)
               (value-of/k body
                           (extend-env var val saved-env)
                           cont)]))


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

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (error "failed to extract proc" val))))


(define (run str ast)
  (println (format "~a:~a" str (value-of-program ast))))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of/k exp1 (init-env) (end-cont)))))

(define (value-of/k exp env cont)
  (cases expression exp
    [const-exp (num) (apply-cont cont (num-val num))]
    [var-exp (var) (apply-cont cont (apply-env env var))]
    [diff-exp (exp1 exp2)
              (value-of/k exp1 env
                          (diff1-cont exp2 env cont))]
    [plus-exp (exp1 exp2)
              (value-of/k exp1 env
                          (plus1-cont exp2 env cont))]
    [mul-exp (exp1 exp2)
              (value-of/k exp1 env
                          (mul1-cont exp2 env cont))]
    [zero?-exp (exp1)
               (value-of/k exp1 env
                           (zero1-cont cont))]
    [if-exp (exp1 exp2 exp3)
            (value-of/k exp1 env
                        (if-test-cont exp2 exp3 env cont))]
    [let-exp (var exp1 body)
             (value-of/k exp1 env
                         (let-exp-cont var body env cont))]
    [proc-exp (var body)
              (apply-cont cont
                  (proc-val (procedure var body env)))]
    [call-exp (rator rand)
              (value-of/k rator env
                          (rator-cont rand env cont))]
    [letrec-exp (p-names b-vars p-bodies letrec-body)
                (value-of/k letrec-body
                            (extend-env-rec* p-names b-vars p-bodies env)
                            cont)]))

(define spgm0 (scan&parse "-(55, -(x,11))"))
spgm0
(run 'spgm0 spgm0)
(define spgm1 (scan&parse "if zero?(42) then 43 else 45"))
spgm1
(run 'spgm1 spgm1)
(define spgm2 (scan&parse "
  let f = proc(x) -(x,11)
  in (f (f 77))
  "))
spgm2
(run 'spgm2 spgm2)
(define spgm3 (scan&parse "
  let f = proc(x) -(x,11)
  in (f (f 77))
  "))
spgm3
(run 'spgm3 spgm3)
(define spgm4 (scan&parse "
  let x = 200
  in let f = proc (z) -(z,x)
     in let x = 100
        in let g = proc (z) -(z,x)
           in -((f 1), (g 1))
           "))
spgm4
(run 'spgm4 spgm4)

(define ast1
  (scan&parse "
    letrec double(x)
       = if zero?(x) then 0 else -((double -(x,1)), -2)
    in (double 6)
    "))
ast1
(run 'ast1 ast1)

(define ast2
  (scan&parse "
      letrec even(x) =
                if zero?(x) then 1 else (odd -(x,1))
              odd(x) =
                if zero?(x) then 0 else (even -(x,1))
         in (odd 13)
  "))
ast2
(run 'ast2 ast2)

(define ast3
  (scan&parse "
    letrec fact(n) =
       if zero?(n) then 1 else *(n, (fact -(n,1)))
    in (fact 5)
  "))
ast3
(run 'ast3 ast3)

(define ast4
  (scan&parse "
    letrec fix(f) =
      letrec d(x) = proc(z) ((f (x x)) z)
      in proc(n) ((f (d d)) n)
    in letrec t4m(f) =
          proc(x) if zero?(x) then 0 else +((f -(x,1)),4)
       in let times4 = (fix t4m)
          in (times4 3)"
     ))
ast4
(run 'ast4 ast4)

(define ast4a
  (scan&parse "
    letrec fix(f) =
      letrec d(x) = (f (x x))
      in (f (d d))
    in letrec t4m(f) =
          proc(x) if zero?(x) then 0 else +((f -(x,1)),4)
       in let times4 = (fix t4m)
          in (times4 3)"
     ))
ast4a
;(run ast4a) - runs infinitely
