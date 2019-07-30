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



; environment
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

; global registers
(define g-exp 'uninitialized)
(define g-env 'uninitialized)
(define g-cont 'uninitialized)
(define g-val 'uninitialized)
(define g-proc1 'uninitialized)


; continuation data-type
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
    (saved-cont continuation?)]
  [try-cont
    (var symbol?)
    (handler-exp expression?)
    (env environment?)
    (cont continuation?)]
  [raise1-cont
    (saved-cont continuation)])

(define (apply-cont)
  (cases continuation g-cont
    [end-cont ()
              (begin
                (println "End-of-computation:")
                g-val)]
    [zero1-cont (saved-cont)
                (set! g-cont saved-cont)
                (set! g-val (bool-val (zero? (expval->num g-val))))
                (apply-cont)]
    [let-exp-cont (var body saved-env saved-cont)
                  (set! g-exp body)
                  (set! g-env (extend-env var g-val saved-env))
                  (set! g-cont saved-cont)
                  (value-of/k)]
    [if-test-cont (exp2 exp3 saved-env saved-cont)
                  (set! g-cont saved-cont)
                  (set! g-env saved-env)
                  (if (expval->bool g-val)
                    (set! g-exp exp2)
                    (set! g-exp exp3))
                  (value-of/k)]
    [diff1-cont (exp2 saved-env saved-cont)
                (set! g-exp exp2)
                (set! g-env saved-env)
                (set! g-cont (diff2-cont g-val saved-cont))
                (value-of/k)]
    [diff2-cont (val1 saved-cont)
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num g-val)])
                  (set! g-cont saved-cont)
                  (set! g-val (num-val (- num1 num2)))
                  (apply-cont))]
    [plus1-cont (exp2 saved-env saved-cont)
                (set! g-exp exp2)
                (set! g-env saved-env)
                (set! g-cont (plus2-cont g-val saved-cont))
                (value-of/k)]
    [plus2-cont (val1 saved-cont)
                (let ([num1 (expval->num val1)]
                      [num2 (expval->num g-val)])
                  (set! g-cont saved-cont)
                  (set! g-val (num-val (+ num1 num2)))
                  (apply-cont))]
    [mul1-cont (exp2 saved-env saved-cont)
               (set! g-exp exp2)
               (set! g-env saved-env)
               (set! g-cont (mul2-cont g-val saved-cont))
               (value-of/k)]
    [mul2-cont (val1 saved-cont)
               (let ([num1 (expval->num val1)]
                     [num2 (expval->num g-val)])
                 (set! g-cont saved-cont)
                 (set! g-val (num-val (* num1 num2)))
                 (apply-cont))]
    [rator-cont (rand saved-env saved-cont)
                (set! g-exp rand)
                (set! g-env saved-env)
                (set! g-cont (rand-cont g-val saved-cont))
                (value-of/k)]
    [rand-cont (val1 saved-cont)
               (let ([proc (expval->proc val1)])
                 (set! g-proc1 proc)
                 (set! g-cont saved-cont)
                 (apply-procedure/k))]))


(define (apply-procedure/k)
  (cases proc g-proc1
    [procedure (var body saved-env)
               (set! g-exp body)
               (set! g-env (extend-env var g-val saved-env))
               (value-of/k)]))


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
    [a-program (exp1)
               (set! g-exp exp1)
               (set! g-env (init-env))
               (set! g-cont (end-cont))
               (value-of/k)]))

(define (value-of/k)
  (cases expression g-exp
    [const-exp (num)
               (set! g-val (num-val num))
               (apply-cont)]
    [var-exp (var)
             (set! g-val (apply-env g-env var))
             (apply-cont)]
    [diff-exp (exp1 exp2)
              (set! g-exp exp1)
              (set! g-cont (diff1-cont exp2 g-env g-cont))
              (value-of/k)]
    [plus-exp (exp1 exp2)
              (set! g-exp exp1)
              (set! g-cont (plus1-cont exp2 g-env g-cont))
              (value-of/k)]
    [mul-exp (exp1 exp2)
             (set! g-exp exp1)
             (set! g-cont (mul1-cont exp2 g-env g-cont))
             (value-of/k)]
    [zero?-exp (exp1)
               (set! g-exp exp1)
               (set! g-cont (zero1-cont g-cont))
               (value-of/k)]
    [if-exp (exp1 exp2 exp3)
            (set! g-exp exp1)
            (set! g-cont (if-test-cont exp2 exp3 g-env g-cont))
            (value-of/k)]
    [let-exp (var exp1 body)
             (set! g-exp exp1)
             (set! g-cont (let-exp-cont var body g-env g-cont))
             (value-of/k)]
    [proc-exp (var body)
              (set! g-val
                  (proc-val (procedure var body g-env)))
              (apply-cont)]
    [call-exp (rator rand)
              (set! g-exp rator)
              (set! g-cont (rator-cont rand g-env g-cont))
              (value-of/k)]
    [letrec-exp (p-names b-vars p-bodies letrec-body)
                (set! g-exp letrec-body)
                (set! g-env (extend-env-rec* p-names b-vars p-bodies g-env))
                (value-of/k)]))


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
