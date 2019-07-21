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
       identifier "(" identifier ")" "=" expression
       "in" expression)
      letrec-exp)

    (expression ("%nameless-var" number) nameless-var-exp)
    (expression
      ("%let" expression "in" expression)
      nameless-let-exp)
    (expression
      ("%lexproc" expression)
      nameless-proc-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

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

;(define (apply-procedure proc1 val)
;  (cases proc proc1
;    (procedure (var body env)
;      (value-of body (extend-env var val env)))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (error "failed to extract proc" val))))

;(define (run ast)
;  (value-of-program ast))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-nameless-env)))))

(define (empty-nameless-env) '())

(define (init-nameless-env)
  (extend-nameless-env
    (num-val 1)
    (extend-nameless-env
      (num-val 5)
      (extend-nameless-env
        (num-val 10)
        (empty-nameless-env)))))

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-name symbol?)
    (b-var symbol?)
    (body expression?)
    (env environment?)))
(define (apply-env env search-var)
  (cases environment env
    (empty-env ()
      (error "No binding for" search-var))
    (extend-env (saved-var saved-val saved-env)
      (if (eqv? saved-var search-var)
        saved-val
        (apply-env saved-env search-var)))
    (extend-env-rec (p-name b-var p-body saved-env)
      (if (eqv? p-name search-var)
        (proc-val (procedure b-var p-body env))
        (apply-env saved-env search-var)))))

(define (init-env)
  (extend-env
    'i (num-val 1)
    (extend-env
      'v (num-val 5)
      (extend-env
        'x (num-val 10)
        (empty-env)))))


(define (nameless-environment? x)
  ((list-of expval?) x))

(define (extend-nameless-env val env)
  (cons val env))

(define (apply-nameless-env env n)
  (list-ref env n))

(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    [procedure (body saved-nameless-env)
      (value-of body
                (extend-nameless-env val saved-nameless-env))]))



(define (empty-senv) '())
(define (extend-senv var senv)
  (cons var senv))
(define (apply-senv senv var)
  (cond
    [(null? senv)
     (error "Unbound variable" var)]
    [(eqv? var (car senv)) 0]
    [else
      (+ 1 (apply-senv (cdr senv) var))]))

(apply-senv '(z y x) 'x)
(apply-senv '(z y x) 'y)
(apply-senv '(z y x) 'z)

(define (translation-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (a-program (translation-of exp1 (init-senv))))))

(define (init-senv)
  (extend-senv 'i
    (extend-senv 'v
      (extend-senv 'x
        (empty-senv)))))


(define (translation-of exp senv)
  (cases expression exp
    [const-exp (num) (const-exp num)]
    [diff-exp (exp1 exp2)
              (diff-exp
                (translation-of exp1 senv)
                (translation-of exp2 senv))]
    [plus-exp (exp1 exp2)
              (plus-exp
                (translation-of exp1 senv)
                (translation-of exp2 senv))]
    [mul-exp (exp1 exp2)
             (mul-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv))]
    [zero?-exp (exp1)
               (zero?-exp
                 (translation-of exp1 senv))]
    [if-exp (exp1 exp2 exp3)
            (if-exp
              (translation-of exp1 senv)
              (translation-of exp2 senv)
              (translation-of exp3 senv))]
    [var-exp (var)
             (nameless-var-exp
               (apply-senv senv var))]
    [let-exp (var exp1 body)
             (nameless-let-exp
               (translation-of exp1 senv)
               (translation-of body
                    (extend-senv var senv)))]
    [proc-exp (var body)
              (nameless-proc-exp
                (translation-of body
                                (extend-senv var senv)))]
    [call-exp (rator rand)
              (call-exp
                (translation-of rator senv)
                (translation-of rand senv))]
    [else
      (error "Invalid source expression" exp)]))

(define (value-of exp env)
  (cases expression exp
    [const-exp (num) (num-val num)]
    [diff-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (- num1 num2)))]
    [plus-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (+ num1 num2)))]
    [mul-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (* num1 num2)))]
    [zero?-exp (exp1)
      (let* ([val1 (value-of exp1 env)]
             [num1 (expval->num val1)])
        (if (zero? num1)
          (bool-val #t)
          (bool-val #f)))]
    [if-exp (exp1 exp2 exp3)
      (let ([val1 (value-of exp1 env)])
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env)))]
    [call-exp (rator rand)
      (let ([proc (expval->proc (value-of rator env))]
            [arg (value-of rand env)])
        (apply-procedure proc arg))]
    [nameless-var-exp (n)
      (apply-nameless-env env n)]
    [nameless-let-exp (exp1 body)
      (let ([val (value-of exp1 env)])
        (value-of body (extend-nameless-env val env)))]
    [nameless-proc-exp (body)
      (proc-val (procedure body env))]
    [else
      (error "Invalid expression" exp)]))



(define ast3
  (scan&parse "
    letrec fact(n) =
       if zero?(n) then 1 else *(n, (fact -(n,1)))
    in (fact 5)
  "))
ast3

(define pgm1 (scan&parse "
         -(55, -(x,11))"))
pgm1
(translation-of-program pgm1)

(define pgm2
(scan&parse "
  let f = proc(x) -(x,11)
  in (f (f 77))
  ")
  )

pgm2
(define npgm2 (translation-of-program pgm2))
npgm2
(value-of-program npgm2)


