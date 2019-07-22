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

    ;; explicit-refs

    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (expression
      ("newref" "(" expression ")")
      newref-exp)

    (expression
      ("deref" "(" expression ")")
      deref-exp)

    (expression
      ("setref" "(" expression "," expression ")")
      setref-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;

(define prog1 (scan&parse "
  let x = newref(0)
  in letrec even(dummy)
              = if zero?(deref(x))
                then 1
                else begin
                      setref(x, -(deref(x),1));
                      (odd 888)
                     end
            odd(dummy)
             = if zero?(deref(x))
               then 0
               else begin
                       setref(x, -(deref(x),1));
                       (even 888)
                    end
      in begin setref(x,13); (odd 888) end
                          "))
prog1

(define prog2 (scan&parse "
      let g = let counter = newref(0)
              in proc (dummy) begin
                    setref(counter, -(deref(counter), -1)); deref(counter)
                end
      in let a = (g 11)
         in let b = (g 11)
            in -(a,b)
"))
prog2

(define (init-env)
  (extend-env
    'i (num-val 1)
    (extend-env
      'v (num-val 5)
      (extend-env
        'x (num-val 10)
        (empty-env)))))

(define (value-of-program pgm)
  (initialize-store!)
  [cases program pgm
         (a-program (exp1)
                    (value-of exp1 (init-env)))])

(define (empty-store) '())

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (let ([next-ref (length the-store)])
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (set! the-store
    (letrec
      ([setref-inner
         (lambda (store1 ref1)
           (cond
             [(null? store1)
              (error "Invalid ref" ref the-store)]
             [(zero? ref1)
              (cons val (cdr store1))]
             [else
               (cons
                 (car store1)
                 (setref-inner
                   (cdr store1) (sub1 ref1)))]))])
      (setref-inner the-store ref))))

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

(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (env environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body env)
      (value-of body (extend-env var val env)))))

(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (proc-val
    (proc proc?))
  (ref-val
    (ref reference?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (error "failed to extract num" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (error "failed to extract bool" val))))

(define (expval->ref val)
  (cases expval val
    (ref-val (ref) ref)
    (else (error "failed to extract ref" val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (error "failed to extract proc" val))))

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
    (plus-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (+ num1 num2))))
    (mul-exp (exp1 exp2)
      (let* ([val1 (value-of exp1 env)]
             [val2 (value-of exp2 env)]
             [num1 (expval->num val1)]
             [num2 (expval->num val2)])
        (num-val (* num1 num2))))
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
        (apply-procedure proc arg)))
    (letrec-exp (p-name b-var p-body letrec-body)
      (value-of letrec-body (extend-env-rec p-name b-var p-body env)))
    [begin-exp (exp1 exps)
               (letrec
                 ([value-of-begins
                    (lambda (e1 es)
                      (let ([v1 (value-of e1 env)])
                        (if (null? es)
                          v1
                          (value-of-begins (car es) (cdr es)))))])
                 (value-of-begins exp1 exps))]
    [newref-exp (exp1)
                (let ([v1 (value-of exp1 env)])
                  (ref-val (newref v1)))]
    [deref-exp (exp1)
               (let* ([v1 (value-of exp1 env)]
                      [ref1 (expval->ref v1)])
                 (deref ref1))]
    [setref-exp (exp1 exp2)
                (let* ([ref (expval->ref (value-of exp1 env))]
                       [val2 (value-of  exp2 env)])
                  (begin
                    (setref! ref val2)
                    (num-val 23)))]))



#|
(initialize-store!)
(define r1 (newref 42))
(define r2 (newref 43))
(define r3 (newref 44))
the-store
r2
(deref r2)
(setref! r2 55)
the-store
(deref r2)
|#

(value-of-program prog2)


