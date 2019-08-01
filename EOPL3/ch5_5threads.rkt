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

    (expression
     ("try" expression "catch" "(" identifier ")" expression)
     try-exp)

    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (expression
      ("set" identifier "=" expression)
      set-exp)

    (expression
     ("spawn" "(" expression ")")
     spawn-exp)

    (expression
     ("yield" "(" ")")
     yield-exp)

    (expression
     ("mutex" "(" ")")
     mutex-exp)

    (expression
     ("wait" "(" expression ")")
     wait-exp)

    (expression
     ("signal" "(" expression ")")
     signal-exp)

    (expression
     ("raise" expression)
     raise-exp)

    (expression
     (unop "(" expression ")")
     unop-exp)

    (unop ("car") car-unop)
    (unop ("cdr") cdr-unop)
    (unop ("null?") null?-unop)
    (unop ("zero?") zero?-unop)
    (unop ("print") print-unop)

    (expression
      ("[" (separated-list number ",") "]")
      const-list-exp)


    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val reference?)
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
              (newref
                (proc-val
                  (procedure
                    (list-ref b-vars n)
                    (list-ref p-bodies n)
                    env))))]
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

; queue
(define (empty-queue) '())
(define empty? null?)
(define (enqueue q val)
  (append q (list val)))

(define (dequeue q f)
  (f (car q) (cdr q)))

; scheduler
(define the-ready-queue   'uninitialized)
(define the-final-answer  'uninitialized)
(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)
(define (initialize-scheduler! ticks)
  (set! the-ready-queue (empty-queue))
  (set! the-final-answer 'uninitialized)
  (set! the-max-time-slice ticks)
  (set! the-time-remaining the-max-time-slice))


; mutex
(define (new-mutex)
  (a-mutex
    (newref #f)
    (newref '())))

(define (wait-for-mutex m th)
  (cases mutex m
    [a-mutex (ref-to-closed? ref-to-wait-queue)
      (cond
        [(deref ref-to-closed?)
         (setref! ref-to-wait-queue
                  (enqueue (deref ref-to-wait-queue) th))
         (run-next-thread)]
        [else
          (setref! ref-to-closed? #t)
          (th)])]))

(define (signal-mutex m th)
  (cases mutex m
    [a-mutex (ref-to-closed? ref-to-wait-queue)
      (let ([closed? (deref ref-to-closed?)]
            [wait-queue (deref ref-to-wait-queue)])
        (when closed?
          (if (empty? wait-queue)
            (setref! ref-to-closed? #f)
            (dequeue wait-queue
              (lambda (first-waiting-th other-waiting-ths)
                (place-on-ready-queue! first-waiting-th)
                (setref! ref-to-wait-queue
                         other-waiting-ths)))))
          (th))]))


; continuation

(define-datatype continuation continuation?
  [end-main-thread-cont]
  [end-subthread-cont]
  [unop-arg-cont
    (unop1 unop?)
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
    (saved-cont continuation?)]
  [set-rhs-cont
    (loc reference?)
    (cont continuation?)]
  [spawn-cont
    (saved-cont continuation?)]
  [wait-cont
    (saved-cont continuation?)]
  [signal-cont
    (saved-cont continuation?)]
  )

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


(define (apply-cont cont val)
  (if (time-expired?)
    (begin
      (place-on-ready-queue!
        (lambda () (apply-cont cont val)))
      (run-next-thread))
    (begin
      (decrement-timer!)
      (cases continuation cont
        [end-main-thread-cont ()
                              (set-final-answer! val)
                              (run-next-thread)]
        [end-subthread-cont ()
                            (run-next-thread)]
        [let-exp-cont (var body saved-env saved-cont)
                      (value-of/k body
                                  (extend-env var (newref val) saved-env)
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
                     (apply-procedure/k proc val saved-cont))]
        [try-cont (var handler-exp env cont)
                  (apply-cont cont val)]
        [raise1-cont (cont)
                    (apply-handler val cont)]
        [unop-arg-cont (unop1 cont)
                       (apply-unop unop1 val cont)]
        [set-rhs-cont (loc cont)
                      (begin
                        (setref! loc val)
                        (apply-cont cont (num-val 26)))]
        [spawn-cont (saved-cont)
          (let ([proc1 (expval->proc val)])
            (place-on-ready-queue!
              (lambda ()
                (apply-procedure/k proc1
                                   (num-val 28)
                                   (end-subthread-cont))))
            (apply-cont saved-cont (num-val 73)))]
        [wait-cont (saved-cont)
                   (wait-for-mutex
                     (expval->mutex val)
                     (lambda ()
                       (apply-cont saved-cont (num-val 42))))]
        [signal-cont (saved-cont)
                       (signal-mutex
                         (expval->mutex val)
                         (lambda ()
                           (apply-cont saved-cont (num-val 53))))]
        ))))

(define (apply-unop unop1 arg cont)
  (cases unop unop1
    [zero?-unop ()
                (apply-cont cont
                            (bool-val
                              (zero? (expval->num arg))))]
    [car-unop ()
              (let [(lst (expval->list arg))]
                (apply-cont cont (car lst)))]
    [cdr-unop ()
              (let [(lst (expval->list arg))]
                (apply-cont cont (list-val (cdr lst))))]
    [null?-unop ()
                (apply-cont cont
                            (bool-val (null? (expval->list arg))))]
    [print-unop ()
                (begin
                  (eopl:printf "~a~%" (expval->num arg))
                  (apply-cont cont (num-val 1)))]
    [else (error "Unsupported unop"  unop1)]))


(define (apply-handler val cont)
  (cases continuation cont
    [end-main-thread-cont ()
                (error "Uncaught-exception in main-thread" val)]
    [end-subthread-cont ()
                (error "Uncaught-exception in a subthread" val)]
    [spawn-cont (saved-cont)
                (apply-handler val saved-cont)]
    [signal-cont (saved-cont)
                 (apply-handler val saved-cont)]
    [wait-cont (saved-cont)
               (apply-handler val wait-cont)]
    [unop-arg-cont (unop1 saved-cont)
                (apply-handler val saved-cont)]
    [let-exp-cont (var body saved-env saved-cont)
                  (apply-handler val saved-cont)]
    [if-test-cont (exp2 exp3 saved-env saved-cont)
                  (apply-handler val saved-cont)]
    [diff1-cont (exp2 saved-env saved-cont)
                (apply-handler val saved-cont)]
    [diff2-cont (val1 saved-cont)
                (apply-handler val saved-cont)]
    [plus1-cont (exp2 saved-env saved-cont)
                (apply-handler val saved-cont)]
    [plus2-cont (val1 saved-cont)
                (apply-handler val saved-cont)]
    [mul1-cont (exp2 saved-env saved-cont)
               (apply-handler val saved-cont)]
    [mul2-cont (val1 saved-cont)
               (apply-handler val saved-cont)]
    [rator-cont (rand saved-env saved-cont)
                (apply-handler val saved-cont)]
    [rand-cont (val1 saved-cont)
                (apply-handler val saved-cont)]
    [try-cont (var handler-exp saved-env saved-cont)
              (value-of/k handler-exp
                          (extend-env var (newref val) saved-env)
                          saved-cont)]
    [set-rhs-cont (loc saved-cont)
                  (apply-handler loc saved-cont)]
    [raise1-cont (saved-cont)
                (apply-handler val saved-cont)]))


(define (apply-procedure/k proc1 val cont)
  (cases proc proc1
    [procedure (var body saved-env)
               (value-of/k body
                           (extend-env var (newref val) saved-env)
                           cont)]))


(define (init-env)
  (extend-env
    'i (newref (num-val 1))
    (extend-env
      'v (newref (num-val 5))
      (extend-env
        'x (newref (num-val 10))
        (empty-env)))))

(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (env environment?)))

(define-datatype mutex mutex?
  [a-mutex
    (ref-to-closed? reference?)
    (ref-to-wait-queue? reference?)])




(define-datatype expval expval?
  [num-val
    (value number?)]
  [bool-val
    (boolean boolean?)]
  [proc-val
    (proc proc?)]
  [list-val
    (lst (list-of expval?))]
  [mutex-val
    (mutex mutex?)]
  )

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

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define expval->mutex
  (lambda (v)
    (cases expval v
      (mutex-val (l) l)
      (else (expval-extractor-error 'mutex v)))))



(define (run str ast)
  (println (format "~a:~a" str (value-of-program 1 ast))))

(define (value-of-program timeslice pgm)
  (initialize-store!)
  (initialize-scheduler! timeslice)
  (cases program pgm
    (a-program (exp1)
      (value-of/k exp1
                  (init-env)
                  (end-main-thread-cont)))))

(define (place-on-ready-queue! th)
  (set! the-ready-queue
    (enqueue the-ready-queue th)))

(define (run-next-thread)
  (if (empty? the-ready-queue)
    the-final-answer
    (dequeue the-ready-queue
      (lambda (first-ready-thread other-ready-threads)
        (set! the-ready-queue other-ready-threads)
        (set! the-time-remaining the-max-time-slice)
        (first-ready-thread)))))

(define (set-final-answer! val)
  (set! the-final-answer val))

(define (time-expired?)
  (zero? the-time-remaining))

(define (decrement-timer!)
  (set! the-time-remaining (sub1 the-time-remaining)))

; value-of/k

(define (value-of/k exp env cont)
  (cases expression exp
    [const-exp (num) (apply-cont cont (num-val num))]
    [var-exp (var) (apply-cont cont (deref (apply-env env var)))]
    [diff-exp (exp1 exp2)
              (value-of/k exp1 env
                          (diff1-cont exp2 env cont))]
    [plus-exp (exp1 exp2)
              (value-of/k exp1 env
                          (plus1-cont exp2 env cont))]
    [mul-exp (exp1 exp2)
              (value-of/k exp1 env
                          (mul1-cont exp2 env cont))]
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
    [try-exp (exp1 var handler-exp)
             (value-of/k exp1 env
                         (try-cont var handler-exp env cont))]
    [raise-exp (exp1)
               (value-of/k exp1 env
                           (raise1-cont cont))]
    [letrec-exp (p-names b-vars p-bodies letrec-body)
                (value-of/k letrec-body
                            (extend-env-rec* p-names b-vars p-bodies env)
                            cont)]
    [begin-exp (exp1 exps)
               (if (null? exps)
                 (value-of/k exp1 env cont)
                 (value-of/k
                   (call-exp
                     (proc-exp
                       (fresh-identifier 'dummy)
                       (begin-exp (car exps) (cdr exps)))
                     exp1)
                   env cont))]

    [set-exp (id1 exp1)
             (value-of/k exp1 env
                         (set-rhs-cont (apply-env env id1) cont))]
    [unop-exp (unop1 exp1)
               (value-of/k exp1 env
                           (unop-arg-cont unop1 cont))]
    [const-list-exp (nums)
                    (apply-cont cont
                                (list-val (map num-val nums)))]
    [spawn-exp (exp1)
               (value-of/k exp1 env
                           (spawn-cont cont))]
    [yield-exp ()
               (place-on-ready-queue!
                 (lambda ()
                   (apply-cont cont (num-val 99))))
               (run-next-thread)]
    [mutex-exp ()
               (apply-cont cont (mutex-val (new-mutex)))]
    [wait-exp (exp1)
              (value-of/k exp1 env
                          (wait-cont cont))]
    [signal-exp (exp1)
                (value-of/k exp1 env
                            (signal-cont cont))]

    [else (error "Unsupported" exp)]
    ))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

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


(define ast5a
  (scan&parse "
      let t1 = proc(x) proc(y)
                        if zero?(y)
                        then raise 42
                        else  +(x,y)
      in ((t1 4) 3)"))
ast5a
(run 'ast5a ast5a)

(define ast5b
  (scan&parse "
      let t1 = proc(x) proc(y)
                        if zero?(y)
                        then raise 42
                        else  +(x,y)
      in try ((t1 4) 3)
         catch (exn) -(0,exn)"))
ast5b
(run 'ast5b ast5b)

(define ast5c
  (scan&parse "
      let t1 = proc(x) proc(y)
                        if zero?(y)
                        then raise 42
                        else  +(x,y)
      in try ((t1 4) 0)
         catch (exn) -(0,exn)"))
ast5c
(run 'ast5c ast5c)

(define ast6 (scan&parse "
  let g = let count = 0
          in proc (dummy)
               begin
                 set count = -(count,-1);
                 count
               end
  in let a = (g 11)
      in let b = (g 11)
          in -(a,b)
"))
ast6
(run 'ast6 ast6)

(define ast7 (scan&parse "
  let times4 = 0
  in begin
       set times4 = proc (x)
                     if zero?(x)
                     then 0
                     else -((times4 -(x,1)), -4); 
                   (times4 3)
      end
"))
ast7
(run 'ast7 ast7)

(define ast8a (scan&parse "
  let lsst = [8,2,3,4,5]
  in car(lsst)"))
ast8a
(run 'ast8a ast8a)

(define ast8b (scan&parse "
  let lsst = [8,2,3,4,5]
  in cdr(lsst)"))
ast8a
(run 'ast8b ast8b)

(define thread1 (scan&parse "
  letrec
    noisy (l) = if null?(l)
                then 0
                else begin
                       print(car(l));
                       (noisy cdr(l))
                     end
   in
     begin
      spawn(proc (d) (noisy [1,2,3,4,5]));
      spawn(proc (d) (noisy [6,7,8,9,10]));
      print(100);
      33
     end
    "))
thread1
(run 'thread1 thread1)

(define thread2 (scan&parse "
  let buffer = 0
  in let producer =
      proc (n)
        letrec
          wait1(k) = if zero?(k)
                    then set buffer = n
                    else begin
                           print(+(k,200));
                           (wait1 -(k,1))
                         end
        in (wait1 5)
     in let consumer = proc(d)
              letrec busywait1 (k) = if zero?(buffer)
                                    then begin
                                           print(+(k,100));
                                           (busywait1 -(k,-1))
                                         end
                                    else buffer
              in (busywait1 0)
        in begin
             spawn (proc (d) (producer 44));
             print(300);
             (consumer 86)
           end
"))
thread2
(run 'thread2 thread2)

(define thread3 (scan&parse "
  let x = 0
  in let mut = mutex()
  in let incr_x = proc (id) proc (dummy)
                    begin
                      wait(mut);
                      set x = -(x,-1);
                      set x = -(x,-1);
                      print(x);
                      signal(mut)
                    end
  in begin
      spawn( (incr_x 100));
      spawn( (incr_x 200));
      spawn( (incr_x 300));
      yield()
     end
     "))
thread3
(run 'thread3 thread3)
