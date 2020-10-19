#lang racket
#|
(require (for-syntax racket/match))

; 4 Pattern matching: syntax-case and syntax-rules

(define-syntax (our-if-match2 stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))

(our-if-match2 #f "true" "false")
(our-if-match2 #t "true" "false")

(define-syntax-rule (our-if-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))
(our-if-rule #t "yes!" "no!")



; 4.1 Pattern variable vs. template -- fight!

(define-syntax (hyphen-define/wrong1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (let ([name (string->symbol (format "~a-~a" a b))])
         #'(define (name args ...)
             body0 body ...))]))
|#


(define-syntax (hyphen-define/wrong1.1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (let ([name (string->symbol (format "~a-~a" #'a #'b))])
         #'(define (name args ...)
             body0 body ...))]))

(hyphen-define/wrong1.1 foo bar () #t)
;(foo-bar)

(define-syntax (hyphen-define/wrong1.2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a" #'a #'b)))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
(hyphen-define/wrong1.2 foo bar () #t)
;(foo-bar)

(define-syntax (hyphen-define/ok1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
(hyphen-define/ok1 foo bar () #t)
(foo-bar)


; 4.1.1 Syntax-case

(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a
                                        (string->symbol (format "~a-~a"
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))
(hyphen-define/ok2 fun gun () 'fungun)
(fun-gun)




; 4.1.2 with-syntax*
(require (for-syntax racket/syntax))

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'(+ c 2))]))
(foo 42)

; 4.1.3 format-id

(define-syntax (hyphen-define/ok3 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
         #'(define (name args ...)
             body0 body ...))]))
(hyphen-define/ok3 bar baz () 'bar-baz)
(bar-baz)

; 4.1.4 Another example


(require (for-syntax racket/string racket/syntax))

(define-syntax (hyphen-define* stx)
    (syntax-case stx ()
      [(_ (names ...) (args ...) body0 body ...)
       (let ([name-stxs (syntax->list #'(names ...))])
         (with-syntax ([name (datum->syntax (car name-stxs)
                                            (string->symbol
                                             (string-join (for/list ([name-stx name-stxs])
                                                            (symbol->string
                                                             (syntax-e name-stx)))
                                                          "-")))])
           #'(define (name args ...)
               body0 body ...)))]))

(hyphen-define* (foo bar baz) (v) (* 2 v))
(foo-bar-baz 50)