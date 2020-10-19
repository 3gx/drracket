#lang racket
(require (for-syntax racket/match))

; 3.4 Compile time vs. run time

(define-syntax (foo stx)
  (make-pipe)
  #'(void))

(define (display-and-return x)
  (displayln x)
  x)

(define (our-if pred-expr true-expr false-expr)
  (cond [pred-expr true-expr]
        [else false-expr]))

(our-if #t
        (display-and-return "true")
        (display-and-return "false"))

(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (println xs)
  (define v1 `(cond [,(cadr xs) ,(caddr xs)]
                             [else ,(cadddr xs)]))
  (println v1)
  (datum->syntax  stx v1))

(our-if-v2 #t
           (display-and-return "true")
           (display-and-return "false"))

(our-if-v2 #f
           (display-and-return "true")
           (display-and-return "false"))


(define-syntax (our-if-match stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))


(our-if-match #f
              (display-and-return "true")
              (display-and-return "false"))

