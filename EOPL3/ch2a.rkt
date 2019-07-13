#lang racket

(module v1 racket
  (define zero (lambda () '()))
  (define is-zero? (lambda (n) (null? n)))
  (define successor (lambda (n) (cons #t n)))
  (define predecessor (lambda (n) (cdr n)))
  (provide (all-defined-out))
)

(module v2 racket
  (define zero (lambda () 0))
  (define is-zero? (lambda (n) (zero? n)))
  (define successor (lambda (n) (add1 n)))
  (define predecessor (lambda (n) (sub1 n)))
  (provide (all-defined-out))
)

(module v3 racket
  (define zero (lambda () '(0)))
  (provide (all-defined-out))
)

(require 'v1)
(define (plus x y)
  (if (is-zero? x)
    y
    (successor (plus (predecessor x) y))))

(define (mul a b)
  (if (is-zero? b)
    (zero)
     (plus a (mul a (predecessor b)))))


(define one (successor (zero)))
one
(define two (plus (successor (zero)) (successor (zero))))
two
(define four (mul two two))
four
(define eight (mul two four))
eight
