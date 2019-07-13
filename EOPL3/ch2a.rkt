#lang racket

(define (foldl f acc xs)
  (if (null? xs)
      acc
      (foldl f
             (f acc (car xs))
             (cdr xs))))

(define (foldr1 f acc xs)
  (if (null? xs)
      acc
      (f (foldr f acc (cdr xs))
         (car xs))))
(define (foldr2 f acc xs)
  (if (null? xs)
      acc
      (f (car xs)
         (foldr f acc (cdr xs)))))

(foldl list 'init '(a b c))
(foldr1 list 'init '(a b c))
(foldr2 list 'init '(a b c))



(module v1 racket
  (define zero (lambda () '()))
  (define is-zero? (lambda (n) (null? n)))
  (define successor (lambda (n) (cons #t n)))
  (define predecessor (lambda (n) (cdr n)))
  (define (value n)
    (if (is-zero? n)
      0
      (add1 (value (cdr n)))))
  (provide (all-defined-out))
)

(module v2 racket
  (define zero (lambda () 0))
  (define is-zero? (lambda (n) (zero? n)))
  (define successor (lambda (n) (add1 n)))
  (define predecessor (lambda (n) (sub1 n)))
  (define (value n) n)
  (provide (all-defined-out))
)

(module v3 racket
  (define NN 16)
  (define zero (lambda () '()))
  (define (is-zero? n) (lambda (n) (null? n)))
  (define (inc n)
    (if (< (add1 n) NN)
          (cons #f (add1 n))
          (cons #t 0)))
  (define (dec n)
    (if (< (sub1 n) 0)
          (cons #t 0)
          (cons #f (sub1 n))))
  (define (succ-rec n)
    'void)
  (define (successor n)
    (if (is-zero? n)
      '(1)
      (succ-rec n)))
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
(println (format "value: ~a" (value eight)))
