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
  (define NN 256)
  (define zero (lambda () '()))
  (define (is-zero? n) (null? n))

  (define (rfold f acc lst)
    (if (null? lst)
      acc
      (f (car lst)
         (rfold f acc (cdr lst)))))

  (define (inc n carry-m)
    (define carry (car carry-m))
    (define m (cdr carry-m))
    (define n1
      (if carry
        (add1 n)
        n))
    (if (< n1 NN)
      (cons #f (cons n1 m))
      (cons #t (cons 0 m))))

  (define (dec n carry-m)
    (define carry (car carry-m))
    (define m (cdr carry-m))
    (define n1
      (if carry
        (sub1 n)
        n))
    (if (< n1 0)
      (cons #t (cons (sub1 NN) m))
      (cons #f (cons n1 m))))

  (define (successor n)
    (define carry-m (rfold inc (cons #t '()) n))
    (define carry (car carry-m))
    (define m (cdr carry-m))
    (if carry
      (cons 1 m)
      m))

  (define (predecessor n)
    (define carry-m (rfold dec (cons #t '()) n))
    (define carry (car carry-m))
    (define m (cdr carry-m))
    (if (eq? 0 (car m))
      (cdr m)
      m))

  (define (pow n pair)
    (define val (car pair))
    (define ex (cdr pair))
    (cons (+ val (* n (expt NN ex))) (add1 ex)))

  (define (value n)
    (car (rfold pow (cons 0 0) n)))


  (provide (all-defined-out))
)

(require 'v3)
(define (plus x y)
  (if (is-zero? x)
    y
    (successor (plus (predecessor x) y))))

(define (mul a b)
  (if (is-zero? b)
    (zero)
     (plus a (mul a (predecessor b)))))

(define (factorial n)
  (if (is-zero? (predecessor n))
    n
    (mul n (factorial (predecessor n)))))


(define one (successor (zero)))
one
(successor one)
(define three (successor (successor one)))
three
(println (format "value: ~a" (value three)))
(successor one)
(predecessor (successor one))
(predecessor (predecessor (successor one)))
(define two (plus (successor (zero)) (successor (zero))))
two
(define four (mul two two))
four
(define eight (mul two four))
eight
(println (format "value: ~a" (value eight)))
(define twelve (plus eight  four))
twelve
(println (format "value: ~a" (value twelve)))
(define fac8 (factorial eight))
fac8
(println (format "value: ~a" (value fac8)))
(define fac9 (factorial (successor eight)))
fac9
(println (format "value: ~a" (value fac9)))
(define fac10 (factorial (successor (successor eight))))
fac10
(println (format "value: ~a" (value fac10)))
