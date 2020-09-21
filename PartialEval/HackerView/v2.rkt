#lang racket

(define (emit-power x n)
  (cond ((= n 0) 1)
        ((odd? n) (emit-* x (emit-power x (- n 1))))
        (else (emit-square (emit-power x (/ n 2))))))

(define (emit-square x)
  (emit-* x x))

(define (emit-* x y)
  `(* ,x ,y))

(emit-power 'x 5)
