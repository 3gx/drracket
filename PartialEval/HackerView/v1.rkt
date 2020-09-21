#lang racket

(define (power x n)
  (cond ((= n 0) 1)
        ((odd? n) (* x (power x (- n 1))))
        (else (square (power x (/ n 2))))))

(define (square x)
  (* x x))

(define (emit-power x n)
  (cond ((= n 0) 1)
        ((odd? n) `(* x ,(emit-power x (- n 1))))
        (else `(square ,(emit-power x (/ n 2))))))

(square 5)
(emit-power 'x 5)

(define (emit-power1 x n)
  (cond ((= n 0) 1)
        ((= n 1) 'x)
        ((odd? n) `(* x ,(emit-power1 x (- n 1))))
        (else `(square ,(emit-power1 x (/ n 2))))))

(emit-power1 'x 5)

(define (emit-power2 x n)
  (cond ((= n 0) 1)
        ((= n 1) 'x)
        ((odd? n) `(* x ,(emit-power2 x (- n 1))))
        (else (emit-square (emit-power2 x (/ n 2))))))


(define (emit-square x)
  `(* ,x ,x))

(emit-power2 'x 5)

(define (emit-power3 x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        ((odd? n) `(* ,x ,(emit-power3 x (- n 1))))
        (else (emit-square2 (emit-power3 x (/ n 2))))))

(define (emit-square2 x)
  (if (symbol? x)
      `(* ,x ,x)
      `(let ((y ,x))
         (* y y))))

(emit-power3 'x 5)
(emit-power3 'xxx 5)
(emit-power3 'input 2)
