#lang racket

(define (emit-power x n)
  (cond ((= n 0) 1)
        ((odd? n) (emit-* x (emit-power x (- n 1))))
        (else (emit-square (emit-power x (/ n 2))))))

(define (emit-square x)
  (emit-* x x))

(define (emit-* x y)
  (cond ((eqv? y 1) x)
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (* y y)))
        (else
         `(* ,x ,y))))


(emit-power 'x 5)
(emit-power 'x 10)
