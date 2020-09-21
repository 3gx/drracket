#lang racket

(define (emit-* x y)
  (cond ((eqv? y 1) x)
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (* y y)))
        (else
         `(* ,x ,y))))

(define (emit-power x n)
  (let ((* emit-*))

    (define (power x n)
      (cond ((= n 0) 1)
            ((odd? n) (* x (power x (- n 1))))
            (else (square (power x (/ n 2))))))

    (define (square x)
      (* x x))

    (power x n)))

(emit-power 'x 5)
