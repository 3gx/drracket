#lang racket

(define (poly-value coeffs x)
  (foldl (lambda (value coeff)
           (+ (* value x) coeff))
         0
         coeffs))

(poly-value '(5 0 1) 2)

(define (emit-poly-value coeffs x)
  (let ((* emit-*) (+ emit-+))
    (foldl (lambda (value coeff)
             (+ (* value x) coeff))
           0
           coeffs)))

(define (emit-* x y)
  (cond ((eqv? y 1) x)
        ((eqv? x 1) y)
        ((eqv? x 0) (eqv? y 0) 0)
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (* y y)))
        (else
         `(* ,x ,y))))

(define (emit-+ x y)
  (cond ((eqv? y 0) x)
        ((eqv? x 0) y)
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (+ y y)))
        (else
         `(+ ,x ,y))))

(emit-poly-value '(5 0 1) 'x)
