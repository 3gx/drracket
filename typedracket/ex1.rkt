#lang racket

(struct pt (x y))

; distance : pt pt -> real
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-x p1))))))

(define pa (pt 3 4))
(define pb (pt 5 6))
(distance pa pb)

