#lang typed/racket


(struct pt ([x : Real] [y : Real]))

(: distance (-> pt pt Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-x p1))))))

(define pa (pt 3 4))
(define pb (pt 5 6))
(distance pa pb)

(provide (all-defined-out))

