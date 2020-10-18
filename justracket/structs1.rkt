#lang racket

(struct fun (x y) #:mutable #:transparent)
(struct gun (a b) #:mutable #:transparent)

(define f1 (fun 3 4))
(define f2 (fun 3 4))
(define g1 (gun f1 f2))
f1
f2
g1
(set-fun-x! f1 42)
(set-fun-y! f2 (- 42))
f1
f2