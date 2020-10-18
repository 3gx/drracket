#lang typed/racket

(require math/array)
(require rackunit)
(require compatibility/mlist)

(: fun (-> Number Number Number))
(define (fun x y) (+ x y))

(fun 3 4)

(: xx Integer)
(define xx 42)
(define mat (array #[#[1 2 3] #[4 xx 6] #[6 7 8] #[11 12 13]]))
(define arr (array-slice-ref mat (list 1 (::))))
mat
arr
xx

(struct Lst ([lst : (Listof Number)]) #:transparent #:mutable)

(struct sfun ([name : String] [lst : Lst]) #:transparent #:mutable)

(define f1 (sfun "hellow" (Lst '(3 4 5 6))))
f1

(: append-to-lst! (-> Lst Number Void))
(define (append-to-lst! lst value)
  (set-Lst-lst! lst (append (Lst-lst lst) `(,value)))
  (void))


(append-to-lst! (sfun-lst f1) 32)
f1

(define lst (mlist 3 4 5))
lst

(: hun (-> #:a Number #:b Number Number))
(define (hun #:a a #:b b)
  a)

(hun #:b 3 #:a 4)

