#lang racket

(require eopl)

(module rec1 racket
  (define (fact n)
    (if (zero? n) 1 (* n (fact (sub1 n)))))

  (fact 5)
)

(module cps-base racket
  (require eopl)
  (define-datatype continuation continuation?
    [end-cont]
    [fact1-cont
      (n integer?)
      (cont continuation?)]
  )

  (provide (all-defined-out))
)

(module cps0 racket
  (require eopl)
  (require (submod ".." cps-base))
  (define (fact n)
    (fact/k n (end-cont)))

  (define (fact/k n cont)
    (if (zero? n)
      (apply-cont cont 1)
      (fact/k (sub1 n) (fact1-cont n cont))))

  (define (apply-cont cont val)
    (cases continuation cont
      [end-cont () val]
      [fact1-cont (saved-n saved-cont)
                  (apply-cont saved-cont (* saved-n val))]))

  (fact 5)
)

(module cps1 racket
  (require eopl)
  (require (submod ".." cps-base))
  (define n 'uninit)
  (define cont 'uninit)
  (define val 'uninit)

  (define (fact arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (fact/k))

  (define (fact/k)
    (if (zero? n)
      (begin
        (set! val 1)
        (apply-cont))
      (begin
        (set! cont (fact1-cont n cont))
        (set! n (sub1 n))
        (fact/k))))

  (define (apply-cont)
    (cases continuation cont
      [end-cont () val]
      [fact1-cont (saved-n saved-cont)
                  (set! cont saved-cont)
                  (set! val (* saved-n val))
                  (apply-cont)]))

  (fact 5)
)

(module cps2 racket
  (require eopl)
  (require (submod ".." cps-base))
  (define n 'uninit)
  (define cont 'uninit)
  (define val 'uninit)
  (define pc 'uninit)

  (define (fact arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (set! pc fact/k)
    (trampoline!)
    val)

  (define (trampoline!)
    (when pc
      (begin
        (pc)
        (trampoline!))))

  (define (fact/k)
    (if (zero? n)
      (begin
        (set! val 1)
        (set! pc apply-cont))
      (begin
        (set! cont (fact1-cont n cont))
        (set! n (sub1 n))
        (set! pc fact/k))))

  (define (apply-cont)
    (cases continuation cont
      [end-cont ()
                (set! pc #f)]
      [fact1-cont (saved-n saved-cont)
                  (set! cont saved-cont)
                  (set! val (* saved-n val))
                  (set! pc apply-cont)]))

  (fact 5)
)




(require 'rec1)
(require 'cps0)
(require 'cps1)
(require 'cps2)


