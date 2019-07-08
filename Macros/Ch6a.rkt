#lang racket

; 6 What’s the point of splicing-let?


(require racket/splicing)
(splicing-let ([x 0])
    (define (get-x)
      x))

(get-x)

(define get-y
    (let ([y 0])
      (lambda ()
        y)))

(get-y)

(splicing-let ([x 0])
    (define (inc)
      (set! x (+ x 1)))
    (define (dec)
      (set! x (- x 1)))
    (define (get)
      x))

(define-values (inc1 dec1 get1)
    (let ([x 0])
      (values (lambda ()  ; inc
                (set! x (+ x 1)))
              (lambda ()  ; dec
                (set! x (- x 1)))
              (lambda ()  ; get
                x))))