#lang racket
(define-syntax foo
  (lambda (stx)
    (syntax `(I am foo stx))))

(define-syntax (also-foo stx)
  (syntax '(I am also-foo stx)))

(define-syntax (say-hi stx)
  #'(displayln "hi"))

(define-syntax (show-me stx)
  (print stx)
  #'(void))

(show-me '(+ 1 2))
(show-me `(+ 1 2 ,x))
(show-me (+ 1 2))

(define stx #'(if x (list "true") #f))
stx

; 3.3 Actually transforming the input

(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me 3 2 +)
(reverse-me "backwards" "i" "am" values)

