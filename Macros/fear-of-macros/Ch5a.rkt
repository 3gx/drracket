#lang racket

; 5 Syntax parameters


(define-syntax-rule (aif1 condition true-expr false-expr)
  (let [(it condition)]
    (if it
        true-expr
        false-expr)))
;(aif1 #t (displayln it1) (void))


(require racket/stxparam)
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))
(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
                              true-expr)
        false-expr)))

