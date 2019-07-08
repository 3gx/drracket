#lang racket

(module phase-mismatch-mod racket
    (require syntax/parse (for-syntax syntax/parse))
  (begin-for-syntax
    (define-syntax-class foo
      (pattern (a b c))))
    (define-syntax (macro stx)
      (syntax-parse stx
        [(_ f:foo) #'(+ f.a f.b f.c)])))

(module stxclass-mod racket
    (require syntax/parse)
    (define-syntax-class foo
      (pattern (a b c)))
    (provide foo))

(module macro-mod racket
    (require (for-syntax syntax/parse
                         'stxclass-mod))
    (define-syntax (macro stx)
      (syntax-parse stx
        [(_ f:foo) #'(+ f.a f.b f.c)]))
    (provide macro))