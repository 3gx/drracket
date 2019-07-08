#lang racket

; 4.3 Using dot notation for nested hash lookups


(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
              ([k (in-list ks)])
      (hash-ref h k))))


(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))
(hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)
(hash-refs js '(a b c))

(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ; Check for no args at all
    [(_)
       (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx)]
    ; If the optional 'default' is missing, use #f
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (unless (identifier? #'chain)
        (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain))     
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
; Check that we have at least hash.key
         (unless (and (>= (length ids) 2)
                      (not (eq? (syntax-e (cadr ids)) '||)))
           (raise-syntax-error #f "Expected hash.key" stx #'chain))       
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))

(hash.refs js.a.b.c)
(hash.refs js.blah)
(hash.refs js.a.b.blah)
(hash.refs js.a.b.blah 'did-not-exist)

