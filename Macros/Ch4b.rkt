#lang racket
(require (for-syntax racket/syntax))

; 4.2 Making our own struct

(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ; Define a ctor
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ; Define a predicate
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail?
             (lambda () (foo-a "furble")))


;(our-struct "blah" ("blah" "blah"))

(define-syntax (our-struct-v2 stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ; Define a ctor
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ; Define a predicate
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

(define-syntax (our-struct-v3 stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       ; Guard or "fender" expression:
       (for-each (lambda (x)
                   (unless (identifier? x)
                     (raise-syntax-error #f "not an identifier" stx x)))
                 (cons #'id (syntax->list #'(fields ...))))
       (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
         #`(begin
             ; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id  (list fields ...))))
             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id "~a is not a ~a struct" v 'id))
                        (vector-ref v ix))))))]))