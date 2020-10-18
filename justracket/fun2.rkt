#lang typed/racket
(require (for-syntax syntax/location))

(struct MyInt ([x : Number]) #:transparent)

(: myadd (-> (-> MyInt Number) Number))
(define (myadd f)
  (define a (MyInt 3))
  (f a))
               
(myadd #| func |# (lambda (a)
                    (define b (MyInt 5))
                    (+ (MyInt-x a) (MyInt-x b))))


;(struct Stx ([src : String]
;             [line : Number]
;             [column : Number]
;             [span : Number]) #:transparent)                       

(struct Base ([stx : Any]) #:transparent)
(struct DerivedA Base ([value : Number]) #:transparent)

(define-syntax (Derived1 value)
    (define stx (list (syntax-source-file-name  value)
                       (syntax-line value)
                        (syntax-column value)
                       (syntax-span value)))

 (datum->syntax value `(DerivedA ,stx 42)))
  
;(define s1 (Derived "stx" 3))
;(displayln s1)

(Derived1 3)
;(define s1 (Derived1 3))

