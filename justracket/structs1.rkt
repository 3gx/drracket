#lang racket

(struct fun (x y) #:mutable #:transparent
        #:methods gen:custom-write
        [(define (custom1 f port mode)
           (write-string
            (string-append
             "(FUN "
             (format "~v" (fun-x f)) " "
             (format "~v" (fun-y f)) ")" )
            port))
         (define write-proc custom1)])
(struct gun (a b) #:transparent)

(define f1 (fun 3 4))
(define f2 (fun 3 4))
(define g1 (gun f1 f2))
f1
f2
g1
(set-fun-x! f1 42)
(set-fun-y! f2 142)
f1
f2

(set-fun-x! (gun-a g1) 142)
g1
