#lang racket
(require racket/match)

; 1. Inductive Sets of Data

; in-S? : N -> Bool
; usage: (in-S? n) = #t if n is in S, #f otherwise

(define (in-S? n)
  (if (zero? n)
      #t
      (if (>= (- n 3) 0)
          (in-S? (- n 3))
          #f)))


(in-S? 3)
(in-S? 4)


; lst-len : List -> Int
; usage: (lst-len l) = the length of l
(define (lst-len lst)
  (if (null? lst)
      0
      (+ 1 (lst-len (cdr lst)))))

(lst-len '(a b c))
(lst-len '(a (b c) d))

; remove-first: (Sym, Listof(Sym)) -> Listof(Sym)
(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? (car los) s)
          (cdr los)
          (cons (car los) (remove-first s (cdr los))))))
(remove-first 'a '(a b c))
(remove-first 'b '(e f g))
(remove-first 'a4 '(c1 a4 c1 a4))

; occurs-free: Sym x LcExp -> Bool

(define (occurs-free? var exp)
  (cond
    [(symbol? exp) (eqv? var exp)]
    [(eqv? (car exp) 'lambda)
     (and
      (not (eqv? var (car (cadr exp))))
      (occurs-free? var (caddr exp)))]
    [else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cadr exp)))]))

(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '((lambda (x) x) (x y)))


; subst

(define (subst new old slist)
  (if (null? slist)
      '()
      (cons
       (subst-in-s-exp new old (car slist))
       (subst new old (cdr slist)))))

(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old)
          new
          sexp)
      (subst new old sexp)))

(subst 'a 'b '((b c) (b (b (b (e c b (e b c)) d)))))

(define (num-of-elm-from lst [n 0])
  (if (null? lst)
      '()
      (cons
        (list n (car lst))
        (num-of-elm-from (cdr lst) (+ n 1)))))

(define (num-elm lst)
  (num-of-elm-from lst))

(num-elm '(a b c d))

; 1.4 Excercises
;
; Assume:
;  s: symbol
;  n: non-negative integer
;  lst: list
;  loi: list of integers
;  los: list of symbols
;  slist: s-list
;
;

;; Excersie 1.15
; (duple n x) - return a list containing n-copies of x

(define (duple n x)
  (if (zero? n)
    '()
    (cons x (duple (- n 1) x))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))


; Ex 1.16
; (invert lst)
(define (invert lst)
  (if (null? lst)
    '()
    (cons (list (cadr (car lst)) (car (car lst))) (invert (cdr lst)))))
(invert '((a 1) (a 2) (1 b) (2 b)))

; Ex 1.17 [*]
; (down lst)
(define (down lst)
  (if (null? lst)
    '()
    (cons (list (car lst)) (down (cdr lst)))))
(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))

; Ex 1.18 [*]
(define (swapper s1 s2 slist)
  (if (null? slist)
    '()
    (cons (swapper-sexp s1 s2 (car slist))
          (swapper s1 s2 (cdr slist)))))
(define (swapper-sexp  s1 s2 sexp)
  (if (symbol? sexp)
    (cond
      [(eqv? s1 sexp) s2]
      [(eqv? s2 sexp) s1]
      [else sexp])
    (swapper s1 s2 sexp)))
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))

; Ex 1.19 [**]
; (list-set lst n x)
(define (list-set lst n x)
  (if (zero? n)
    (cons x (cdr lst))
    (cons (car lst) (list-set (cdr lst) (sub1 n) x))))
(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

; Ex 1.20 [*]
(define (count-occur s slist)
  (if (null? slist)
    0
    (+ (count-occur-sexp s (car slist))
       (count-occur s (cdr slist)))))

(define (count-occur-sexp s sexp)
  (if (symbol? sexp)
    (if (eqv? s sexp)
      1
      0)
    (count-occur s sexp)))
(count-occur 'x '((f x) y (((x z) x))))
(count-occur 'x '((f x) y (((x z) () x))))
(count-occur 'x '((f x) y (((x z) (x x) x))))
(count-occur 'w '((f x) y (((x z) x))))

; Ex 1.21 [**]
(define (productA lst1 lst2)
  (if (null? lst1)
    '()
    (append (productA (cdr lst1) lst2)
            (product1 (car lst1) lst2))))
(define (productB lst1 lst2)
  (if (null? lst1)
    '()
    (append (product1 (car lst1) lst2)
            (productB (cdr lst1) lst2))))
(define (product1 s lst2)
  (if (null? lst2)
    '()
    (cons (list s (car lst2))
          (product1 s (cdr lst2)))))

(product1 'a '(x y z))
(productA '(a b c) '(x y))
(productB '(a b c) '(x y))

; Ex 1.21 [**]
(define (filter-in pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter-in pred (cdr lst)))
      (filter-in pred (cdr lst)))))
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in list? '(a (b c) 17 foo))


(define (list-index1 pred lst)
  (if (or (null? lst)
          (pred (car lst)))
    0
    (add1 (list-index1 pred (cdr lst)))))

(define (list-index pred lst)
  (let ([len (list-index1 pred lst)])
    (if (eq? len (length lst))
      #f
      len)))
(list-index number? '(a 2 (1 3) b 7))
(list-index number? '(a a (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
(list-index list? '(1 2 (a b) 3))

; Ex 1.26
(define (up lst)
  (if (null? lst)
    '()
    (if (list? (car lst))
      (append (car lst) (up (cdr lst)))
      (cons (car lst) (up (cdr lst))))))
(up '((1 2) (3 4)))
(up '((x (y)) z))

; Ex 1.27
(define (flatten lst)
  (if (null? lst)
    '()
    (if (list? (car lst))
      (append (flatten (car lst)) (flatten (cdr lst)))
      (cons (car lst) (flatten (cdr lst))))))
(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))

; Ex 1.28
(define (merge a b [predicate <])
  (if (null? a)
    b
    (if (null? b)
      a
      (if (predicate (car a) (car b))
        (cons (car a) (merge (cdr a) b predicate))
        (cons (car b) (merge a (cdr b) predicate))))))
(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))

; Ex 1.28
(define (sort a [predicate <])
  (if (null? a)
    '()
    (merge (list (car a)) (sort (cdr a) predicate) predicate)))

(sort '(8 2 5 2 3))

; Ex 1.30
(define (sort/predicate op lst)
  (sort lst op))
(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))

; Ex 1.31
(define tr1 '(foo 1 2))
(define tr2 '(bar 1 (foo 1 2)))
(define tr3 '(baz
               (bar 1 (foo 1 2))
               (biz 4 5)))
tr3

(define (t-leaf sym)
  `(leaf ,sym))
(define (t-leaf? node)
  (and (list? node)
       (eqv? 'leaf (car node))))

(define (t-node val lnode rnode)
  `(node ,val ,lnode ,rnode))
(define (t-node? node)
  (and (list? node)
       (eqv? 'node (car node))))
(define (lson node)
  (if (t-node? node)
    (caddr node)
    (error (format "lson: Not a node: ~a" node))))
(define (rson node)
  (if (t-node? node)
    (cadddr node)
    (error (format "rson: Not a node: ~a" node))))
(define (content node)
  (if (or (t-node? node)
          (t-leaf? node))
    (cadr node)
    (error (format "content: Not a node: ~a" node))))

(define tl1 (t-leaf 77))
(define tl2 (t-leaf 88))
(define tn1 (t-node 42 tl1 tl2))

tl1
tl2
tn1
(t-leaf? tl1)
(t-leaf? tl2)
(t-leaf? tn1)

(t-node? tl1)
(t-node? tl2)
(t-node? tn1)

(with-handlers ([exn:fail? (lambda (exn) 'node-a-node)]) (lson tl1))
(lson tn1)
(rson tn1)
(with-handlers ([exn:fail? (lambda (exn) 'node-a-node)]) (lson tl2))
(content tn1)
(content tl1)


; Ex 1.32
(define (double x)
  (* 2 x))
(define (double-t node)
  (if (t-leaf? node)
    (t-leaf (double (content node)))
    (t-node (double (content node))
            (double-t (lson node))
            (double-t (rson node)))))

(double-t tn1)

; Ex 1.36

(define tn2
  (t-node 'red
          (t-node 'bar (t-leaf 26) (t-leaf 12))
          (t-node 'red (t-leaf 11)
                  (t-node 'quux (t-leaf 117) (t-leaf 14)))))
tn2

(define (depth-t node [depth 0])
  (if (t-leaf? node)
    (t-leaf (sub1 depth))
    (t-node (content node)
            (depth-t (lson node) (add1 depth))
            (depth-t (rson node) (add1 depth)))))
(depth-t tn2)

;  Ex 1.34

(define bt1
  (t-node 14
          (t-node 7
                  (t-leaf '())
                  (t-node 12
                          (t-leaf '())
                          (t-leaf '())))
          (t-node 26
                  (t-node 20
                          (t-node 17
                                  (t-leaf '())
                                  (t-leaf '()))
                          (t-leaf '()))
                  (t-node 31
                          (t-leaf '())
                          (t-leaf '())))))
(define (path n node [what #f])
  (if (t-leaf? node)
    (error (format "Value '~a' not found" n))
    (if (eq? n (content node))
      '()
      (if (<= n (content node))
        (cons 'left (path n (lson node)))
        (cons 'right (path n (rson node)))))))
(path 17 bt1)


; Ex 1.35
(define tr4
  (t-node 'foo
          (t-node 'bar
                  (t-leaf 26)
                  (t-leaf 12))
          (t-node 'baz
                  (t-leaf 11)
                  (t-node 'quux
                          (t-leaf 117)
                          (t-leaf 14)))))
tr4
(define counter 0)
(define (number-leaves node)
  (if (t-leaf? node)
    (begin
      (set! counter (add1 counter))
      (t-leaf counter))
    (t-node (content node)
            (number-leaves (lson node))
            (number-leaves (rson node)))))
(define (number-leaves1 node)
  (letrec ([counter 0]
           [f (lambda (node)
              (if (t-leaf? node)
                (begin
                  (set! counter (add1 counter))
                  (t-leaf counter))
                (t-node (content node)
                        (f (lson node))
                        (f (rson node)))))])
    (f node)))
(define (number-leaves2-impl pair)
  (define node (car pair))
  (define counter (cdr pair))
  (if (t-leaf? node)
    (cons (t-leaf counter) (add1 counter))
    (match-let*
      ([(cons l c1) (number-leaves2-impl (cons (lson node) counter))]
       [(cons r c2) (number-leaves2-impl (cons (rson node) c1))])
      (cons (t-node (content node) l r) c2))))

(define (number-leaves2 node)
  (car (number-leaves2-impl (cons node 1))))

(number-leaves tr4)
(number-leaves1 tr4)
(number-leaves2 tr4)

; Ex 1.36


(define (g1inc b)
  (if (null? b)
    b
    (cons (list (add1 (caar b)) (cadar b))
          (g1inc (cdr b)))))

(define (g1 a b)
  (cons a (g1inc b)))


(define (number-elements lst)
  (if (null? lst)
    '()
    (g1 (list 0 (car lst)) (number-elements (cdr lst)))))

(number-elements '(a b c d e f))

(define (cumulative-sum l [so-far 0])
  (cond
    [(empty? l) '()]
    [else (define next (+ so-far (first l)))
          (cons next (cumulative-sum (rest l) next))]))

(cumulative-sum '(1 1 1))




