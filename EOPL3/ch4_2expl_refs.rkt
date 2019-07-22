#lang racket

(require eopl)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("+" "(" expression "," expression ")")
     plus-exp)
    (expression
     ("*" "(" expression "," expression ")")
     mul-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
      ("proc" "(" identifier ")" expression)
      proc-exp)

    (expression
      ("(" expression expression ")")
      call-exp)

    (expression
      ("letrec"
       (arbno identifier "(" identifier ")" "=" expression)
       "in" expression)
      letrec-exp)

    ;; explicit-refs

    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (expression
      ("newref" "(" expression ")")
      newref-exp)

    (expression
      ("deref" "(" expression ")")
      deref-exp)

    (expression
      ("setref" "(" expression "," expression ")")
      setref-exp)

    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;

(define prog1 (scan&parse "
  let x = newref(0)
  in letrec even(dummy)
              = if zero?(deref(x))
                then 1
                else begin
                      setref(x, -(deref(x),1));
                      (odd 888)
                     end
            odd(dummy)
             = if zero?(deref(x))
               then 0
               else begin
                       setref(x, -(deref(x),1));
                       (even 888)
                    end
      in begin setref(x,13); (odd 888) end
                          "))
prog1

(define prog2 (scan&parse "
      let g = let counter = newref(0)
              in proc (dummy) begin
                    setref(counter, -(deref(counter), -1)); deref(counter)
                end
      in let a = (g 11)
         in let b = (g 11)
            in -(a,b)
"))
prog2
