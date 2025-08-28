#lang play

(print-only-errors #t)

;; Definir la gramatica concreta

#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
|#

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x))

;; parse :: s-expr -> Expr
;; Parses source code to Expr AST.
(define (parse s-expr)
  (match s-expr
    ;; atoms
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    ;; s-exprs
    [(list '+ l-expr r-expr)
     (add (parse l-expr) (parse r-expr))]
    [(list '- l-expr r-expr)
     (sub (parse l-expr) (parse r-expr))]
    [(list 'if0 c-expr t-expr f-expr)
     (if0 (parse c-expr) (parse t-expr) (parse f-expr))]
    [(list 'with (list (? symbol? x) named-expr-expr) body-expr) ;; <-- '{with {x {+ 1 1}} {+ 1 x}}
     (with x (parse named-expr-expr) (parse body-expr))]
    ))

(test (parse '1) (num 1))
(test (parse '{+ 2 1}) (add (num 2) (num 1)))
(test (parse '{- 2 1}) (sub (num 2) (num 1)))
(test (parse '{if0 {+ 1 1} 2 1}) (if0 (add (num 1) (num 1)) (num 2) (num 1)))
(test (parse 'x) (id 'x))
(test (parse '{with {x {+ 5 5}} {+ x x}}) (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))

#|
;; calc :: Expr -> number
;; Evaluates an arithmetic expression
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c t f)
     (if (zero? (calc c))
         (calc t)
         (calc f))]))

(test (calc (parse '1)) 1)
(test (calc (parse '{+ 2 1})) 3)
(test (calc (parse '{+ {if0 0 {+ {if0 1 1 2} 3} 9} 8})) 13)
(test (calc (parse '{- 2 1})) 1)
(test (calc (parse '{if0 {+ 1 1} 2 1})) 1)
|#

;; interp :: Expr -> number
;; Evaluates an arithmetic expression
(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(if0 c t f)
     (if (zero? (interp c))
         (interp t)
         (interp f))]
    [(with x named-expr body) #f]
    [(id x) #f]))

(test (interp (parse '1)) 1)
(test (interp (parse '{+ 2 1})) 3)
(test (interp (parse '{+ {if0 0 {+ {if0 1 1 2} 3} 9} 8})) 13)
(test (interp (parse '{- 2 1})) 1)
(test (interp (parse '{if0 {+ 1 1} 2 1})) 1)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 10)
(test (interp
       (parse '{with {x {+ 5 5}}
                     {with {y {- 3 4}}
                           {+ x y}}}))
      9)
(test (interp
       (parse '{with {x {+ 5 5}}
                     {with {y {- 3 x}}
                           {+ x y}}}))
      3)





