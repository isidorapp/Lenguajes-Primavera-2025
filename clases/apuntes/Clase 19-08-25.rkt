#lang play

#|
<Expr> ::= (num <number>)
        |  (add <Expr> <Expr>)
        |  (sub <Expr> <Expr>)
        |  (if0 <Expr> <Expr> <Expr>)
        |  (with <Id> <Expr> <Expr>)
        |  (id <Id>)
|#
;; sintaxis abstracta
;; tipo inductivo para representar
;; expresiones aritméticas
(deftype Expr
  (num n)
  (add left right)
  (sub left right)
  (if0 c left right)
  (with id named-expr body)
  (id x))

;; calc :: Expr -> Number
;; reduce una expresión aritmética
(define (calc e)
  (match e
    [(num n) n]
    [(add left right) (+ (calc left) (calc right))]
    [(sub left right) (- (calc left) (calc right))]
    [(if0 c left right) (zero? (calc c))
                        (calc left)
                        (calc right)]))

;; ej. parsear: (calc (parse '(+ (- 3 4)) 7)))

;; sintaxis concreta
#|
<s-expr> ::= <number>
          |  (list '+ <s-expr> <s-expr>)
          |  (list '- <s-expr> <s-expr>)
          |  (list 'if0 <s-expr> <s-expr> <s-expr>)
          |  (list 'with (list <sym> <s-expr>) <s-expr>)
          |  <sym>
|#
;; parse :: <s-expr> -> Expr
;; trasnforma un programa en sintaxis concreta
;; a su versión en sintaxis abstracta
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ left right) (add (parse left) (parse right))]
    [(list '- left right) (sub (parse left) (parse right))]
    [(list 'if0 c left right) (if0 (parse c) (parse left) (parse right))]
    [(list 'with (list x e) b) #:when (symbol? x) (with x (parse e) (parse b))]))

;;ej. with
;;(parse (with (x (+ 5 5)) (+ x x)))


;; run :: <s-expr> -> Number
;; calcula el parseo de una expresión
(define (run prog)
  (calc (parse prog)))

