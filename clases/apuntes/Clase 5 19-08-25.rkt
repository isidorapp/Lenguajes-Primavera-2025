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
;; trasnforma un programa en sintaxis concreta (s-expr)
;; a su versión en sintaxis abstracta (Expr)
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ left right) (add (parse left) (parse right))]
    [(list '- left right) (sub (parse left) (parse right))]
    [(list 'if0 c left right) (if0 (parse c) (parse left) (parse right))]
    [(list 'with (list x e) b) #:when (symbol? x) (with x (parse e) (parse b))]))


;; Clase 21-08-25

;; subst: Expr symbol Expr -> Expr
;; (subst in what for) susituye toda free occurrence del
;; identificador 'what' en la expresión 'in' para la expresión 'for'
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add left right) (add (subst left what for)
                           (subst right what for))]
    [(sub left right) (sub (subst left what for)
                           (subst right what for))]
    [(if0 c left right) (if0 (subst c what for)
                             (subst left what for)
                             (subst right what for))]
    [(with x named-expr body) (with x
                                     (subst named-expr what for)
                                     (if (symbol=? x what)
                                         body            ;; what (=x) no ocurre libre en body
                                         (subst body what for)))]
    [(id x) (if (symbol=? x what)
                for
                (id x))]))
    

;; calc :: Expr -> Number
;; reduce una expresión aritmética
(define (calc e)
  (match e
    [(num n) n]
    [(add left right) (+ (calc left) (calc right))]
    [(sub left right) (- (calc left) (calc right))]
    [(if0 c left right) (zero? (calc c))
                        (calc left)
                        (calc right)]
    [(with x named-expr body) (calc (subst body x (num(calc named-expr))))]
    [(id x) (error 'calc "expresión abierta: ocurrencia libre de ~a" x)]))


;; run :: <s-expr> -> Number
;; calcula el parseo de una expresión
(define (run prog)
  (calc (parse prog)))

