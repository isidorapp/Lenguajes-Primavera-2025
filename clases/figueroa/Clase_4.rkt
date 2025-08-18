#lang play

(define (f v)
  (match v
    ["hola" (print "v es el string 'hola'")]
    [23 (print "v es el número 23")]))

(define (g v)
  (match v
    [(cons 1 (cons "hola" 'f)) (print "par literal")]
    [(list 1 "h" (cons 3 4)) (print "lista literal")]
    [else (print "otro caso")]))

(define (h v)
  (match v
    [a (printf "anything: ~a" a)]))

(define (j v)
  (match v
    [(cons a a) (printf "car: ~a cdr: ~a IGUALES" a a)] ;;     
    [(cons a b) (printf "car: ~a cdr: ~a DISTINTOS" a b)]
    [else (print "not a pair")]))

#|
chequea patrón (cons a a) versus el valor (cons 1 2)
--> asigno variable "a" al valor 1
--> asigno variable "a" al valor 2
--> genera una contradicción/error

|#

(define (my-length-cons l)
  (match l
    ['() 0]
    [(cons first rest) (+ 1 (my-length-cons rest))]))

(define (my-length-list l)
  (match l
    ['() 0]
    [(list first rest ...) (+ 1 (my-length-list rest))]))

(define (match-binop l)
  (match l
    [(list '+ a b) (printf "sum of ~a and ~a" a b)]
    [(list '- a b) (printf "sub of ~a and ~a" a b)]
    [(list '* a b) (printf "mul of ~a and ~a" a b)]))

#|
<BinTree> ::= (leaf <value>)
            | (in-node <value> <BinTree> <BinTree>)
|#
(deftype BinTree
  (leaf v)
  (in-node v l r))

(define b1 (leaf 4))
(define b2 (in-node 5 (leaf 3) (in-node 2 (leaf 0) (leaf 1))))

;; height :: BinTree -> number
;; Computes height of a binary tree
(define (height bt)
  (match bt
    [(leaf _) 0]
    [(in-node _ left right)
     (+ 1 (max (height left) (height right)))]))














