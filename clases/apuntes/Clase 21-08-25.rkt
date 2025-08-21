#lang play

#|
(with (x (+ 5 5)) (with (y (- 3 x)) (+ y (+ x z))))

binding occurrence: primera ocurrencia (ej. de x) introduce el identificador
bound occurrences: siguientes ocurrencias dentro del scope (alcance) de la binding instance, hacen referencia a ella
free occurrence: no está en el scope de ningún with (binding instance propio) (ej. z)

open expression: al menos una free occurrence de un identificador
closed expression: no tiene free occurrences de identificadores (a estas sí se les puede hacer semántica)

semántica de expresiones aritméticas con identificadores
ej.
(with (x (+ 5 5)) (with (y (- 3 x)) (+ x y)))
(with (x 10) (with (y (- 3 x)) (+ x y)))
(with (y (- 3 10)) (+ 10 y))
(with (y -7) (+ 10 y))
(+ 10 -7)
(3)

ej.
(with (x 10) (with (x 1) x)) -> 1
cuando hay colisión de scopes, tiene precedencia el binder más interno

with (x 10) (+ x (with (x 1) x)) -> 11

**definición subst y redef calc en Clase 19-08-25**
|#

;; Clase 21-08-25

;; eval :: Expr listof(FunDef) -> number

;; Definition of a function
;; <FunDef> ::= (FunDef <id> <id> <expr>
(deftype FunDef
  (fundef name arg body))

;; look-up :: symbol listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [(list) (error 'look-up "not found")]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                           head
                           (look-up f-name tail))]))
    