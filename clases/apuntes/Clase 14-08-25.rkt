#lang play
(print-only-errors #t) ;;solo imprimir errores

;; Gramatica BNF
#|
<BinTree> ::= (leaf <value>)
           |  (in-node <value> <BinTree> <BinTree>
|#

(deftype BinTree
  (leaf value)
  (in-node value left right))


#|
ej. my-bt

|#

;;Constructores
(define my-bt
  (in-node 5
           (in-node 4
                    (leaf 1)
                    (leaf 8))
           (leaf 3)))

;;Accesores
;(in-node-value my-bt)
;(in-node-left my-bt)
;(in-node-right my-bt)

;;Predicado de tipos
;(BinTree? my-bt)

;;Definición de funciones:
; 1. Firma función
; 2. Comentario de qué hace la función
; 3. Tests
; 4. Implementación función

;; height :: BinTree -> Integer
;; Computa la altura de un árbol binario
(define (height bt)
  (match bt       ;; pattern matching
    [(leaf v) 0]      ;; si v es hoja, altura 0
    [(in-node v l r)     ;; extraer max entre izq y der
     (+ 1 (max (height l) (height r)))])) ;; y sumarle 1

;; tests
(test (height (leaf 6)) 0)
(test (height my-bt) 2)

;; contains? :: (BinTree of A) A -> Bool
;; Verifica si un árbol binario contiene un dato dado
(define (contains? bt n)
  (match bt
    [(leaf v) (equal? v n)]
    [(in-node v l r) (or (equal? v n)
                     (contains? l n)
                     (contains? r n))]))

;; tests
(test (contains? (leaf 5) 5) #t)
(test (contains? (leaf 5) 6) #f)
(test (contains? my-bt 5) #t)
(test (contains? my-bt 10) #f)

#|
;; foo :: BinTree -> A
(define (foo bt)
  (match bt
    [(leaf v) (f v)] ;; f
    [(in-node v l r) (g v (foo l) (foo r))])) ;; g
|#

;; fold-bt :: (A -> B) (A B B -> B) -> (BinTree of A) -> B)
;; captura el esquema de recursión primitiva sobre ab
(define (fold-bt f g)
  (lambda (bt)
    (match bt
      [(leaf v) (f v)] ;; f
      [(in-node v l r) (g v
                          ((fold-bt f g) l)
                          ((fold-bt f g) r))])))

(define height-fbt
  (fold-bt (lambda (x) 0) ;; f cte que devuelve 0
           (lambda (v resl resr) (+ 1 (max resl resr)))))
           ;; f que toma v, el resultado recursivo de l y r


;; sum-bt :: BinTree -> Integer
;; retorna la suma de los elementos de un ab numerico
(define sum-bt
  (fold-bt identity +))

;; max-bt :: BinTree -> Integer
;; retorna el maximo valor de un ab numerico
(define max-bt
  (fold-bt identity max))


           