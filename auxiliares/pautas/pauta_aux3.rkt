#lang play

(print-only-errors #t)

;;P1

;;a)
;;pitatoria :: ListOf[Int] -> Int
;;Calcula la multiplicación entre todos los numeros de la lista
(define (pitatoria lista)
  (foldl * 1 lista))

(test (pitatoria '(2 3 4)) 24)


;;b)
;;invertir :: ListOf[A] -> ListOf[A]
;;Invierte la lista
(define (invertir lista)
  (foldl cons '() lista))

(test (invertir '(a b c)) '(c b a))


;;c)
;;filtrar-pares :: ListOf[Int] -> ListOf[Int]
;;Deja solo los elementos pares
(define (filtrar-pares lista)
  (foldr (lambda (x acc) 
           (if (even? x) 
               (cons x acc) 
               acc))
         '() lista))


(test (filtrar-pares '(1 2 3 4 5 6)) '(2 4 6))

;;d)
;;eliminar-duplicados:: ListOf[A] -> ListOf[A]
;;elimina los duplicados
(define (eliminar-duplicados lista)
  (foldr (lambda (x acc) 
           (if (member x acc) 
               acc 
               (cons x acc)))
         '() lista))

(test (eliminar-duplicados '(1 2 2 3 3 3 4)) '(1 2 3 4))


;;e)
;;suma-multiplos :: ListOf[Int] ListOf[Int] -> Int
;;Suma de productos de elementos correspondientes
(define (suma-multiplos l1 l2)
  (foldl (lambda (x y acum) 
         (+ acum (* x y)))
       0 
       l1 
       l2))

; Calcula: (1*4) + (2*5) + (3*6) = 4 + 10 + 18 = 32
(test (suma-multiplos '(1 2 3) '(4 5 6)) 32)




;;------------P2---------------
;; Variaciones de los ejercicios propuestos en clase

;a)
#|
<NTree> ::= (leaf <Number>)
            (in-node <Number> <NTree>*)
|#

(deftype NTree
  (nleaf v)
  (nnode v childs))


;b)
;; fold-ntree :: (Number -> A) (Number ListOf(A) -> A) -> (NTree -> A)
(define (fold-ntree f g)
   (λ (bt)
     (match bt
       [(nleaf v) (f v)]
       [(nnode v childs) (g v (map (fold-ntree f g) childs))]
       ))
  )

; Al igual que bintree tenemos que aplicar recursivamente foldntree a cada hoja,
; para eso usamos map para aplicar lant funcion con los argumentos incluidos.

;c)
;(sum-n-tree bt) :: NTree -> Number
;; hace la suma de todos los valores internos y hojas
(define (sum-n-tree bt)
  ( (fold-ntree (λ (v) v)
                (λ (v childs) (+ v (foldl + 0 childs)))) ; Tambien puede ser (apply + leaves)
                bt)
  )

(test (sum-n-tree (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) 26  )
(test (sum-n-tree (nleaf 2)) 2 )

;; contains-n-tree? :: NTree Number -> Boolean
;; retorna #t valor v se encuentra en algún nodo del árbol n-ario nt y #f si no
(define (contains-n-tree? nt v)
  ( (fold-ntree (λ (x) (equal? x v))
          (λ (x childs) (if (equal? x v)
                         #t
                         (foldl (λ (x y) (or x y)) #f childs)))) ; No se puede usar apply o directamente or por que or
                                                                 ;; NO es una función, es una MACRO
    nt))

(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 12) #f)
(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 4) #t)
(test (contains-n-tree? (nleaf 2) 12) #f)

;; select-n-tree:: (A -> Boolean) NTree -> ListOf(Numbers)
;; lista de valores del árbol n-ario nt que satisfacen el predicado p.
(define (select-n-tree p nt)
  ( (fold-ntree (λ (v) (if (p v)
                             (list v)
                             '()))
                  (λ (v childs) (if (p v)
                                 (append (list v) (apply append childs)) ; Como nuestro caso base devuelve una lista, hay que aplicar append a cada elemento de childs
                                 (apply append childs)))                 ; si no, se arma una lista de listas anidadas.
                  ) nt)
  )

(test (select-n-tree even? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) (list 2 4 6 10))
(test (select-n-tree (λ (x) (< 10 x)) (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) '())



;;------------P3---------------
;;a)
#|
<Pol> ::= (nullp)
          (plus <Int> <Int> <Pol>)
|#
(deftype Polynomio
  (nullp)
  (plus coef grado resto))


;;b)
;----------;obtener coeficiente-----------

;; ;; get-list-coef :: ListOf[Char] -> ListOf[Char]
;; toma una lista de characeteres que es el polinomio y retorna la lista
;; de characters hasta encontrar el x, sin incluir el x
(define (get-list-coef pol)
  (if (equal? (car pol) #\x)
      '()
      (append (list (car pol)) (get-list-coef (cdr pol)))
      ))

;; get-coef :: ListOf[Char] -> Number
;:; toma un polinomio escrito en characteres y saca su coef
(define (get-coef pol)
  (string->number (list->string (get-list-coef pol))))

(test (get-coef (list #\2 #\1 #\x #\2 #\3)) 21)

;; --------------obtener grado---------


;; ;; get-list-grado :: ListOf[Char] -> ListOf[Char]
;; toma una lista de characeteres que es el polinomio y retorna la lista
;; de characters hasta encontrar el ^
(define (get-list-grado pol)
    (if (equal? (car pol) #\^)
      (cdr pol)
      (get-list-grado (cdr pol))
      ))

;; get-coef :: ListOf[Char] -> Number
;:; toma un polinomio escrito AL REVÉS en characteres y saca su grado
(define (get-grado pol)
  (string->number (list->string (get-list-grado pol))))

(test (get-grado (list #\2 #\1 #\x #\^ #\2 #\3)) 23)

;;-------Parse--------

(define (parse-p pol)
    (match pol
      ;;forma (2x^1 + ...)
      [ (list polinomio '+ resto ...) (let ([pol-lista (string->list (symbol->string polinomio))])
                                 (plus (get-coef pol-lista) (get-grado pol-lista) (parse-p resto))) ]
      ;;forma (2)
      [ (list n) #:when (number? n) (plus n 1 (nullp)) ]
      ;;forma (2x^2)
      [ (list polinomio) (let ([pol-lista (string->list (symbol->string polinomio))])
                                        (plus (get-coef pol-lista) (get-grado pol-lista) (nullp))) ]
      ))
  

(test (parse-p '(23x^5 + 2x^2 + 1x^1)) (plus 23 5 (plus 2 2 (plus 1 1 (nullp)))));;monomios completos
(test (parse-p '(23x^5 + 2x^-3 + 1x^12)) (plus 23 5 (plus 2 -3 (plus 1 12 (nullp))))) ;;grado de mas de 1 digito
(test (parse-p '(23x^5 + -2x^2 + 5)) (plus 23 5 (plus -2 2 (plus 5 1 (nullp))))) ;;monomio grado cero y coef negativo


;c)
;; fold-p :: A (Number Number A -> A) -> (Polynomio -> A)
;; abstrae el patrón recursivo en los polinomios
(define (fold-p f g)
  (λ (p)
      (match p
        [(nullp) f]
        [(plus c grado resto) (g c grado ((fold-p f g) resto)) ]
        )))


;;d)
;;eval :: Number Polynomio -> Number
;; evalua el polinomio
(define (eval n p)
  ((fold-p 0
          (λ (c g resto) (+ (* c (expt n g)) resto))) p)
  )

(test (eval 3 (parse-p '(-23x^5 + 2x^2 + 1x^1))) -5568)
(test (eval 0 (parse-p '(-23x^5 + 2x^2 + 1x^1))) 0)
