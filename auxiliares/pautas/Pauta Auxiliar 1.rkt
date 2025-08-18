#lang play
(print-only-errors #t)


#|
a) La diferencia es que (list 'a 'b) es igual a (cons 'a (cons 'b '()), es decir, es una lista enlazada, mientras que (cons 'a 'b) es solo un par

b) Debemos construir ( list 1 '(2 ' (3 . 4)) #f)

ver que para construir (list 1 2 3) hay que hacer (cons 1 (cons 2 (cons 3 '())))

entonces si construimos (cons 1 (cons x (cons #f '()))

tenemos todo excepto que el elemento x debemos remplazarlo por '(2 '(3 . 4))

y '(2 ' (3 . 4) es lo mismo que (list 2 (cons 3 4)) que seria lo mismo que (cons 2 (cons (cons 3 4) '()))

concluyendo en el siguiente resultado:

(cons 1 (cons (cons 2 (cons (cons 3 4) '())) (cons #f '())))
|#

(test ( list 1 '(2 (3 . 4)) #f) (cons 1 (cons (cons 2 (cons (cons 3 4) '())) (cons #f '()))))

#|

c) b se obtiene con (car (car (cdr (car l))))

   f se obtiene con (car (cdr (cdr (car (cdr l)))))


|#

(define l (list '(a (b c)) '(d e f)))
(test (car (car (cdr (car l)))) 'b)
(test (car (cdr (cdr (car (cdr l))))) 'f)

#|

d) '(a . b) -> (cons 'a 'b)
   '(a b) -> (cons 'a (cons 'b '()))
   '(a (b . c) d) -> (cons 'a (cons (cons 'b 'c) (cons 'd '())))

|#

(test (cons 'a 'b) '(a . b))
(test (cons 'a (cons 'b '())) '(a b))
(test (cons 'a (cons (cons 'b 'c) (cons 'd '()))) '(a (b . c) d))


;;--------------P2-------------------

;;a)
;;sum-coins :: Number Number Number -> Number
;;calcula el monto total a partir de monedas de 50, 100 y 500
(define (sum-coins c50 c100 c500)
  (+ (* c50 50) (* c100 100) (* c500 500))
  )
         
(test (sum-coins 1 1 1) 650)
(test (sum-coins 0 0 1) 500)
(test (sum-coins 0 1 0) 100)
(test (sum-coins 1 0 0) 50)


;;b)
;;sumar-multiplos :: ListOf[Number] -> Number
;;Suma los numeros de la lista que son múltiplos de 3 o de 5 (ambos incluidos)
;;Se asume que todos los numeros son positivos

(define (sumar-multiplos l)
  (match l
  ['() 0]
  [(list n resto ...)(if(or (zero? (modulo n 3))(zero? (modulo n 5)))
                        (+ n (sumar-multiplos resto))
                        (sumar-multiplos resto))
                         ]))
                            
                                
         
(test (sumar-multiplos '(3 5 7 9 10)) 27)
(test (sumar-multiplos '(1 2 4)) 0)
(test (sumar-multiplos '()) 0)

;;c)
;;pitatoria :: Number Number -> Number
;;calcula la pitatoria desde un inicio hasta un final dado

(define (pitatoria i j)
  (if (equal? i j)
      i
      (* j (pitatoria i (- j 1)))))

(test (pitatoria 1 2) 2)
(test (pitatoria 0 5) 0)
(test (pitatoria 2 4) 24)
(test (pitatoria 3 3) 3)


;;d)
;;lista_impares :: Number -> ListOf[Number]
;;crea la lista de largo n entregado de impares

(define (lista_impares n)
  (define (lista_impares_aux n* l)
    (cond [ (< n* 0) (error "Not negatives length allows")]
          [ (zero? n*) l ]
          [ else (lista_impares_aux (- n* 1) (append (list (- (* 2 n*) 1)) l)) ]
          ))
  (lista_impares_aux n '()))


(test (lista_impares 3) (list 1 3 5))
(test (lista_impares 0) '())
(test (lista_impares 5) (list 1 3 5 7 9))
(test/exn (lista_impares -1) "Not negatives length allows")

;;solucion alternativa sin usar una funcion auxiliar
(define (lista_impares2 n)
      (cond [ (< n 0) (error "Not negatives length allows")]
          [ (zero? n) '() ]
          [ else (append (lista_impares2 (- n 1)) (list (- (* 2 n) 1))) ]
          ))

(test (lista_impares2 3) (list 1 3 5))
(test (lista_impares2 0) '())
(test (lista_impares2 5) (list 1 3 5 7 9))
(test/exn (lista_impares2 -1) "Not negatives length allows")
