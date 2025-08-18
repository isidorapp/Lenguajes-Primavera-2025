#lang play

;; negate :: (A -> Bool) -> (A -> Bool)
;; Retorna un predicado que es la negación del argumento dado
(define (negate p)
  (lambda (a)
    (not (p a))))

(test (not (odd? 1)) ((negate odd?) 1))

;; reject :: (A -> Bool) List[A] -> List[A]
;; Crea una nueva lista donde se remueven los elementos que cumplen el predicado
(define (reject p l)
  (filter (negate p) l))

(test (reject odd? '(1 2 3 4 5)) '(2 4))
(test (reject even? '(1 2 3 4 5)) '(1 3 5))

;; compose :: (A -> B) (B -> C) -> (A -> C)
;; Compone funciones f y g
(define (compose f g)
  (λ (x)
    (f (g x))))

;; apply-f-n
(define (add1 x) (+ x 1))

(define (apply-f-n f n)
  (lambda (x)
    (if (= n 1) ;; caso base
        (f x)   ;; caso base
        (f ((apply-f-n f (- n 1)) x)) ;; caso recursivo
 )))

(test ((apply-f-n add1 5) 1) 6)








