#lang play

;; Optimizaciones de recursión

#|
TCO - tail call optimization: llamada en posición de
cola no crea un nuevo stack frame

TRO - tail recursive optimization: caso particular
de TCO para funciones recursivas
|#

;; recursión normal: crea contexto alrededor de llamadas
;; recursivas
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

;; recursión de cola: la llamada recursiva es la
;; última operación
(define (tail-fact accum n)
  (if (zero? n)
      accum
      (tail-fact (* accum n) (sub1 n))))

;; fibonacci recursión normal
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; fibonacci recursivo por la cola
(define (tail-fib acc1 acc2 n)
  (if (zero? n)
      acc1
      (tail-fib acc2 (+ acc1 acc2) (sub1 n))))

;; GCD recursivo por la cola
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

#|
Trampoline

una estructura TailCall puede ser:
1. el valor final de la computación
2. una función sin argumentos (thunk) que encapsula
qué sigue por hacer

el resultado internaliza el control:
- si la computación está lista, retorna valor final
- sino, aplica un paso de computación (ej. ejecutar
el siguiente thunk)

result es recursivo por la cola (si se implementa en
un lenguaje sin TRO, se usa ciclo while)
|#

(deftype TailCall
  (call rest)
  (done val))

(define (result tc)
  (match tc
    [(done val) val]
    [(call rest) (result (rest))]))


;; ejemplo de uso con TRO
(define (even-tr n)
  (match n
    [0 (done #t)]
    [1 (done #f)]
    [else (call (λ () (odd-tr (sub1 n))))]))

(define (odd-tr n)
  (match n
    [0 (done #t)]
    [1 (done #f)]
    [else (call (λ () (even-tr (sub1 n))))]))
         


