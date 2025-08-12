;; VENTANA DE DEFINICIONES
#lang racket

;; factorial(n) = 1 si n = 0; n * factorial (n-1) en otro caso;

(define (factorial n)
  (if (zero? n)
      1 ;; <--- rama caso true #t
      (* n (factorial (- n 1))) ;; <-- else

      )) ;; rama caso false #f

;; definiciones globales
(define pi 3.14)          ;; <-- esto es definir un identificador global asociado a un valor
;; (define (pi 3.14) ...)    <--- esto sería definir una función

;; definiciones locales
(let ([x 1])
  (+ x x))

(let ([x 1]
      [y 2])
  (+ x y))

#|
(let ([x 1]
      [y (+ x 1)])
  (+ x y))
|#

;; let anidados explícitos
(let ([x 1])
  (let ([y (+ x 1)])
    (+ x y)))

;; let*: anidación implícita
(let* ([x 1]
       [y (+ x 1)])
  (+ x y))

;; square :: Number -> Number <--- CONTRATO: el invocador de la función, y el implementador de la función
;; El invocador: debe proveer los parámetros con los tipos correctos.
;; El implementador: si se proveen los parámetros con los tipos correctos, la función debe responder con una solución correcta y del tipo indicado.
;; Calcula el cuadrado de un número dado
(define (square x)
  (* x x))

(define (square2 x)
  (if (number? x)
      (* x x)
      (error "Debe ser un numero")))

;; linear :: Number Number Number -> Number
;; calcula el valor ax+b
(define (linear x a b)
  (+ (* a x) b))

;; cuadratic :: Number Number Number -> Number
;; calcula el valor ax^2+b
(define (cuadratic x a b)
  (+ (* a (square x) b)))


;; abs-value-if :: Number -> Number
;; calcula el valor absoluto de x
(define (abs-value-if x)
  (if (>= x 0)
      x
      ;;(- x)))
      (* -1 x)))

;; abs-value-cond :: Number -> Number
;; calcula el valor absoluto de x
(define (abs-value-cond x)
  (cond
    [(> x 0) x]
    [(= x 0) 0]
    [(< x 0) (- x)]))


;; solve-cuadratic-no-let :: Number Number Number -> Number
;; encuentra una solución real para ax^2+bx+c, si existe.
(define (solve-cuadratic-no-let a b c)
  (if (> (- (* b b) (* 4 a c)) 0)
      (+ (- b) (/ (sqrt (- (* b b) (* 4 a c))) (* 2 a)))
      (error "No real solution")))

;; solve-cuadratic-let :: Number Number Number -> Number
;; encuentra una solución real para ax^2+bx+c, si existe.
(define (solve-cuadratic-let a b c)
  (let ([discriminant (- (* b b) (* 4 a c))])
    (if (> discriminant 0)
        (+ (- b) (/ (sqrt discriminant) (* 2 a)))
        (error "No real solution"))))


;; my-max :: Number Number -> Number
;; Mayor entre a y b
(define (my-max a b)
  (if (>= a b)
      a
      b))

;; pick-random :: Number Number -> Number
;; Elige al azar entre a y b
(define (pick-random a b)
  (let ([coin (random)])
    (if (< coin 0.5)
        a
        b)))

;; pick-random-in-interval :: Number Number -> Number
;; Retorna un valor aleatorio en el intervalo [a ... b] (supuesto a < b)
(define (pick-random-in-interval a b)
  (+ a (* (random) (- b a))))  
  
(define p1 (cons 1 2))

(define p2 (cons "hola" #f))

(define l1 (cons 'a (cons 1 (cons #t '()))))

(define l2 (list 'a 1 #t))

;; sum-list :: List[Number] -> Number
;; Suma los valores de una lista de números
(define (sum-list l)
  (cond
    [(empty? l) 0] ;; caso base
    [else (+ (first l) (sum-list (cdr l)))])) ;; caso recursivo





















