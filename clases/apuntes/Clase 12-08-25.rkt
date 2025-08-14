#lang play
(define l (list 1 2 3 4))
(define v (vector 1 2 3 4))

(define (my-double n)
  (* n 2))

(define my-double-lambda
  (lambda (n) (* n 2)))

(define (addn n)
  (lambda (x) (+ x n)))

;ejercicios clases
(define (negate p)
  (lambda (n) (not (p n))))

(define (reject p l)
  (filter (negate p) l))

;;currificada: toma los argumentos de a 1
(define (apply-twice f)
  (lambda (x) (f (f x))))

;;no currificada: toma los argumentos simultáneamente
(define (a-t f x)
  (f (f x)))

;;f :: A B -> C --currying-> f' :: A -> (B -> C)
(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

;;pattern matching
;;distance :: (cons Int Int) (cons Int Int) -> Float
;;calcula distancia entre dos puntos p1 y p2
(define (distance p1 p2)
  (def (cons x1 y1) p1)
  (def (cons x2 y2) p2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
