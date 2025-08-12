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
