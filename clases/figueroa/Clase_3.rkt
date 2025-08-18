#lang play

(define l1 '(1 -1 0 -32 14))

(define (add1 n) (+ n 1))

(define l2 '(1 2 3 4 5))

(define (add124 n) (+ n 124))

(define l3 '(1 2 3 4 5 6 7 8))

(define (add2 x) (+ x 2)) ;; (addn 2)
(define (add3 x) (+ x 3)) ;; (addn 3)
(define (add4 x) (+ x 4)) ;; (addn 4)
;; ...
(define (add9999 x) (+ x 9999)) ;; (addn 9999)

(define (addn n)
  (lambda (x)
    (+ x n)))

(define add1* (addn 1))
(define add9999* (addn 9999))

;; ((curry +) 1)
;; --> (curry +)
#| (λ (a)
     (λ (b)
       (+ a b)))

((curry +1) 1)
--> (λ (b)
      (+ 1 b)) --> add1
|#

(define (f x) (+ x x))

(define l4 '(1 2 3))
(define s1 "hola")
(define n1 1)
(define b1 #f)
(define g (lambda (x) (+ x x))) ;; <--> (define (g x) (+ x x))


;; Ejemplo mysqrt

(print-only-errors #t)

;; mysqrt :: Number -> Number
;; Computes square root of a non-negative number,
;; raises an exception if argument is negative.
(define (mysqrt x)
  (if (< x 0)
      (error "mysqrt: negative argument")
      (sqrt x)))

;; Tests
(test (mysqrt 0) 0)
(test (mysqrt 1) 1)
(test (mysqrt 4) 2)
(test (mysqrt 9) 3)
(test/exn (mysqrt -1) "mysqrt: negative argument")


