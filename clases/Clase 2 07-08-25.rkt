#lang racket

;factorial
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

;ejercicios ppt
(define (my-max a b)
  (cond
     [(> a b) a]
     [(<= a b) b]))

(define (pick-random a b)
    (cond
      [(> (random) 0.5) a]
      [else b]))


