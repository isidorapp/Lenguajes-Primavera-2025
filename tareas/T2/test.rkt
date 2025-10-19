#lang play

#|
Nombre: Isidora Calderón Pérez
|#

(require "T2.rkt")
(print-only-errors #t)

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

#| Parte B |#
;; cortar-ceros (función auxiliar)
(test (cortar-ceros '()) '())
(test (cortar-ceros '(0)) '())
(test (cortar-ceros '(1 0)) '(1))
(test (cortar-ceros '(1 2 0 0)) '(1 2))
(test (cortar-ceros '(1 2 3)) '(1 2 3))
(test (cortar-ceros '(1 0 1 0 1 0 0 0 0 0 1 0)) '(1 0 1 0 1 0 0 0 0 0 1))

;; parse
(test (parse 0) (poly '(0)))
(test (parse '(3 1 0 0)) (poly '(3 1)))
(test (parse 'x) (id 'x))
(test (parse '(+ (3 5 7) (1 2))) (add (poly '(3 5 7)) (poly '(1 2))))
(test (parse ' (* (2 1) (1 2))) (mul (poly '(2 1)) (poly '(1 2))))
(test (parse '(if0 (+ (3 5) (-3 2)) 1 0))
      (if0 (add (poly '(3 5)) (poly '(-3 2))) (poly '(1)) (poly '(0))))
(test (parse '(with x (1 2) (* 2 x)))
      (with 'x (poly '(1 2)) (mul (poly '(2)) (id 'x))))
(test (parse 5) (poly '(5)))
(test (parse 'y) (id 'y))
(test (parse '(1 0 0 0)) (poly '(1)))
(test (parse '(0 0 1)) (poly '(0 0 1)))
(test (parse '(1 2 3 4)) (poly '(1 2 3 4)))
(test (parse '(+ (+ 1 2) 3)) 
      (add (add (poly '(1)) (poly '(2))) (poly '(3))))
(test (parse '(* (* 2 3) 4)) 
      (mul (mul (poly '(2)) (poly '(3))) (poly '(4))))
(test (parse '(with x 1 (+ x x))) 
      (with 'x (poly '(1)) (add (id 'x) (id 'x))))
(test (parse '(if0 0 (if0 1 2 3) 4))
      (if0 (poly '(0)) (if0 (poly '(1)) (poly '(2)) (poly '(3))) (poly '(4))))

#| Parte C |#
;; sum-poly (función auxiliar)
(test (sum-poly '() '()) '())
(test (sum-poly '(1) '()) '(1))
(test (sum-poly '() '(2)) '(2))
(test (sum-poly '(1 2) '(3 4)) '(4 6))
(test (sum-poly '(1 2 3) '(4 5)) '(5 7 3))

;; mul-sec (función auxiliar)
(test (mul-sec '() '()) '())
(test (mul-sec '(3) '(1 2)) '(3 6))
(test (mul-sec '(1 2) '(3 4)) '(3 10 8))
(test (mul-sec '(0 1) '(0 1)) '(0 0 1))
(test (mul-sec '(1 1) '(1 -1)) '(1 0 -1))
(test (mul-sec '(2 0 3) '(1 4)) '(2 8 3 12))

;; mul-poly (función auxiliar)
(test (mul-poly '() '()) '())
(test (mul-poly '(1) '()) '(1))
(test (mul-poly '() '(2)) '(2))
(test (mul-poly '(2) '(3)) '(6))
(test (mul-poly '(1 2) '(3 4)) '(3 10 8))

;; reduce
(test (reduce (poly '(3 1 0)) empty-env)
      (poly '(3 1)))
(test/exn (reduce (id 'x) empty-env)
          "reduce: variable x is not defined")
(test (reduce (id 'x) (extend-env 'x (poly '(1)) empty-env))
      (poly '(1)))
(test (reduce (add (poly '(3 5 7)) (poly '(1 2))) empty-env)
      (poly '(4 7 7)))
(test/exn (reduce (id 'y) empty-env)
          "reduce: variable y is not defined")
(test (reduce (mul (poly '()) (poly '(1 2 0 2 0 0))) empty-env)
      (poly '(1 2 0 2)))
(test (reduce (mul (poly '(0 0 3 0 0)) (poly '())) empty-env)
      (poly '(0 0 3)))
(test (reduce (mul (poly '(2 1)) (poly '(1 2))) empty-env)
      (poly '(2 5 2)))
(test (reduce (with 'z (poly '(0)) (if0 (id 'z) (poly '(9)) (poly '(8)))) empty-env)
      (poly '(9)))
(test (reduce (with 'y (poly '(2 3)) (add (id 'y) (poly '(1)))) empty-env)
      (poly '(3 3)))
(test (reduce (if0 (poly '(2)) (poly '(1 2)) (poly '(3 4))) empty-env)
      (poly '(3 4)))
(test (reduce (if0 (poly '(0)) (poly '(1 2)) (poly '(3 4))) empty-env)
      (poly '(1 2)))
(test (reduce (with 'x (poly '(1 2)) (mul (poly '(2)) (id 'x))) empty-env)
      (poly '(2 4)))


;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Parte B |#
;; parser
(test (parser '1) (real 1))
(test (parser '(1 i)) (imaginary 1))
(test (parser '(+ 2 (1 i))) (addc (real 2) (imaginary 1)))
(test (parser '(with [(x 1) (y 1)] (+ x y)))
      (withc (list (cons 'x (real 1)) (cons 'y (real 1))) (addc (idc 'x) (idc 'y))))
(test/exn (parser '(with [] 1))
          "parser: *with* expects at least one definition")
(test (parser 'x) (idc 'x))
(test (parser '(- 2 (1 i))) (subc (real 2) (imaginary 1)))
(test (parser '(if0 0 1 2)) (if0c (real 0) (real 1) (real 2)))

#| Parte C |#
;; from-CValue
(test (from-CValue (compV 3 0)) (real 3))
(test (from-CValue (compV 0 4)) (addc (real 0) (imaginary 4)))
(test (from-CValue (compV 2 3)) (addc (real 2) (imaginary 3)))

;; cmplx+
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx+ (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx+ (compV -1 -2) (compV 3 4)) (compV 2 2))

;; cmplx-
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))
(test (cmplx- (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx- (compV 5 6) (compV 1 2)) (compV 4 4))

;; cmplx0?
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 0)) #f)
(test (cmplx0? (compV 1 1)) #f)

#| Parte D |#
;; subst-defs (función auxiliar)
(test (subst-defs '() 'x (real 1)) '())
(test (subst-defs (list (cons 'y (real 2))) 'x (real 1))
      (list (cons 'y (real 2))))
(test (subst-defs (list (cons 'x (real 2))) 'x (real 1))
      (list (cons 'x (real 2))))
(test (subst-defs (list (cons 'x (real 2)) (cons 'y (idc 'x)) (cons 'z (real 3))) 'x (real 1))
      (list (cons 'x (real 2)) (cons 'y (idc 'x)) (cons 'z (real 3))))
(test (subst-defs (list (cons 'a (real 1)) (cons 'x (real 2)) (cons 'b (idc 'x))) 'x (real 1))
      (list (cons 'a (real 1)) (cons 'x (real 2)) (cons 'b (idc 'x))))
(test (subst-defs (list (cons 'a (idc 'x)) (cons 'b (idc 'x)) (cons 'x (real 2))) 'x (real 1))
      (list (cons 'a (real 1)) (cons 'b (real 1)) (cons 'x (real 2))))
(test (subst-defs (list (cons 'x (real 1)) (cons 'y (idc 'x)) (cons 'x (real 3))) 'x (real 99))
      (list (cons 'x (real 1)) (cons 'y (idc 'x)) (cons 'x (real 3))))

;; shadowing (función auxiliar)
(test (shadowing 'x '()) #f)
(test (shadowing 'x (list (cons 'x (real 2)) (cons 'y (idc 'x)))) #t)
(test (shadowing 'z (list (cons 'x (real 2)) (cons 'y (idc 'z)))) #f)

;; subst
(test (subst (parser '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
      (withc (list (cons 'x (real 2)) (cons 'y (real 1))) 
             (addc (idc 'x) (real 1))))
(test (subst (parser '(with [(x 2) (y x)] (+ x x))) 'x (real 1))
      (withc (list (cons 'x (real 2)) (cons 'y (idc 'x))) 
             (addc (idc 'x) (idc 'x))))
(test (subst (real 5) 'x (real 1)) (real 5))
(test (subst (idc 'x) 'x (real 1)) (real 1))
(test (subst (idc 'y) 'x (real 1)) (idc 'y))
(test (subst (addc (idc 'x) (idc 'y)) 'x (real 1)) 
      (addc (real 1) (idc 'y)))

#| Parte E |#
;; interp
(test (interp (real 5)) (compV 5 0))
(test (interp (imaginary 3)) (compV 0 3))
(test (interp (addc (real 2) (imaginary 3))) (compV 2 3))
(test (interp (subc (real 5) (real 3))) (compV 2 0))
(test (interp (if0c (real 0) (real 1) (real 2))) (compV 1 0))
(test (interp (if0c (real 1) (real 1) (real 2))) (compV 2 0))

(test/exn (interp (idc 'x)) "Free occurrence of variable x")
(test/exn (interp (addc (real 1) (idc 'y))) "Free occurrence of variable y")
(test/exn (interp (subc (idc 'a) (real 2))) "Free occurrence of variable a")
(test/exn (interp (if0c (idc 'b) (real 1) (real 2))) "Free occurrence of variable b")
(test/exn (interp (withc (list (cons 'x (real 1))) (idc 'c))) "Free occurrence of variable c")
(test/exn (interp (addc (real 1) (addc (real 2) (idc 'd)))) "Free occurrence of variable d")
(test/exn (interp (withc (list (cons 'x (real 1))) 
                         (withc (list (cons 'y (real 2))) 
                                (addc (idc 'x) (idc 'z))))) 
          "Free occurrence of variable z")
(test/exn (interp (parser '(with [(x 1)] e))) "Free occurrence of variable e")

(test (interp (parser '(if0 1 1 2))) (compV 2 0))
(test (interp (parser '(if0 0 1 2))) (compV 1 0))
(test (interp (parser '(with [(x 2) (y 3)] (+ x y)))) (compV 5 0))
(test (interp (parser '(with [(a 1) (b 2) (c 3)] (+ a (+ b c))))) (compV 6 0))
(test (interp (parser '(with [(a 1)] (with [(b 2)] (+ a b))))) (compV 3 0))



