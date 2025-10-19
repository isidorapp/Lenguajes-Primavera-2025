#lang play
(require "T1.rkt")

(print-only-errors #t)

;; tests parte 1b
(test (occurrences (varp "a") "b") 0)
(test (occurrences (andp (varp "a") (varp "b")) "a") 1)
(test (occurrences (andp (varp "a") (varp "a")) "a") 2)
(test (occurrences (orp (andp (varp "b") (varp "c")) (notp (varp "d"))) "a") 0)
(test (occurrences (andp (orp (notp (varp "b")) (varp "b")) (varp "b")) "b") 3)

;; tests parte 1c
(test (vars (varp "a")) (list "a"))
(test (vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (andp (varp "a") (varp "a"))) (list "a"))
(test (vars (orp (andp (varp "b") (varp "c")) (notp (varp "d")))) (list "b" "c" "d"))
(test (vars (andp (orp (notp (varp "b")) (varp "b")) (varp "b"))) (list "b"))
;; caso Prop sin variables
(define p-sin-vars
  (notp #f))
(test (vars p-sin-vars) '())


;; tests parte 1d
(test (all-environments (list)) (list (list)))
(test (all-environments (list "a")) (list (list (cons "a" #t))
                                          (list (cons "a" #f))))
(test (all-environments (list "a" "b")) (list (list (cons "a" #t) (cons "b" #t))
                                              (list (cons "a" #t) (cons "b" #f))
                                              (list (cons "a" #f) (cons "b" #t))
                                              (list (cons "a" #f) (cons "b" #f))))
(test (all-environments (vars p-sin-vars)) (list (list)))
(test (all-environments (vars (orp (varp "a") (varp "a")))) (list (list (cons "a" #t))
                                                                  (list (cons "a" #f))))
(test (all-environments (vars (andp (orp (varp "a") (varp "b"))(notp (varp "c"))))) (list (list (cons "a" #t) (cons "b" #t) (cons "c" #t))
                                                                                          (list (cons "a" #t) (cons "b" #t) (cons "c" #f))
                                                                                          (list (cons "a" #t) (cons "b" #f) (cons "c" #t))
                                                                                          (list (cons "a" #t) (cons "b" #f) (cons "c" #f))
                                                                                          (list (cons "a" #f) (cons "b" #t) (cons "c" #t))
                                                                                          (list (cons "a" #f) (cons "b" #t) (cons "c" #f))
                                                                                          (list (cons "a" #f) (cons "b" #f) (cons "c" #t))
                                                                                          (list (cons "a" #f) (cons "b" #f) (cons "c" #f))))

;; tests parte 1e
(test (eval (varp "a") (list (cons "a" #t))) #t)
(test (eval (varp "a") (list (cons "a" #f))) #f)
(test (eval (varp "a") (list (cons "a" #t) (cons "b" #f))) #t)
(test (eval (varp "a") (list (cons "a" #f) (cons "b" #t) (cons "c" #t))) #f)
(test/exn (eval (varp "a") '()) "eval: variable \"a\" is not defined in environment")
(test/exn (eval (varp "b") '()) "eval: variable \"b\" is not defined in environment")


;; tests parte 1f
(test (tautology? (orp (varp "a") (notp (varp "a")))) #t)
(test (tautology? (andp (varp "a") (notp (varp "a")))) #f)
(test (tautology? (andp (orp (varp "a") (varp "b")) (andp (notp (varp "a")) (varp "b")))) #f)
(test (tautology? (andp (orp (notp (varp "q")) (varp "p")) (notp (andp (varp "q") (varp "p"))))) #f)
(test (tautology? (andp (notp (orp (andp (varp "p") (varp "q")) (varp "r"))) (andp (varp "q") (orp (varp "r") (varp "q"))))) #f)
(test (tautology? (andp (orp (varp "a") (notp (varp "a"))) (orp (varp "b") (notp (varp "b"))))) #t)
(test (tautology? '()) #t)

;; tests parte 2a
(test(simplify-negations (notp (notp (varp "a")))) (varp "a"))
(test(simplify-negations (notp (andp (varp "a") (varp "b")))) (orp (notp (varp "a")) (notp (varp "b"))))
(test(simplify-negations (notp (orp (varp "a") (varp "b")))) (andp (notp (varp "a")) (notp (varp "b"))))
(test(simplify-negations (notp (orp (notp (varp "a")) (varp "b")))) (andp (notp (notp (varp "a"))) (notp (varp "b"))))
(test(simplify-negations (orp (notp (notp (varp "a"))) (notp (varp "b")))) (orp (varp "a") (varp "b")))
(test(simplify-negations (notp (notp (notp (andp (notp (notp (varp "a"))) (notp (varp "b"))))))) (notp (andp (notp (notp (varp "a"))) (notp (varp "b")))))

;; tests parte 2b
(test (distribute-and (andp (orp (varp "a") (varp "b")) (varp "c"))) (orp (andp (varp "a") (varp "c")) (andp (varp "b") (varp "c"))))
(test (distribute-and (andp (varp "c") (orp (varp "a") (varp "b")))) (orp (andp (varp "c") (varp "a")) (andp (varp "c") (varp "b"))))
(test (distribute-and (notp (andp (orp (notp (varp "a")) (varp "b")) (andp (orp (varp "c") (andp (varp "d") (varp "e"))) (varp "f")))))
      (notp (orp (orp (andp (notp (varp "a")) (andp (varp "c") (varp "f")))
                      (andp (notp (varp "a")) (andp (andp (varp "d") (varp "e")) (varp "f"))))
                 (orp (andp (varp "b") (andp (varp "c") (varp "f")))
                      (andp (varp "b") (andp (andp (varp "d") (varp "e")) (varp "f")))))))


;; tests parte 2c
(test ((apply-until (lambda (x) (/ x (add1 x))) (lambda (x new-x) (<= (- x new-x) 0.1))) 1) 0.25)
(test ((apply-until (lambda (x) (* x (sub1 x))) (lambda (x new-x) (>= (+ x new-x) 300))) -1234) 1523990)
(test ((apply-until (lambda (x) (/ x (sub1 (* x x)))) (lambda (x new-x) (<= (+ x new-x) 0.00054321))) 12345)
      (/ -1881365951280 23225462363753551))

;; tests parte 2d
(test (DNF (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
      (orp (orp (andp (varp "a") (varp "c")) (andp (varp "a") (varp "d")))
           (orp (andp (varp "b") (varp "c")) (andp (varp "b") (varp "d")))))
(test (DNF (andp (andp (andp (varp "a") (notp (notp (varp "b")))) (orp (varp "c") (varp "d"))) (notp (notp (notp (varp "e"))))))
      (orp (andp (andp (andp (varp "a") (varp "b")) (varp "c")) (varp "e")) (andp (andp (andp (varp "a") (varp "b")) (varp "d")) (varp "e"))))
(test (DNF (varp "a")) (varp "a"))
;; tests parte 3a

;; tests parte 3b
;; occurrences-2
(test ((occurrences-2 (varp "a")) "b") 0)
(test ((occurrences-2 (andp (varp "a") (varp "b"))) "a") 1)
(test ((occurrences-2 (andp (varp "a") (varp "a"))) "a") 2)
(test ((occurrences-2 (orp (andp (varp "b") (varp "c")) (notp (varp "d")))) "a") 0)
(test ((occurrences-2 (andp (orp (notp (varp "b")) (varp "b")) (varp "b"))) "b") 3)

;; vars-2
(test (vars-2 (varp "a")) (list "a"))
(test (vars-2 (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars-2 (andp (varp "a") (varp "a"))) (list "a"))
(test (vars-2 (orp (andp (varp "b") (varp "c")) (notp (varp "d")))) (list "b" "c" "d"))
(test (vars-2 (andp (orp (notp (varp "b")) (varp "b")) (varp "b"))) (list "b"))

;; eval-2
(test ((eval-2 (varp "a")) (list (cons "a" #t))) #t)
(test ((eval-2 (varp "a")) (list (cons "a" #f))) #f)
(test ((eval-2 (varp "a")) (list (cons "a" #t) (cons "b" #f))) #t)
(test ((eval-2 (varp "a")) (list (cons "a" #f) (cons "b" #t) (cons "c" #t))) #f)
(test/exn ((eval-2 (varp "a")) '()) "eval: variable \"a\" is not defined in environment")
(test/exn ((eval-2 (varp "b")) '()) "eval: variable \"b\" is not defined in environment")

;; simplify-negations-2
(test (simplify-negations-2 (notp (notp (varp "a")))) (varp "a"))
(test (simplify-negations-2 (notp (andp (varp "a") (varp "b"))))
      (orp (varp "a") (varp "b")))
(test (simplify-negations-2 (notp (orp (varp "a") (varp "b"))))
      (andp (varp "a") (varp "b")))
(test (simplify-negations-2 (notp (orp (notp (varp "a")) (varp "b"))))
      (andp (notp (varp "a")) (varp "b")))
(test (simplify-negations-2 (orp (notp (notp (varp "a"))) (notp (varp "b")))) (orp (varp "a") (notp (varp "b"))))

;; distribute-and-2
(test (distribute-and-2 (andp (orp (varp "a") (varp "b")) (varp "c")))
      (orp (andp (varp "a") (varp "c")) (andp (varp "b") (varp "c"))))
(test (distribute-and-2 (andp (varp "c") (orp (varp "a") (varp "b"))))
      (orp (andp (varp "c") (varp "a")) (andp (varp "c") (varp "b"))))
(test (distribute-and-2 (andp (varp "a") (varp "b"))) (andp (varp "a") (varp "b")))
(test (distribute-and-2 (notp (andp (orp (notp (varp "a")) (varp "b")) 
                                     (andp (orp (varp "c") (andp (varp "d") (varp "e"))) (varp "f")))))
      (notp (orp (orp (andp (notp (varp "a")) (andp (varp "c") (varp "f")))
                      (andp (notp (varp "a")) (andp (andp (varp "d") (varp "e")) (varp "f"))))
                 (orp (andp (varp "b") (andp (varp "c") (varp "f")))
                      (andp (varp "b") (andp (andp (varp "d") (varp "e")) (varp "f")))))))