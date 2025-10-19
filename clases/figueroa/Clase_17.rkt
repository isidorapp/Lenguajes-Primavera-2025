#lang play

(print-only-errors #t)

;; Definir la gramatica concreta

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)            
           | (list <s-expr> <s-expr>)
|#

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f arg))

;; parse :: s-expr -> Expr
;; Parses source code to Expr AST.
(define (parse s-expr)
  (match s-expr
    ;; atoms
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    ;; s-exprs
    [(list '+ l-expr r-expr)
     (add (parse l-expr) (parse r-expr))]
    [(list '- l-expr r-expr)
     (sub (parse l-expr) (parse r-expr))]
    [(list 'if0 c-expr t-expr f-expr)
     (if0 (parse c-expr) (parse t-expr) (parse f-expr))]
    [(list 'with (list (? symbol? x) named-expr-expr) body-expr) ;; <-- syntactic sugar     
     ;; (with x (parse named-expr-expr) (parse body-expr))]
     (app (fun x (parse body-expr)) (parse named-expr-expr))]
    [(list 'fun (list x) b) (fun x (parse b))]    
    [(list f a) (app (parse f) (parse a))]))

(test (parse '1) (num 1))
(test (parse '{+ 2 1}) (add (num 2) (num 1)))
(test (parse '{- 2 1}) (sub (num 2) (num 1)))
(test (parse '{if0 {+ 1 1} 2 1}) (if0 (add (num 1) (num 1)) (num 2) (num 1)))
(test (parse 'x) (id 'x))
(test (parse '{with {x {+ 5 5}} {+ x x}}) ;;(with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))
      (app (fun 'x (add (id 'x) (id 'x))) (add (num 5) (num 5)))) 

;; <env> ::= mtEnv
;;         | (aEnv <sym> <loc> <env>)
(deftype Env
  (mtEnv)
  (aEnv id loc env))

(define empty-env (mtEnv))

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id loc rest) (if (symbol=? id x)
                            loc
                            (env-lookup x rest))]))

;; <sto> ::= mtSto
;;         | (aSto <loc> <value> <sto>)
(deftype Store
  (mtSto)
  (aSto loc val sto))

(define empty-sto (mtSto))

(define extend-sto aSto)

(define (lookup-sto l sto)
  (match sto
    [(mtSto) (error 'lookup-sto "No value at location: ~a" l)]
    [(aSto loc val rest)
     (if (equal? l loc)
         val
         (lookup-sto l rest))]))

;; next-location :: Store -> Loc
;; returns next free location of a store
(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (+ 1 (next-location rest))]))

;; Values of expressions
;; <value> ::= (numV <number>)
;;           | (closureV <sym> <expr> <env>)
(deftype Value
  (numV n)
  (closureV arg body env)
  (boxV loc))

(define (num+ n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV v1) n1)
  (def (numV v2) n2)
  (numV (- v1 v2)))

(define (num-zero? n)
  (def (numV v) n)
  (zero? v))


;; Pair of values and stores
;; <value*store> ::= (v*s <value> <store>)
(deftype Value*Store
  (v*s val sto))

;; interp :: Expr Env Store -> Value*Store
;; Evaluates an arithmetic expression
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))     
     (v*s (num+ l-val r-val) r-sto)]
    
    [(sub l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num- l-val r-val) r-sto)]
    
    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))     
     (if (num-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
    
    [(id x) (v*s (lookup-sto (env-lookup x env) sto) sto)]

    [(fun arg body) (v*s (closureV arg body env) sto)]
    
    [(app f e)
     (def (v*s (closureV the-arg the-body closed-env) fun-sto) (interp f env sto))
     (def (v*s arg-val arg-sto) (interp e env fun-sto))

     ;; Nueva ubicación de memoria para el valor del argumento
     (def new-loc (next-location arg-sto))
     (interp the-body
             (extend-env the-arg new-loc closed-env)
             (extend-sto new-loc arg-val arg-sto)) ;; <-- también retorna un (v*s v sto')
    ]))

(define (run prog)
  (def (v*s result sto) (interp (parse prog) empty-env empty-sto))
  (match result
    [(numV n) n]
    [(closureV arg body env) "<#procedure>"]))

(test (run '1) 1)
(test (run '{+ 2 1}) 3)
(test (run '{+ {if0 0 {+ {if0 1 1 2} 3} 9} 8}) 13)
(test (run '{- 2 1}) 1)
(test (run '{if0 {+ 1 1} 2 1}) 1)
(test (run '{with {x {+ 5 5}} {+ x x}}) 20)
(test (run
        '{with {x {+ 5 5}}
                     {with {y {- 3 4}}
                           {+ x y}}})
      9)
(test (run
        '{with {x {+ 5 5}}
            {with {y {- 3 x}}
                    {+ x y}}})
      3)

;;(test (run '{+ 1 {double 2}}) 5)
(test (run '{with {double {fun {x} {+ x x}}} {+ 1 {double 2}}}) 5)

(define p1 '{with {x 3}
                  {with {f {fun {y} {+ x y}}}
                        {with {x 5}
                              {+ x {f 4}}}}})

(test (run p1) 12)

(define p1-racket (let ([x 3])
    (let [(f (λ (y) (+ x y)))]
      (let ([x 5])
        (+ x (f 4))))))






             

