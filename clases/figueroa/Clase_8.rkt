#lang play

(print-only-errors #t)

;; Definir la gramatica concreta

#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
           | (list <sym> <s-expr>)
|#

#|
<fundef> ::= (fundef <sym> <sym> <expr>) ;; <expr> no contiene otras definiciones
|#
(deftype FunDef
  (fundef name arg body))

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
         | (app <sym> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x)
  (app f-name f-arg))

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
    [(list 'with (list (? symbol? x) named-expr-expr) body-expr) ;; <-- '{with {x {+ 1 1}} {+ 1 x}}
     (with x (parse named-expr-expr) (parse body-expr))]
    [(list f-name f-arg) (app f-name (parse f-arg))]))  

(test (parse '1) (num 1))
(test (parse '{+ 2 1}) (add (num 2) (num 1)))
(test (parse '{- 2 1}) (sub (num 2) (num 1)))
(test (parse '{if0 {+ 1 1} 2 1}) (if0 (add (num 1) (num 1)) (num 2) (num 1)))
(test (parse 'x) (id 'x))
(test (parse '{with {x {+ 5 5}} {+ x x}}) (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))

;; lookup :: symbol listof(FunDef) -> FunDef
(define (lookup f-name defs)
  (match defs
    [(list) (error 'lookup "Function ~a not found" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                          head
                          (lookup f-name tail))]))


;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; interp :: Expr listof(FunDef) Env -> number
;; Evaluates an arithmetic expression
(define (interp expr f-list env)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l f-list env) (interp r f-list env))]
    [(sub l r) (- (interp l f-list env) (interp r f-list env))]
    [(if0 c t f)
     (if (zero? (interp c f-list env))
         (interp t f-list env)
         (interp f f-list env))]

    [(id x) (env-lookup x env)]
    
    [(with x named-expr body)
     #|(interp
      (subst body
             x
             (num (interp named-expr f-list)))
      f-list
      env)|#
     (def new-env (extend-env x (interp named-expr f-list env) env))
     (interp body f-list new-env)
    ]
    
    [(app f-name f-arg)
     #|
     (def (fundef _ the-arg the-body) (lookup f-name f-list))
     (interp (subst the-body the-arg (num (interp f-arg f-list))) f-list env)
     |#
     (def (fundef _ the-arg the-body) (lookup f-name f-list))
     ;;(def new-env (extend-env the-arg (interp f-arg f-list env) empty-env)) ;; <-- por qué se extiende un ambiente vacío? -- alcance estático
     (def new-env (extend-env the-arg (interp f-arg f-list env) env)) ;; <-- alcance dinámico
     (interp the-body f-list new-env)
    ]))

(define (run prog fundefs)
  (interp (parse prog) fundefs empty-env))

(test (run '1 '()) 1)
(test (run '{+ 2 1} '()) 3)
(test (run '{+ {if0 0 {+ {if0 1 1 2} 3} 9} 8} '()) 13)
(test (run '{- 2 1} '()) 1)
(test (run '{if0 {+ 1 1} 2 1} '()) 1)
(test (run '{with {x {+ 5 5}} {+ x x}} '()) 20)
(test (run
        '{with {x {+ 5 5}}
                     {with {y {- 3 4}}
                           {+ x y}}}
       '())
      9)
(test (run
        '{with {x {+ 5 5}}
            {with {y {- 3 x}}
                    {+ x y}}}
       '())
      3)

(test (run '{+ 1 {double 2}}
              (list (fundef 'double 'n (parse '{+ n n}))))
      5)

(run '{with {n 5} {f 10}}
     (list (fundef 'f 'x (parse 'n))))


