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
  (with x named-expr body)
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

;; Values of expressions
;; <value> ::= (numV <number>)
;;           | (closureV <sym> <expr> <env>)
(deftype Value
  (numV n)
  (closureV arg body env))

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

;; interp :: Expr Env -> number
;; Evaluates an arithmetic expression
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(sub l r) (num- (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    ;; [(fun arg body) (fun arg body)] ;; por ahora

    [(fun arg body) (closureV arg body env)]
    
    [(app f e)
     ;; (def (fun arg body) (interp f env))
     ;; (def new-env (extend-env arg (interp e env) empty-env)) ;; extender env vacio -> perdida de scope
     ;; (def new-env (extend-env arg (interp e env) env)) ;; extender env actual -> scope dinamico     

     (def (closureV the-arg the-body closed-env) (interp f env))
     (def new-env (extend-env the-arg (interp e env) closed-env)) ;; <-- se mantiene alcance estático    
    
     (interp the-body new-env)]
    ))

(define (run prog)
  (let ([result (interp (parse prog) empty-env)])
    (match result
      [(numV n) n]
      [(closureV arg body env) "<#procedure>"])))

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






             

