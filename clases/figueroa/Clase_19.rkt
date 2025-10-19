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
           | (list 'refun (list <sym>) <s-expr>)
           | (list 'newbox <s-expr>)
           | (list 'openbox <s-expr>)
           | (list 'setbox <s-expr> <s-expr>)
           | (list 'seqn <s-expr> <s-expr>)
           | (list 'set <sym> <s-expr>)
           | (list <s-expr> <s-expr>)
|#

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)         
         | (id <sym>)
         | (fun <sym> <expr>)
         | (refun <sym> <expr>)
         | (app <expr> <expr>)
         | (openbox <expr>)
         | (newbox <expr> <expr>)
         | (setbox <expr> <expr>)
         | (seqn <expr> <expr>)
         | (set <sym> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (refun arg body)
  (app f arg)
  (newbox v)
  (openbox b)
  (setbox b v)
  (seqn e1 e2)
  (set id val-expr))

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
     (app (fun x (parse body-expr)) (parse named-expr-expr))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list 'refun (list x) b) (refun x (parse b))]
    [(list 'newbox expr) (newbox (parse expr))]
    [(list 'openbox expr) (openbox (parse expr))]
    [(list 'setbox box-expr val-expr) (setbox (parse box-expr) (parse val-expr))]
    [(list 'seqn expr1 expr2) (seqn (parse expr1) (parse expr2))]
    [(list 'set x val-expr) (set x (parse val-expr))]
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
  (refclosureV arg body env)
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

    [(refun arg body) (v*s (refclosureV arg body env) sto)]
    
    [(app f e)
     (def (v*s fun-val fun-sto) (interp f env sto)) ;; fun-val puede ser closureV o refclosureV
     (match fun-val
       [(closureV the-arg the-body closed-env)
        (def (v*s arg-val arg-sto) (interp e env fun-sto)) ;; <-- e puede ser {+ 1 2} o un id. 
        ;; Nueva ubicación de memoria para el valor del argumento
        (def new-loc (next-location arg-sto))
        (interp the-body
                (extend-env the-arg new-loc closed-env)
                (extend-sto new-loc arg-val arg-sto))
        ]

       [(refclosureV the-arg the-body closed-env) ;; se asume/exige que "e" sea un identificador/variable        
        ;; (def loc (env-lookup e env)) ;; <-- no funciona porque e = (id 'v)
        ;; (def (v*s arg-val arg-sto) (interp e env fun-sto)) <-- NO se debe interpretar "e".
        (def loc (env-lookup (id-x e) env))
        (interp the-body
                (extend-env the-arg loc closed-env)
                fun-sto)
       ])]

    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto))
     (interp expr2 env sto1)]

    [(newbox val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def new-loc (next-location val-sto))
     (v*s (boxV new-loc)
          (extend-sto new-loc val-val val-sto))]

    [(openbox box-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (v*s (lookup-sto loc box-sto) box-sto)]

    [(setbox box-expr val-expr)
     ;; Decidir: izquierda-a-derecha o derecha-a-izquierda
     ;; Vamos a usar izquierda-a-derecha
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (def (v*s val-val val-sto) (interp val-expr env box-sto))

     ;; (v*s ? (extend-sto ? ? val-sto))]
     (v*s val-val (extend-sto loc val-val val-sto))]

    [(set id val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def loc (env-lookup id env))
     (v*s val-val
          (extend-sto loc val-val val-sto))]
    ))

(define (run prog [print-sto #f])
  (def (v*s result sto) (interp (parse prog) empty-env empty-sto))
  (begin
    (when print-sto (print sto) (newline))
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


(define m1 '{with {b {newbox 0}}
                  {seqn {setbox b {+ 1 {openbox b}}}
                        {openbox b}}})

(test (run m1) 1)

(define m2 '{with {a {newbox 1}}
                  {with {f {fun {x} {+ x {openbox a}}}}
                        {seqn {setbox a 2} {f 5}}}})

(test (run m2) 7)

(define m3 '{with {b {newbox 0}}
                  {if0 {seqn {setbox b 5} {openbox b}}
                       1
                       {openbox b}}})

(test (run m3) 5)

(define m4 '{with {b {newbox 4}}
                  {+ {with {dummy {setbox b 5}}
                           {openbox b}}
                     {openbox b}}})

(test (run m4) 10)

(define m5 '{with {switch {newbox 0}}
                  {with {toggle
                         {fun {dummy}
                              {seqn {setbox switch {- 1 {openbox switch}}}
                                    {openbox switch}}}}
                        {+ {toggle 1} {toggle 1}}}})

(test (run m5) 1)


(define v1 '{with {b 0}
                  {seqn {set b {+ 1 b}}
                        b}})

(define v2 '{with {b 0}
                  {if0 {seqn {set b 5} b}
                       1
                       b}})

(test (run v1) 1)

(test (run v2) 5)

(define v3 '{with {v 0}
                  {with {f {fun {y} {set y 5}}}
                        {seqn {f v} v}}})

(test (run v3) 0)

(define v4 '{with {v 0}
                  {with {f {refun {y} {set y 5}}}
                        {seqn {f v} v}}})

(test (run v4) 5)

; (define v5 '{with {v 0}
;                  {with {f {refun {y} {set y 5}}}
;                        {seqn {f 5} v}}}) ;; <-- no tiene sentido (f 5)













                   
             

