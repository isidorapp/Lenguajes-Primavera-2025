#lang play

#| sintaxis abstracta
<expr> :: = (num <num>)
          | (add <expr> <expr>)
          | (sub <expr> <expr>)
          | (if0 <expr> <expr> <expr>)
          | (id <sym>)
          | (fun <sym> <expr>)
          | (app <expr> <expr>)
|#
;; definición inductiva de expresiones aritméticas
;; con funciones de primera clase
(deftype Expr
  (num n)
  (add left right)
  (sub left right)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f arg))

#| sintaxis concreta
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
|#
;; parse :: s-expr -> Expr
;; convierte s-expressions a Exprs
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ left right) (add (parse left) (parse right))]
    [(list '- left right) (sub (parse left) (parse right))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list (? symbol? x) e) b) ;;las with expressions se traducen como
     (app (fun x (parse b)) (parse e))] ;;apps de funciones internas
    [(list f a) (app (parse f) (parse a))]))

#| semantica
(fun arg body)
(app f-name f-arg)
La interpretación de la aplicación de la función:
       
       (apply-twice)                  (add1)
(( (fun (f) (fun (z) (f (f z)))) (fun (x) (+ x 1)) ) (+ 10 5)) in env []

1. Reducir la expresión en la posición de función
((fun (z) ((fun (x)(+ x 1)) (fun (x) (+ x 1)) ) (+ 10 5)) in env []

2. Reducir el argumento


Ej.
La interpretación de la aplicación de la función:
eval (app e1 e2) env
- (eval e1 env) -> v1
- (eval e2 env) -> v2
- (eval (body v1) (env;[(arg v1) -> v2]))

Ej.
(with (x 3)
      (with (f (fun (y) (+ x y)))
            (with (x 5)
                  (+ x (f 4)))))

(+ 5 (+ 5 4)) -> scope dinámico
Las variables libres en el body de f son ligadas
siguiendo el environment en la ubicación
de llamada

(+ 5 (+ 3 4)) -> scope estático
Las variables libres en el body de f son ligadas
siguiendo el environment en la ubicación
de definición de f

Para mantener scope estático, cuando se encuentra una
definición de función, debemos wrappearla con el
environment en ese punto durante la interpretación
Esto guarda las sustituciones pendientes correctamente

Closure: esturctura que une una definición de función
y su environment en el momento de definición


function definition + environment at the def location =
closure of the function

|#

;; Values of expressions
;; <value> ::= (numV <number>)
;;           | (closureV <sym> <expr> <env>)
(deftype Value
  (numV n)
  (closureV id body env))

;; eval :: Expr Env -> Value
;; evalúa una expresión en un
(define (num+ n1 n2)
  (def (numV vq) n1)
  (def (numV v2) n2)
  (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV vq) n1)
  (def (numV v2) n2)
  (numV (- v1 v2)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))

(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add left right) (num+ (eval left env) (eval right env))]
    [(sub left right) (num- (eval left env) (eval right env))]
    [(if0 c t f) (if (num-zero? (eval c env)
                                (eval t env)
                                (eval f env)))]
    [(app f e) (def (closureV the-arg the-body the-claus.ev) (eval f env))
               (def  the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))

;;El cambio de the-claus-env por env es el cambio a scope dinámico
