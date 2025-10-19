#lang play


#|
Nombre: Isidora Calderón Pérez
¿Utilizó Whiteboard Policy? (SI o NO): NO
En caso afirmativo, ¿con quién?:
¿en qué ejercicio(s)?:
|#


;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

#| Parte A |#

#|
<poly> ::= (poly (Listof <number>))
         | (id <id>)
         | (add <poly> <poly>)
         | (mul <poly> <poly>)
         | (if0 <poly> <poly> <poly>)
         | (with <id> <poly> <poly>)
|#

;; tipo de datos recursivo que genera los AST 
;; de los programas del lenguaje 
(deftype Poly
  (poly (list))
  (id x)
  (add l r)
  (mul l r)
  (if0 c t f)
  (with id p1 p2))

#| Parte B |#

#|
<s-poly> ::= <number>
           | <symbol>
           | (list <number> ...)
           | (list + <s-poly> <s-poly> ...)
           | (list * <s-poly> <s-poly> ...)
           | (list if0 <s-poly> <s-poly> <s-poly>)
           | (list with <symbol> <s-poly> <s-poly>)
|#

;; función auxiliar
;; cortar-ceros :: (Listof Number) -> (Listof Number)
;; dada una lista, quita todos los ceros a la derecha
;; y devuelve la nueva lista
(define (cortar-ceros l)
  (match l
    ['() '()]
    [else (let ([num (car (reverse l))])
            (if (zero? num)
                (cortar-ceros (reverse (cdr (reverse l))))
                l))]))

;; parse :: <s-poly> -> Poly
;; transforma el programa s-poly en sintaxis concreta
;; a su versión en sintaxis abstracta Poly
(define (parse s-poly)
  (match s-poly
    [(? number? n) (poly (list n))]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with x p1 p2) #:when (symbol? x) (with x (parse p1) (parse p2))]
    [(list n ...) (poly (cortar-ceros s-poly))]))
    

#| Parte C |#

(deftype Env
  (tyEnv)
  (xtEnv symbol value env))

(define empty-env (tyEnv))

;; extend-env :: symbol Poly Env -> Env
;; Extiende un ambiente añadiendo una variable.
(define (extend-env x v env)
  (xtEnv x v env))

;; env-lookup :: symbol Env -> (or #f Env)
;; Busca una variable en un ambiente, retornando el
;; valor encontrado o #f en caso de que no esté definida.
(define (env-lookup x env)
  (match env
    [(tyEnv) #f]
    [(xtEnv s v tail) (if (symbol=? x s)
                          v
                          (env-lookup x tail))]))


;; funciones auxiliares para suma de polinomios (listas)
;; sum-poly :: (Listof Number) (Listof Number) -> (Listof Number)
;; suma dos polinomios y devuelve el resultado
(define (sum-poly p1 p2)
  (match (list p1 p2)
    [(list '() '()) '()] ;;ambos vacíos
    [(list '() p2) p2] ;;p1 vacío
    [(list p1 '()) p1] ;;p2 vacío
    [(list (cons car1 cdr1) (cons car2 cdr2))
     (cons (+ car1 car2) (sum-poly cdr1 cdr2))]))
 

;; mul-sec :: (Listof Number) (Listof Number) -> (Listof Number)
;; multiplica punto a punto, dejando el grado correspondiente
(define (mul-sec p1 p2)
  (define (mul-init a b)
    (map (lambda (x) (* a x)) b))
  (match p1
    ['() '()]
    [(cons carp1 cdrp1)
       (sum-poly (mul-init carp1 p2)
                 (cons 0 (mul-sec cdrp1 p2)))]))


;; mul-poly :: (Listof Number) (Listof Number) -> (Listof Number)
;; multiplica dos polinomios y devuelve el resultado
(define (mul-poly p1 p2)
  (match (list p1 p2)
    [(list '() '()) '()] ;;ambos vacíos
    [(list '() p2) p2] ;;p1 vacío
    [(list p1 '()) p1] ;;p2 vacío
    [(list p1 p2) (mul-sec p1 p2)]))

  
;; reduce :: Poly Env -> Poly
;; reduce un polinomio hasta obtener el polinomio puro
(define (reduce p env)
  (match p
    [(poly n) (poly (cortar-ceros n))]
    [(id x)
     (let ([y (env-lookup x env)])
              (if y
                  y
                  (error 'reduce (format (string-append "variable " (symbol->string x) " is not defined")))))]
    [(add l r)
     (match (list (reduce l env) (reduce r env))
       [(list (poly a) (poly b))
        (poly (cortar-ceros (sum-poly a b)))])]
    [(mul l r)
     (match (list (reduce l env) (reduce r env))
       [(list (poly a) (poly b))
        (poly (cortar-ceros (mul-poly a b)))])]
    [(if0 c t f)
     (let ([x (reduce c env)])
       (match x
         [(poly n)
          (if (null? n)
           (reduce t env)
           (reduce f env))]))]
    [(with x p1 p2)
     (let ([rp1 (reduce p1 env)])
       (reduce p2 (extend-env x rp1 env)))]))


;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Parte A |#

#|
<expr> ::= (real <number>)
        | (imaginary <number>)
        | (idc <symbol>)
        | (addc <expr> <expr>)
        | (subc <expr> <expr>)
        | (if0c <expr> <expr> <expr>)
        | (withc (list (cons <symbol> <expr>) ...) <expr>)
|#
;; extensión tipo de datos con operadores números reales e imaginarios,
;; y with con múltiples definiciones
(deftype Expr
  (real n)
  (imaginary n)
  (idc x)
  (addc l r)
  (subc l r)
  (if0c c t f)
  (withc defs b))

#| Parte B |#

#|
Concrete syntax of expressions:

<s-expr> ::= <number>
           | (list <number> i)
           | <symbol>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list (cons <symbol> <s-expr>) ...) <s-expr>) 
|#

;; parser :: <s-expr> -> Expr
;; transforma una s-expresion en sintaxis concreta a una
;; Expresion en sintaxis abstracta
(define (parser s-expr)
  (match s-expr
    [(? number? n) (real n)]
    [(? symbol? x) (idc x)]   
    [(list '+ l r) (addc (parser l) (parser r))]
    [(list '- l r) (subc (parser l) (parser r))]
    [(list 'if0 c t f) (if0c (parser c) (parser t) (parser f))]
    [(list 'with defs body)
     (if (null? defs)
         (error 'parser "*with* expects at least one definition")
         (withc (map (lambda (def) 
                       (match def
                         [(list x e) (cons x (parser e))])) defs)
                (parser body)))]
    [(list n 'i) #:when (number? n)
                (imaginary n)]))
  

#| Parte C |#

#|
<cvalue> ::= (compV <num> <num>)
|#
(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; convierte un CValue a un elemento Expr
(define (from-CValue v)
  (match v
    [(compV r i) (if (zero? i)
                     (real r)
                     (addc (real r) (imaginary i)))]))

;; cmplx+ :: CValue CValue -> CValue
;; suma dos valores del lenguaje
(define (cmplx+ v1 v2)
  (match v1
    [(compV r1 i1)
     (match v2
       [(compV r2 i2) (compV (+ r1 r2) (+ i1 i2))])]))

;; cmplx- :: CValue CValue -> CValue
;; resta dos valores del lenguaje
(define (cmplx- v1 v2)
  (match v1
    [(compV r1 i1)
     (match v2
       [(compV r2 i2) (compV (- r1 r2) (- i1 i2))])]))

;; cmplx0? :: CValue -> Boolean
;; chequea si un número es 0
(define (cmplx0? v)
  (match v
    [(compV r i) (and (zero? r) (zero? i))]))

#| Parte D |#

;; funciones auxiliares para substitución sobre
;; listas de definiciones

;; subst-defs :: (Listof (Cons Symbol Expr)) Symbol Expr -> (Listof (Cons Symbol Expr))
;; substituye en las definiciones, se detiene cuando encuentra la variable
(define (subst-defs defs what for)
  (match defs
    ['() '()]
    [(cons (cons x e) rest)
     (if (eq? x what)
         (cons (cons x e) rest)
         (cons (cons x (subst e what for))
               (subst-defs rest what for)))]))

;; shadowing :: Symbol (Listof (Cons Symbol Expr)) -> Boolean
;; verifica si una variable está definida en
;; una lista de definiciones dada
(define (shadowing var defs)
  (match defs
    ['() #f]
    [(cons (cons x _) rest)
     (or (eq? x var)
         (shadowing var rest))]))

;; subst :: Expr Symbol Expr -> Expr
;; realiza la substitución de una expresión por un identificador
(define (subst in what for)
  (match in
    [(real n) in]
    [(imaginary n) in]
    [(idc x) (if (eq? x what) for (idc x))]
    [(addc l r) (addc (subst l what for) (subst r what for))]
    [(subc l r) (subc (subst l what for) (subst r what for))]
    [(if0c c t f) (if0c (subst c what for) (subst t what for) (subst f what for))]
    [(withc defs body)
     (withc (subst-defs defs what for)
            (if (shadowing what defs)
                body
                (subst body what for)))]))

#| Parte E |#

;; interp :: Expr -> CValue
;; reduce una expresión Expr a un valor del lenguaje CValue
(define (interp expr)
  (match expr
    [(real n) (compV n 0)]
    [(imaginary n) (compV 0 n)]
    [(idc x) (error 'interp (format "Free occurrence of variable ~a" x))]
    [(addc l r) (cmplx+ (interp l) (interp r))]
    [(subc l r) (cmplx- (interp l) (interp r))]
    [(if0c c t f) (if (cmplx0? (interp c))
                      (interp t)
                      (interp f))]
    [(withc defs body)
     (match defs
       ['() (interp body)]
       [(cons (cons x e) rest)
        (let ([value (interp e)])
          (interp (withc rest (subst body x (from-CValue value)))))])]))
