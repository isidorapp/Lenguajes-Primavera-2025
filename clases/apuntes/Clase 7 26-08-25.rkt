#lang play

#|
<fundef> ::= (fundef <sym> <sym> <expr>)
|#
(deftype FunDef
  (fundef name arg body))

;; lookup :: symbol listof(FunDef) -> FunDef
;; busca una definición de función dentro de una
;; lista de definiciones
(define (lookup f-name defs)
  (match defs
    [(list) (error 'lookup "Function ~a not found" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                           head
                           (lookup f-name tail))]))

;; aplicar sustitución a los nodos de aplicación de función
;; significa propagar la sustitución hacia la expresión del argumento

;; subst :: Expr Symbol Expr -> Expr
;; (subst in what for) substitutes all free occurrences
;; of identifier 'what' in expression 'in' for expression 'for'
#|
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add left right) (add (subst left what for) (subst right what for))]
    [(sub left right) (sub (subst left what for) (subst right what for))]
    [(if0 c t f) (if0 (subst c what for) (subst t what for) (subst f what for))]
    [(with x e b) (with x (subst e what for) (if (eq? x what) b (subst b what for)))]
    [(id x) (if (eq? x what) for (id x))]
    [(app f-name f-arg) (app f-name (subst f-arg what for))]))


;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (interp (parse prog) f-list))

(define my-funcs
  (list (fundef 'double 'x (parse '(+ x x)))
        (fundef 'add1 'n (parse '(+ n 1)))))

(test (run '(+ 1 (double (add1 4))) my-funcs) 11)
|#

;; EVALUATION BY DEFERRED SUBSTITUTION
;; Evaluación por sustitución: las definiciones locales
;; y llamadas a funciones se manejan con sustitución
#|
[(with x e b) (interp (subst b x (num (interp e f-list))) f-list)]
1. Reducir named expression e recursivamente
2. Sustituir en body b el identificador x con el valor de la
named expression e
3. Reducir la expresión obtenida

[(app f-name f-arg)
 (def (fundef the-arg the-body) (lookup f-name f-list))
 (interp (subst the-body the-arg (num (interp f-arg f-list))) list)]
1. Obtener el nombre de argumento the-arg y el body the-body
desde la definición de función
2. Reducir el argumento f-arg recursivamente
3. Sustituir el argumento formal en el body de la función,
con el valor computado
4. Reducir la expresión obtenida

Ej. Para reducir la expresión
(with (x 3) (with (y 4) (with (z 5) (+ x (+ y z)))))

El intérprete hace las sgtes sustituciones
(subst (with (y 4) (with (z 5) (+ x (+ y z)))) x 3)
(subst (with (z 5) (+ x (+ y z))) y 4)
(subst (+ 3 (+ 4 z)) z 5)

Se puede mejorar con deferred substitution en vez de
haciéndolas justo después de encontrarse con una definición
local (o aplicación de función)
Se hace un repositorio de deferred substitutions durante
la evaluación:
1. Comenzar con repositorio vacío
2. Cuando se encuentra una sustitución (originada por una
definición local o aplicación de función) en vez de hacerla,
se añade una entrada al repositorio asociando el identificador
con su valor, y se continúa la evaluación
3. Si se da un identifier, se obtiene su valor del repositorio

;; Adaptación del intérprete:
;; interp :: Expr listof(FunDef) Env -> number
;; evalúa una expresión aritmética con llamadas
;; a funciones y definiciones locales, difiriendo
;; las sustituciones
(define (interp expr f-list env)
  (match expr
    [(with x e b) (def new-env (extend-env x (interp e f-list env) env))
                  (interp b f-list new-env)]))

Se evalúa el body de la definición local en el environment
aumentado (se mapea el identificador con el valor de la
named expression)

Ambos, el intérprete con deferred substitution y el intérprete
original con sustitución explícita, retornan el mismo output
Cuando se consulta el repositorio para un identificador con
bindings múltiples, debe retornar el añadido más recientemente
(with (x 0) (with (x 1) x)) -> 1

|#
#|
;; SCOPING
La expresión
(with (n 5) (f 10))
con la definición de función
(list (fundef 'f 'x (id 'n))
Se reduce a:
5 en dynamic scope
Error (n is free) en static (or lexical) scope

Las reglas de scoping determinan cómo los identificadores
obtienen sus valores

;; Static scope
En un lenguaje con scope estático, el scope de un ligando
es una región delimitada sintácticamente

(with (n 5) (f 10)) con definición (list (fundef 'f 'x (id 'n))
scope del ligando n => 5
el ligando n=>5 solo tiene efecto en el "texto" (f 10)
como no hay ocurrencia de n en (f 10) el ligando
no tiene efecto

;; Dynamic scope
En un lenguaje con scope dinámico, el scope de un ligando
es todo lo que queda de la ejecución durante la cual
el ligando está haciendo efecto

(with (n 5) (f 10)) con definición (list (fundef 'f 'x (id 'n))

el ligando n=>5 aún tiene efecto durante la ejecución de f
y durante la ejecución de cualquier otra función que
f invoque


;; Static vs Dynamic
Estático:
- Basado en la estructura del código y cómo está escrita
- El scope se determina cuando se escribe el código
- Predecible, se lee el código fuente
- Default adecuado para lenguajes modernos
- Generalmente presenta mejores herramientas de apoyo y escalabilidad

Dinámico:
- Basado en el historial de llamadas a funciones durante la
ejecución del programa
- El scope se determina en runtime, basado en la pila de llamadas
- Difícil de predecir, resultado bugs raros
- Útil como feature opcional y explícita
- Puede usarse para pasar parámetros arbitrarios si
cambiar signatures de función

|#




    
