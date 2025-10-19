#lang play
(print-only-errors #t)

#|
¿Utilizó Ud. la política de Whiteboard Policy para la resolución de la taea (complete con SI o NO):
En caso afirmativo, indique con quién y sobre qué ejercicio(s): NO
|#


#| P1 |#

#| Parte A |#

;; Prop define una proposición (fórmula booleana),
;; que puede ser de forma string (variable simple), y lógico,
;; o lógico, (ambas con dos proposiciones) o bien
;; negación de proposiciones (negación de variable simple)

#|
<Prop> ::= (varp <string>)
         | (andp <Prop> <Prop>)
         | (orp <Prop> <Prop>)
         | (notp <Prop>)
|#

(deftype Prop
  (varp (string))
  (andp p q)
  (orp p q)
  (notp p))

  
#| Parte B |#

;; occurrences :: Prop String -> Number
;; cuenta la cantidad de ocurrencias de una variable en una proposición
(define (occurrences p x)
  (match p
    [(varp n)
     (if (equal? n x)
         1
         0)]
    [(andp p q)
     (+ (occurrences p x)
        (occurrences q x))]
    [(orp p q)
     (+ (occurrences p x)
        (occurrences q x))]
    [(notp p)
     (occurrences p x)]))


#| Parte C |#

;; vars :: Prop -> (Listof String)
;; devuelve una lista (sin duplicados) de todos los nombres de las variables de la proposición
(define (vars p)
  (match p
    [(varp n)
     (list n)]
    [(andp p q)
     (remove-duplicates (append (vars p) (vars q)))]
    [(orp p q)
     (remove-duplicates (append (vars p) (vars q)))]
    [(notp p)
     (remove-duplicates (append (vars p)))]
    [_ '()])) ;; caso Props sin variables



#| Parte D |#

;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; recibe una lista de variables sin duplicados y crea todas los posibles
;; ambientes de evaluación para los valores de verdad de las variables
(define (all-environments vars)
  (match vars
    ['() (list '())]
    [else (let* ((var (car vars))
                 (rest (all-environments (cdr vars))))
            (append
             (map (lambda (x) (cons (cons var #t) x)) rest)
             (map (lambda (x) (cons (cons var #f) x)) rest)))]))


#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean
;; evalúa una proposición p en un ambiente env y obtiene los valores de verdad
;; de cada variable, devolviendo el valor de verdad de la fórmula
(define (eval p env)
  (match p
    [(varp n)
     (let ((x (assoc n env)))
       (if x                  
           (cdr x)
           (error (format "eval: variable ~v is not defined in environment" n))))]
    [(andp p q)
     (and (eval p env) (eval q env))]
    [(orp p q)
     (or (eval p env) (eval q env))]
    [(notp p)
     (not (eval p env))]))


#| Parte F |#
;; función auxiliar
;; check :: Prop (Listof (Listof (Pair String Boolean))) -> Boolean
;; evalúa una proposición para definir si es tautología
;; (siempre verdadero) en base a cada uno de sus ambientes de evaluación
(define (check p envs)
  (cond
    [(null? envs) #t]
    [(null? p) #t]
    [(eval p (car envs)) (check p (cdr envs))]
    [else #f]))

;; tautology? :: Prop -> Boolean
;; define si una proposición p es tautología (siempre verdadera para
;; cualquier ambiente de evaluación)
(define (tautology? p)
   (check p (all-environments(vars p))))
         

#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop
;; recorre la proposición p una sola vez y simplifica las
;; doble negaciones y leyes de de morgan
(define (simplify-negations p)
  (match p
    [(varp n) p]
    [(andp a b) (andp (simplify-negations a) (simplify-negations b))]
    [(orp a b) (orp (simplify-negations a) (simplify-negations b))]
    [(notp p)
     (match p
       [(varp n) p]
       [(andp a b) (orp (notp a) (notp b))]
       [(orp a b) (andp (notp a) (notp b))]
       [(notp a) a])]))



#| Parte B |#

;; distribute-and :: Prop -> Prop
;; recorre la proposición p a modo de árbol una sola vez y distribuye
;; las conjunciones and si corresponde
(define (distribute-and p)
  (match p
    [(varp n) p]
    [(orp a b)
     (orp (distribute-and a) (distribute-and b))]
    [(notp a)
     (notp (distribute-and a))]
    [(andp a b)
     (let ([dist-a (distribute-and a)]
           [dist-b (distribute-and b)])
       (match (list dist-a dist-b)
         [(list (orp a1 a2) b)
          (orp (distribute-and (andp a1 b))
               (distribute-and (andp a2 b)))]
         [(list a (orp b1 b2))
          (orp (distribute-and (andp a b1))
               (distribute-and (andp a b2)))]
         [else (andp dist-a dist-b)]))]))


#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a
;; tomando una función y un predicado, aplica la función, y vuelve a aplicar
;; la función hasta que el predicado sea verdadero, devolviendo el primer valor que la vuelve verdadera
(define (apply-until f p)
  (define (f-x x)
    (let ((next-x (f x)))
      (if (p x next-x)
          next-x
          (f-x next-x))))
  (lambda (x) (f-x x)))

 

#| Parte D |#

;; DNF :: Prop -> Prop
;; aplica las transformaciones de simplificar negaciones y distribución de conjunciones
;; tantas veces sea necesario hasta llegar a la forma normal disyuntiva
(define (DNF p)
  (define simplified
    ((apply-until simplify-negations (lambda (x y) (equal? x y))) p))
  ((apply-until distribute-and (lambda (x y) (equal? x y))) simplified))

(test (DNF (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
      (orp (orp (andp (varp "a") (varp "c")) (andp (varp "a") (varp "d")))
           (orp (andp (varp "b") (varp "c")) (andp (varp "b") (varp "d")))))


#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a
;; captura el esquema de recursión asociado a Prop mediante fold
(define (fold-prop v a o n)
  (lambda (p)
    (match p
      [(varp n) (v n)]
      [(andp p q) (a ((fold-prop v a o n) p)
                     ((fold-prop v a o n) q))]
      [(orp p q) (o ((fold-prop v a o n) p)
                     ((fold-prop v a o n) q))]
      [(notp p) (n ((fold-prop v a o n) p))])))

#| Parte B |#

;; occurrences-2 :: Prop String -> Number
;; cuenta todas las ocurrencias de una variable en una proposición
(define occurrences-2
  (fold-prop (lambda (n) (lambda (x)
                           (if (equal? n x)
                               1
                               0)))
             (lambda (p q) (lambda (x)
                             (+ (p x) (q x))))
             (lambda (p q) (lambda (x)
                             (+ (p x) (q x))))
             (lambda (p) (lambda (x)
                           (p x)))))


;; vars-2 :: Prop -> (Listof String)
;; devuelve una lista (sin duplicados) de todos los nombres de variables de la proposición
(define vars-2
  (fold-prop (lambda (n) (list n))
             (lambda (p q) (remove-duplicates (append p q)))
             (lambda (p q) (remove-duplicates (append p q)))
             (lambda (p) (remove-duplicates (append p)))))

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean
;; evalúa una proposición p en un ambiente env y obtiene los valores de verdad
;; de cada variable, devolviendo el valor de verdad de la fórmula
(define eval-2
  (fold-prop (lambda (n) (lambda (env)
                           (let ((x (assoc n env)))
                             (if x
                                 (cdr x)
                                 (error (format "eval: variable ~v is not defined in environment" n))))))
             (lambda (p q) (lambda (env) (and (p env) (q env))))
             (lambda (p q) (lambda (env) (or (p env) (q env))))
             (lambda (p) (lambda (env) (not (p env))))))

;; simplify-negations-2 :: Prop -> Prop
;; recorre la proposición p y simplifica las
;; doble negaciones y leyes de de morgan
(define simplify-negations-2
  (fold-prop (lambda (n) (varp n))
             (lambda (p q) (andp p q))
             (lambda (p q) (orp p q))
             (lambda (p)
               (match p
                 [(varp n) (notp (varp n))]
                 [(andp p q) (orp p q)]
                 [(orp p q) (andp p q)]
                 [(notp p) p]))))

;; distribute-and-2 :: Prop -> Prop
;; recorre la proposición p a modo de árbol una sola vez y distribuye
;; las conjunciones "and" si corresponde
(define distribute-and-2
  (fold-prop (lambda (n) (varp n))
             (lambda (p q)
               (match (list p q)
                 [(list (orp a1 a2) b)
                  (orp (distribute-and (andp a1 b))
                       (distribute-and (andp a2 b)))]
                 [(list a (orp b1 b2))
                  (orp (distribute-and (andp a b1))
                       (distribute-and (andp a b2)))]
                 [else (andp p q)]))
             (lambda (p q) (orp p q))
             (lambda (p) (notp p))))