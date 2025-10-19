#lang play

;; decisiones de representación

#|
tipos de intérpretes
- sintáctico: implementa todo el comportamiento
explícitamente
- meta intérptrete: usa características del
languaje huésped
- meta-circular: lenguaje interpretado = lenguaje huésped
|#

;; representación funcional de ambientes

;; ambiente como función
;; representación de ambientes como funciones
;; en lugar de estructuras de datos
(define (empty-env-fn)
  (λ (id) (error "free identifier: ~a" id)))

(define (env-lookup-fn id env)
  (env id))

(define (extend-env-fn new-id value env)
  (λ (id)
    (if (symbol=? id new-id)
        value
        (env id))))

;; Meta-interprete usando funciones de Racket para closures

(deftype Value
  (numV n)
  (closureV f))

#|
(define (interp-meta expr env)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp-meta l env) (interp-meta r env))]
    [(sub l r) (- (interp-meta l env) (interp-meta r env))]
    [(id x) (env x)]
    [(fun arg body) (closureV (λ (arg-val) 
                               (interp-meta body (extend-env-fn arg arg-val env))))]
    [(app f e) (match (interp-meta f env)
                 [(closureV fun-val) (fun-val (interp-meta e env))])]))
|#