#lang play

;; Returns current time in milliseconds.
;; current-milliseconds :: () -> number

(define (f x)
  (sleep x))

(define (my-time e)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

;; Un "thunk" es una función que no toma argumentos... (λ () ...)

(define (my-time2 e-thunk)
  (let ([begin-time (current-milliseconds)])
    (begin
      (e-thunk)
      (- (current-milliseconds) begin-time))))


;; defmac simplifica la creación de macros
(defmac (my-time3 e)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))
  
;; (my-time3 (sleep 10))

(defmac (my-let-1 ([id e]) body)
  ((λ (id) body) e))

;; (my-let-1 ([x 5]) (* 3 x)) 

(defmac (my-let-n ([id e] ...) body)
  ((λ (id ...) body) e ...)) 

#|
(my-let-n ([x 1] [y 2] [z 3]) (+ x y z))
--> ((λ (x y z) (+ x y z)) 1 2 3)

(my-let-n () 1)
--> ((λ () 1))
|#

;; (my-let-n () 1)

(defmac (my-let-n2 ([id e] ...) body)
  (+ id ... e...))


(defmac (check c then e1 else e2)
  #:keywords then else
  (if c e1 e2))


;; Lógica de corto-circuito
;; Evaluar e1, si es #t retornar #t
;; Si no, retornar e2 (que puede ser #t o #f)
(defmac (my-or e1 e2)
  (let ([result e1])
    (if result
        result
        e2)))

;; my-and:: evalua e1 si es #f retorno #f inmediatamente
;; (defmac (my-and e1 e2) ...)

(define (my-or-f e1 e2)
  (let ([result e1])
    (if result
        result
        e2)))

(let ([result #t])
  (my-or #f result))













