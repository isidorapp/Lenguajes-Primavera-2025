#lang play

(define ones
  (let ([ones (mcons 1 'dummy)]) ;; Paso 1: asociar la estructura con un identificador y usar un valor provisorio en el lugar de la auto-referencia
    (begin
      (set-mcdr! ones ones) ;; Paso 2: mutar el valor provisorio, seteandolo con el identificador de la estructura cíclica
      ones)))

(define pm1 (mcons 1 2))

(define p1 (cons 1 2))

(define ones* (mcons 1 'dummy))

(define autocaja (let ([b (box 'dummy)]) ;; Paso 1: identificador + placeholder
  (begin
    (set-box! b b) ;; Paso 2: mutar placeholder con identificador auto-referencia
    b)))

;; Ejercicio: lista cíclica para 1.45454545...45

(define m145 (let ([b (mcons 4 (mcons 5 'dummy))]) ;; Paso 1: identificador + placeholder
  (begin
    (set-mcdr! b (mcons 5 b)) ;; Paso 2:
    ;; b ;; <--- solo es el ciclo 454545...45
    (mcons 1 b)))) ;; <--- lista que parte con 1 y luego tiene el ciclo

;; Recursive functions in Racket via mutation

(let ([fact (box 'dummy)])
  (let ([fact-fun
         (lambda (n) (if (zero? n)
                         1
                         (* n ((unbox fact) (- n 1)))))])
    (begin
      (set-box! fact fact-fun)
      ((unbox fact) 6))))

(let ([fact (box 'dummy)])
  (begin
    (set-box! fact (lambda (n) (if (zero? n)
                                   1
                                   (* n ((unbox fact) (- n 1))))))
    ((unbox fact) 6)))
    












