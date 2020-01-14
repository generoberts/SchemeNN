(import (scheme inexact))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todo: derivatives ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://en.wikipedia.org/wiki/Activation_function
;; when apply acitvation function, we have to check that we didn't apply
;; a function that we don't implement here
(define defined-functions '(sigmoid logistic softmax identity))

;; take a vector (or a list) as input
(define (softmax vect)
  (let ((summation (apply + (map exp vect))))
    (map (lambda (val) (/ (exp val) summation)) vect)))

;; also take a vector (or a list) as input
(define (maxout vect)
  (apply max vect))

;; all other functions take a number as argument

;; identity function is defined in (scheme base)

(define (binary-step x)
  (if (>= x 0)
      1
      0))

(define (logistic x)
  (/ 1 (+ 1 (exp (- x)))))

(define (tanh x)
  (define (e^x+-e^-x arg plus?)
    ((if plus? + -) (exp arg) (exp (- x))))
  (/ (e^x+-e^-x x #f)
     (e^x+-e^-x x #t)))

(define (relu x)
  (if (> x 0)
      x
      0))

(define (leaky-relu x)
  (if (>= x 0)
      x
      (* 0.01 x)))

(define (softplus x)
  (log (+ 1 (exp x))))

