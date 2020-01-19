(import (scheme inexact))

;; https://en.wikipedia.org/wiki/Activation_function
;; when apply acitvation function, we have to check that we didn't apply
;; a function that we don't implement here
(define-record-type activation-function
  (activation-function type function derivative)
  activation-function?
  (type type) ; basically a name. once define, activation function can't change its type
  (function function)
  (derivative derivative)) ; all derivative is named as function type but with prefix d

;; usage: (make-activation-function 'softplus)
(define (make-activation-function name)
  (let ((func-deri (case name
                     ((identity) (cons identity didentity))
                     ((binary-step) (cons binary-step dbinary-step))
                     ((logistic) (cons logistic dlogistic))
                     ((tanh) (cons tanh dtanh))
                     ((relu) (cons relu drelu))
                     ((leaky-relu) (cons leaky-relu dleaky-relu))
                     ((softplus) (cons softplus dsoftplus))
                     (else (error "Unknow activation function")))))
    (activation-function name (car func-deri) (cdr func-deri))))

;; take a vector (or a list) as input
(define (softmax vect)
  (let ((summation (apply + (map exp vect))))
    (map (lambda (val) (/ (exp val) summation)) vect)))

;; todo
;; (define (dsoftmax vect))

;; also take a vector (or a list) as input
(define (maxout vect)
  (apply max vect))
;; todo
;; (defnie (dmaxout vect))

;; all other functions take a number as argument

;; identity function is defined in (scheme base)
(define (didentity x)
  1)

(define (binary-step x)
  (if (>= x 0)
      1
      0))
(define (dbinary-step x)
  (if (= x 0)
      1 ; here we hardcode it, since wikipedia doesn't specify the value
      0))

(define (logistic x)
  (/ 1 (+ 1 (exp (- x)))))
(define (dlogistic x)
  (* (logistic x) (- 1 (logistic x))))

(define (tanh x)
  (define (e^x+-e^-x arg plus?)
    ((if plus? + -) (exp arg) (exp (- x))))
  (/ (e^x+-e^-x x #f)
     (e^x+-e^-x x #t)))
(define (dtanh x)
  (- 1 (expt (tanh x) 2)))

(define (relu x)
  (if (> x 0)
      x
      0))
(define (drelu x)
  (if (> x 0)
      1
      0))

(define (leaky-relu x)
  (if (>= x 0)
      x
      (* 0.01 x)))
(define (dleaky-relu x)
  (if (>= x 0)
      1
      0.01))

(define (softplus x)
  (log (+ 1 (exp x))))
(define (dsoftplus x)
  (/ 1 (+ 1 (exp (- x)))))

