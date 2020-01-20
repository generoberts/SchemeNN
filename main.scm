(import (r7rs))

(include "nn")

(define input (make-matrix '((0.5) (0.3))))
(define expected (make-matrix '((1) 1)))
(define l1 (make-layer 2 'tanh 1))
(define m1 (model (list l1)))
(define cost (make-cost-function 'mse))
(define (main)
  (print (feedforward m1 input))
  (print (calculate-error m1 input expected cost)))
