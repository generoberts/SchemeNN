(import (r7rs))

(include "nn")

(define input (make-matrix '((0.5) (0.3))))
(define l1 (make-layer 2 'tanh 1))
(define m1 (model (list l1)))
(define (main)
  (print (feedforward m1 input))
  (print 'done))
