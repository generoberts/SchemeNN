(include "nn")

(define l1 (make-layer 2 'tanh 1))
(define m1 (model (make-matrix '((0.5) (0.3))) (list l1)))
(define (main)
  (print (feedforward m1))
  (print 'done))
