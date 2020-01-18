(include "nn")

(define (main)
  (define l1 (make-layer 2 'tanh 1))
  (define m1 (model '(0.5 0.3) (list l1)))
  (print (feedforward m1))
  (print 'done))
