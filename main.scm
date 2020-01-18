(include "nn")

(define (main)
  (define l1 (make-layer 2 'tanh 1))
  (define m1 (model '(0.5 0.3) (list l1)))
  (print (feedforward (layers m1) (input m1)))
  (print 'done))
