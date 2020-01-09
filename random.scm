;;; Library for random number generator

(import (chicken random))

;; random an integer 0 to (- n 1)
(define (random n)
  (pseudo-random-integer n))


;; uniformly distributed random inexact number between 0 and 1
(define (rrandom)
  (pseudo-random-real))
