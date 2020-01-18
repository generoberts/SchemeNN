;;; cost functions

(define-record-type cost-function
  cost-function?
  (type type) ; basically a name
  (cost-func cost-func)
  (gradient gradient)) ; gradient of this cost function, its name is the type of cost function with prefix 'g'

;; result is what feedforward gives us
;; expeted is what the training data is
(define (mse result expected)
  (if (and (number? result)
           (number? expected)) ; the result is a number, that is we want to predict only 1 value
      (* 0.5 (expt (- result expected) 2))
      (* 0.5 (apply + (map (lambda (x) (* x x))
                           (map - result expected))))))
(define (gmse results expected)
  (if (and (number? result)
           (number? expected))
      (- result expected)
      (map - result expeted)))
