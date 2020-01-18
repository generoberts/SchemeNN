;;; cost functions

(define-record-type cost-function
  cost-function?
  (type type) ; basically a name
  (cost-func cost-func)
  (gradient gradient)) ; gradient of this cost function, its name is the type of cost function with prefix 'g'

;; result is what feedforward gives us
;; expeted is what the training data is
(define (mse result expected)
  (* 0.5 (apply + (map (lambda (x) (* x x))
                       (map - result expeted)))))
(define (gmse results expected)
  (map - result expeted))
