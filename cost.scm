;;; cost functions

(define-record-type cost-function
  (cost-function type cost-func gradient)
  cost-function?
  (type type) ; basically a name
  (cost-func cost-func)
  (gradient gradient)) ; gradient of this cost function, its name is the type of cost function with prefix 'g'

;; because we need to slighly difference apply costs function between number and vector
(define (both-are-number? result expected)
  (and (number? result)
       (number? expected)))

;; result is what feedforward gives us
;; expeted is what the training data is
(define (mse result expected)
  (if (both-are-number? result expected)
      (* 0.5 (expt (- result expected) 2))
      (* 0.5 (apply + (map (lambda (x) (* x x))
                           (map - result expected))))))
(define (gmse results expected)
  (if (both-are-number? result expected)
      (- result expected)
      (map - result expeted)))

(define (cross-entropy result expected)
  (if (both-are-number? result expected)
      (- (+ (* expected (log result)) (* (- 1 expected) (log (- 1 result)))))
      (- (apply + (map (lambda (r e) (+ (* e (log r)) (* (- 1 e) (log (- 1 r))))) result expected)))))
(define (gcross-entropy result expected)
  (if (both-are-number? result expected)
      (/ (- result expected) (* (- 1 result) result))
      (map (lambda (r e) (/ (- r e) (* (- 1 r) r))) result expected)))
