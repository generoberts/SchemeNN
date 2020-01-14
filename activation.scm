;; https://en.wikipedia.org/wiki/Activation_function
(define activation-functions '(sigmoid logistic softmax identity))

(define (logistic x)
  (/ 1 (+ 1 (exp (- x)))))

;; take a vector (or a list) as input
(define (softmax vect)
  (let ((summation (apply + (map exp vect))))
    (map (lambda (val) (/ (exp val) summation)) vect)))
