(include "matrix")
(include "activation")

;; a layer, contrac to normal methology, contains an input matrix, a weightm mamtrix,
;; activation function, and bias
(define-record-type layer
  (layer weight activation bias)
  layer?
  (weight weight-matrix set-weight-matrix!)
  (activation acti-func set-acti-func!)
  (bias bias-matrix set-bias-matrix!))

;; making layer
;; we use this function to make a layer because we don't want to manually type random
;; matrix for weights-matrix everytime
;; activation is activation function's name
;; bias is the value of the bias (just give a number, the procedures know how to elementwise-plus to matrix/value)
(define (make-layer size activation bias)
  (layer (random-matrix size)
         (make-activation-function activation)
         1))

;; model contains a list of layers
(define-record-type model
  (model input layers)
  model?
  (input input)
  (layers layers))

(define (bias-values b)
  (lambda (x) (+ b x)))

;; assuming that we already define model, thus to use this function we do
;; (feedforward (layers model) input) where input is a single row of input matrix
;; todo: should we take a list of input and let the result
;; of this functio be a list of results?
;; reason: beacuse the cost function takes such list of result
;; along with list of expeted values
(define (feedforward model-layers model-input) 
  (define (ff lyres init) ; a list of layers and a init input
    (let ((this-layer (if (not (null? lyres))
                          (car lyres)
                          '())))
      (if (null? this-layer)
          0
          (ff (cdr lyres)
              (matrix-apply (if (null? (cdr lyres)) ; we don't have the next layer
                                (bias-values 0)
                                (bias-values (bias-matrix this-layer)))
                            (matrix-apply (acti-func this-layer)
                                          (matrix-mul (weight-matrix this-layer)
                                                      init)
                                          #t) ; todo: use #f automatically if it's the vector
                            #t)))))
  (ff (reverse model-layers) model-input))


