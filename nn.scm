(include "matrix")

;; a layer, contrac to normal methology, contains an input matrix, a weightm mamtrix,
;; activation function, and bias
(define-record-type layer
  (layer inputs weight activation bias)
  layer?
  (weights weight-matrix set-weight-matrix!)
  (activation acti-func set-acti-func!)
  (bias bias-matrix set-bias-matrix!))

;; model contains a list of layers
(define-record-type model
  (model layers)
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
(define (feedforward model-layers input) 
  (define (ff lyres init) ; a list of layers and a init input
    (let ((this-layer (car lyres)))
      (if (null? this-layer)
          0
          (ff (cdr this-layer)
              (matrix-apply (if (null? (cdr lyres)) ; we don't have the next layer
                                (biars-values 0)
                                (bias-value (bias-matrix this-layer)))
                            (matrix-apply (acti-func this-layer)
                                          (matrix-mul (weight-matrix this-layer)
                                                      init)))))))
  (ff (reverse model-layers) input))


