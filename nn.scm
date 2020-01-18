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

;; apply feedforward to the layers of the model
;; Returns a list of (+ (activation-function (* input weight)) bias)
(define (feedforward mdl)
  (define (ff lyers in) ; a list of layers and an input vector
    (if (null? lyers)
        0
        (let ((result (matrix-apply (if (null? (cdr layer)) ; is this correct?
                                        (bias-values 0)
                                        (bias-values (bias-matrix layer)))
                                    (matrix-mul (car lyers) in)))
              (ff (cdr lyers) result)))))
  (ff (layers mdl) (input mdl)))

;; assuming that we already define model, thus to use this function we do
;; (feedforward (layers model) input) where input is a single row of input matrix
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


