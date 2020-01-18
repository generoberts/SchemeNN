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

;; apply feedforward to the layers of the model
;; Returns a list of (+ (activation-function (* input weight)) bias)
(define (feed-forward mdl)
  (let ((lyrs (layers mdl)))
    (let ((weight (weight-matrix lyrs))
          (activation))))) ; layers of the model
