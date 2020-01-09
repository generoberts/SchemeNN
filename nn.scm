;; a layer, contrac to normal methology, contains an input matrix, a weightm mamtrix,
;; activation function, and bias
(define-record-type layer
  (layer inputs weight activation bias)
  layer?
  (inputs input-metrix set-input-metrix!)
  (weight weight-matrix set-weight-matrix!)
  (activation acti-func set-acti-func!)
  (bias bias-matrix set-bias-matrix!))

;; model contains a list of layers
(define-record-type model
  (model layers)
  model?
  (layers layers))
