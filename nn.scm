(include "matrix" "activation")

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
  (model layers)
  model?
  (layers layers))

(define (bias-values b)
  (lambda (x) (+ b x)))

;; assuming that we already define model, thus to use this function we do
;; (feedforward model)
(define (feedforward mdl inpt)
  (define (calculate this-layer this-init)
    (matrix-apply (bias-values (bias-matrix this-layer))
                  (make-matrix (matrix-apply (function (acti-func this-layer))
                                             (make-matrix (matrix-mul (weight-matrix this-layer)
                                                                      this-init))
                                             #t))
                  #t))
  (define (ff lyres init)
    (let ((this-layer (car lyres)))
      (if (null? (cdr lyres)) ; we don't have the next layer
          (calculate this-layer init)
          (ff (cdr lyres)
              (calculate this-layer init)))))
  (if (model? mdl)
      (let ((l (layers mdl))
            (i inpt))
        (ff (reverse l) i))
      (error "Not a model.")))
