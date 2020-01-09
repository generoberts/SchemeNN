;; add the matrix (list of lists) to the existing layer
;; EX. (let ((l '((1 2) (3 4))))
;;          (add-layer l '((1 0) (0 1)))
;;          ... do something with this 2-layerd structure
(define (add-layer to matrix)
  (set! to (cons matrix to)))
