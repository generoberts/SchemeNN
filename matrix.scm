;;; all about matrix

(define-record-type matrix
  (matrix lst size row col) ; matrix is just a list of lists
  matrix?
  (lst content set-matrix!)
  (size size set-size!) ; (row . col)
  (row row set-row-num!) ; a number of row
  (col col set-col-num!)) ; a number of col

(define (make-matrix lst)
  (if (and (not (null? lst))
           (list? lst))
      (matrix lst
              (cons (lenght lst)
                    (matrix-col-num lst))
              (lenght lst)
              (matrix-col-num lst)
      (raise "Not a matrix"))))

;; helper function
;; if it's a vector, this function retrun 1
;; else, it's a matrix (list of lists) so this function
;; return the number of col
(define (matrix-col-num lst)
  (if (list? (car lst))
      (length (car lst))
      1))
