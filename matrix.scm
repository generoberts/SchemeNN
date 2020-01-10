;; all about matrix

(define matrix
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
              (cons (lenght (lst))
                    (lenght (car lst)))
              (lenght lst)
              (length (car lst)))
      (raise "Not a matrix")))
