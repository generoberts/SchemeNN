;;; all about matrix

(define-record-type matrix
  (matrix lst size row col) ; matrix is just a list of lists
  matrix?
  (lst content set-matrix!)
  (size size set-size!) ; (row . col)
  (row row set-row-num!) ; a number of row
  (col col set-col-num!)) ; a number of col


;; make matrix from a list of lists
(define (make-matrix lst)
  (if (and (not (null? lst))
           (list? lst))
      (matrix lst
              (cons (length lst)
                    (matrix-col-num lst))
              (length lst)
              (matrix-col-num lst))
      (raise "Not a matrix")))

;; helper function
;; if it's a vector, this function retrun 1
;; else, it's a matrix (list of lists) so this function
;; return the number of col
(define (matrix-col-num lst)
  (if (list? (car lst))
      (length (car lst))
      1))

;; matrix multiplication
;; https://rosettacode.org/wiki/Matrix_multiplication
(define (matrix-mul m1 m2)
  (when (and (matrix? m1)
             (matrix? m2))
    (map
     (lambda (row)
       (apply map
              (lambda column
                (apply + (map * row column)))
              (content m2)))
     (content m1))))

;; now, binding elementwise and rowwise together for ease of use
(define (matrix-apply func mtrx element?)
  (define (elementwise-apply func mtrx)
    (when (matrix? mtrx)
      (map (lambda (row)
             (map (lambda (element) (func element))
                  row))
           mtrx)))
  (define (rowwise-apply func mtrx)
    (when (matrix? mtrx)
      (map (lambda (row)
             (func row))
           mtrx)))
  (if element?
      (elementwise-apply func mtrx)
      (rowwise-apply func mtrx)))

;; identity square matrix of the given size
(define (identity-matrx size)
  (define (make-list size) ; make a list of length size, filling it with 0
    (if (= 0 size)
        '()
        (cons 0 (make-list (- size 1)))))
  (let ((first-list (make-list size))
        (i 0))
    (map (lambda (zero)
           (begin
             (set! zero
                   (set! (list-ref (make-list size) i)
                         1))
             (set! i (+ i 1)))) first-list)))
