;;; all about matrix

(include "random")

(define-record-type matrix
  (matrix lst size row col) ; matrix is just a list of lists
  matrix?
  (lst matrix-content set-matrix!)
  (size matrix-size set-size!) ; (row . col)
  (row matrix-row set-row-num!) ; a number of row
  (col matrix-col set-col-num!)) ; a number of col


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
              (matrix-content m2)))
     (matrix-content m1))))

;; apply a function element-wise to the matrix
;; if element? is #f, it apply rowwise
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
(define (identity-matrix size)
  (define (identity-matrix-builder size) ; returns a list of lists representing identity matrix. We have to pass that in to make-matrix function
    (define (make-list-helper size index) ; all but the index is 0, the index is 1
      (if (= 0 size)
          '()
          (cons (if (= index (- size 1))
                    1
                    0)
                (make-list-helper (- size 1) index))))
    (define (make-list size index)
      (reverse (make-list-helper size index)))
    (define (make-zeros size) ; a list of 0s
      (if (= 0 size)
          '()
          (cons 0 (make-zeros (- size 1)))))
    (do ((first-list (make-zeros size))
         (i 0 (+ i 1)))
        ((= i size) first-list)
      (set! (list-ref first-list i)
            (make-list size i))))
  (make-matrix (identity-matrix-builder size)))

;; radom matrix is a matrix of size NxN that its all elements
;; are random number from 0 to 1
(define (random-matrix size)
  (define (random-matrix-builder size)
    (define (random-row s)
      (if (= 0 s)
          '()
          (cons (rrandom) (random-row (- s 1)))))
    (do ((result '() (cons (random-row size) result))
         (i 0 (+ i 1)))
        ((= i size) result)))
  (make-matrix (random-matrix-builder size)))
