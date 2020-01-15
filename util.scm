;;; utilities for reading csv file and saving model in sexp

;; remove all targets from lst
(define (remove target lst)
  (cond
   ((null? lst) '())
   ((eq? target (car lst)) (remove target (cdr lst)))
   (else
    (cons (car lst) (remove target (cdr lst))))))

;; turn a list of  chars to a list of string
(define (chars->strings lst)
  (map (lambda (ch) (string ch)) lst))

;; read csv file line-by-line and return the list of those lines
(define (csv->list-of-list file) ; file is a name of the file
  (call-with-input-file file
    (lambda (l)
      (let f ((result '())
              (line (read-line l)))
        (cond
         ((eof-object? line)
          (reverse result))
         (else
          (f (cons (list line) result) ; use list lien because we want to make the result of the form lists-in-list, and the inner list will act as a line separater
             (read-line l))))))))

(define (read-csv file)
  (let ((the-list (csv->list-of-list file))) ; it's of the form (("1,2") ("3,4"))
    (map (lambda (line) (chars->strings ; turns a list of chars to a list of string
                    (remove #\, ; removes commas from a list of chars
                            (string->list ; turns a string to a list of chars 
                             (car  line))))) ; take the first element of the line, which has only 1 element that is, for example ("1,2") for the first line
         the-list)))
