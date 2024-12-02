#lang racket

(provide read-columns read-rows)

; read lines of "a b ..." and produces list of lists
(define/contract (read-columns filename)
  (-> string? (listof (listof string?)))
  (with-input-from-file filename
    (lambda ()
      (for/foldr ([sublists '()]) ([line (in-lines)])
        (define line-parts (regexp-split #px"\\s+" line))
        (if (= (length sublists) 0)
            (map list line-parts)
            (map cons line-parts sublists))))))


; read lines of "a1 a2 ..." and produces list of lists
(define/contract (read-rows filename)
  (-> string? (listof (listof string?)))
  (with-input-from-file filename
    (lambda ()
      (for/foldr ([sublists '()]) ([line (in-lines)])
        (define line-parts (regexp-split #px"\\s+" line))
        (cons line-parts sublists)))))


(module+ test
  (require rackunit)
  
  (define (test/read-columns)
    (define COL1 (list "3." "1" "4" "1" "5"))
    (define COL2 (list "9" "2" "6" "5" "3"))
    (define COL3 (list "5" "8" "9" "7" "9"))
    (check-equal? (read-columns "input-formats/column.txt") (list COL1))
    (check-equal? (read-columns "input-formats/two-column.txt")
                  (list COL1 COL2))
    (check-equal? (read-columns "input-formats/three-column.txt")
                  (list COL1 COL2 COL3)))

  (define (test/read-rows)
    (define ROW1 (list "3." "1" "4" "1" "5"))
    (define ROW2 (list "9" "2" "6" "5" "3"))
    (define ROW3 (list "5" "8" "9" "7" "9"))
    (check-equal? (read-rows "input-formats/row.txt") (list ROW1))
    (check-equal? (read-rows "input-formats/two-row.txt")
                  (list ROW1 ROW2))
    (check-equal? (read-rows "input-formats/three-row.txt")
                  (list ROW1 ROW2 ROW3)))
  
  (test/read-columns)
  (test/read-rows))